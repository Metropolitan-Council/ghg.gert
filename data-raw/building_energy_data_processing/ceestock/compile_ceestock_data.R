### import CEEStock data for electrification and retrofit elasticities
### single_family_attached is sparsely populated and will use coarser sq_ft bins
### and imputations anchored to single_family_detached to fill in

# library(dplyr, tidyr, readr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)

ceestock_raw <- readr::read_csv(
  "./data-raw/building_energy_data_processing/ceestock/ceestcok_savings_v1.csv"
) %>%
  janitor::clean_names() %>%
  mutate(
    # unit conversions
    across(.cols = matches("elec") & !matches("pct"), .fns = ~ .x * 0.2933),
    across(.cols = matches("gas") & !matches("pct"), .fns = ~ .x * 0.963),
    # derived column
    gas_other_mm_btu = gas_mm_btu - gas_heat_mm_btu
  ) %>%
  rename_with(~ str_replace(.x, "mm_btu", "mwh"), matches("elec") & !matches("pct")) %>%
  rename_with(~ str_replace(.x, "mm_btu", "mcf"), matches("gas") & !matches("pct")) %>%
  rename_with(~ str_replace(.x, "savings", "change")) %>%
  mutate(
    mc_classification = if_else(
      model_geometry_building_type_acs == "Single-Family Detached",
      "single_family_detached",
      "single_family_attached"
    )
  )

#' Collapse the three sub-1000 bins for ALL housing types
bin_all <- function(x) {
  case_when(
    x %in% c("0-499", "500-749", "750-999") ~ "<1000",
    TRUE ~ str_replace_all(x, "-", " to ")
  )
}

#' Further collapse fine bins into coarse bins for sf_attached
bin_to_att <- function(x) {
  # input is already processed by bin_all()
  case_when(
    x %in% c("1000 to 1499", "1500 to 1999") ~ "1000 to 1999",
    x %in% c("2000 to 2499", "2500 to 2999") ~ "2000 to 2999",
    x %in% c("3000 to 3999", "4000+") ~ "3000+",
    TRUE ~ x # <1000 passes through
  )
}

BINS_ATT <- c("<1000", "1000 to 1999", "2000 to 2999", "3000+")

### low n in ceestock is causing spikes in certain sqft/age combos of sfa
### going to use resstock to smooth these

#' Map ResStock sqft factor labels → coarse attached bins
bin_to_att_resstock <- function(x) {
  case_when(
    x == "Less than 1,000"                        ~ "<1000",
    x %in% c("1,000 to 1,499", "1,500 to 1,999") ~ "1000 to 1999",
    x %in% c("2,000 to 2,499", "2,500 to 2,999") ~ "2000 to 2999",
    x == "3,000 or more"                          ~ "3000+",
    TRUE ~ NA_character_
  )
}


#' Compute smoothed att/det energy ratios from a ResStock dataset.
#' Returns build_year × sqft_bin with ratio_kwh and ratio_mcf,
#' capped at `cap` to enforce the building-science constraint.
resstock_att_det_ratios <- function(strategy_key, cap = 0.97) {
  att_key <- paste0("sf_attached_vintagesqft_", strategy_key)
  det_key <- paste0("sf_detached_vintagesqft_", strategy_key)

  att <- resstock_summaries[[att_key]] %>%
    mutate(sqft_bin = bin_to_att_resstock(as.character(sqft_bin))) %>%
    filter(!is.na(sqft_bin)) %>%
    group_by(build_year, sqft_bin) %>%
    summarise(
      att_kwh = median(median_kwh, na.rm = TRUE),
      att_mcf = median(median_mcf, na.rm = TRUE),
      .groups = "drop"
    )

  det <- resstock_summaries[[det_key]] %>%
    mutate(sqft_bin = bin_to_att_resstock(as.character(sqft_bin))) %>%
    filter(!is.na(sqft_bin)) %>%
    group_by(build_year, sqft_bin) %>%
    summarise(
      det_kwh = median(median_kwh, na.rm = TRUE),
      det_mcf = median(median_mcf, na.rm = TRUE),
      .groups = "drop"
    )

  inner_join(att, det, by = c("build_year", "sqft_bin")) %>%
    transmute(
      build_year, sqft_bin,
      ratio_kwh = pmin(att_kwh / det_kwh, cap),
      ratio_mcf = pmin(att_mcf / det_mcf, cap)
    )
}

# ── Strategy → ResStock dataset mapping ────────────────────────────────────────
strategy_resstock_keys <- list(
  "Baseline"              = "baseline",
  "Retrofit"              = "envelope",
  "Heatpump"              = "heatpump",
  "Electric appliances"   = "heatpump",
  "Retrofit and heatpump" = "combo"
)
# function to extract strategies as needed, e.g. split gas to heating and other/appliances

apply_strategy_mutate <- function(data, strategy) {
  switch(strategy,
    "Baseline" = data, # no additional mutation needed
    "Retrofit" = data, # savings columns already correct for Only Wx
    "Heatpump" = data %>% mutate(
      gas_other_mcf        = gas_mcf - gas_heat_mcf,
      gas_other_change_mcf = gas_change_mcf - gas_heat_change_mcf,
      # remove appliance effects from baseline totals
      gas_mcf              = gas_mcf - gas_other_change_mcf,
      elec_mwh             = elec_mwh - elec_other_change_mwh,
      # isolate heating change only
      gas_change_mcf       = gas_heat_change_mcf,
      elec_change_mwh      = elec_heat_change_mwh + elec_cool_change_mwh
    ),
    "Electric appliances" = data %>% mutate(
      gas_other_mcf    = gas_mcf - gas_heat_mcf,
      # remove heating change from totals
      gas_change_mcf   = gas_change_mcf - gas_heat_change_mcf,
      gas_mcf          = gas_mcf - gas_heat_change_mcf,
      elec_mwh         = elec_mwh - (elec_heat_change_mwh + elec_cool_change_mwh),
      elec_change_mwh  = elec_other_change_mwh
    ),
    "Retrofit and heatpump" = data %>% mutate(
      gas_other_mcf        = gas_mcf - gas_heat_mcf,
      gas_other_change_mcf = gas_change_mcf - gas_heat_change_mcf,
      gas_mcf              = gas_mcf - gas_other_change_mcf,
      elec_mwh             = elec_mwh - elec_other_change_mwh,
      gas_change_mcf       = gas_heat_change_mcf,
      elec_change_mwh      = elec_heat_change_mwh + elec_cool_change_mwh
    )
  )
}

# helper lists ####

# output values per strategy (currently only using elec_mwh)
strategy_vcols <- list(
  "Baseline" = c("elec_mwh", "gas_heat_mcf", "gas_mcf", "gas_other_mcf"),
  "Retrofit" = c("elec_mwh", "elec_change_mwh", "gas_mcf", "gas_change_mcf"),
  "Heatpump" = c("elec_mwh", "elec_change_mwh", "gas_mcf", "gas_change_mcf"),
  "Electric appliances" = c("elec_mwh", "elec_change_mwh", "gas_mcf", "gas_change_mcf"),
  "Retrofit and heatpump" = c("elec_mwh", "elec_change_mwh", "gas_mcf", "gas_change_mcf")
)

# CEEStock scenario labels

strategy_scenario_filter <- list(
  "Baseline"              = "Baseline",
  "Retrofit"              = "Only Wx",
  "Heatpump"              = "Dual Fuel 80% No Wx",
  "Electric appliances"   = "Dual Fuel 80% No Wx",
  "Retrofit and heatpump" = "Dual Fuel 80%"
)

# strategy builder func

build_strategy_sf <- function(raw, strategy_label, vcols,
                              rs_key           = strategy_resstock_keys[[strategy_label]],
                              plausibility_cap = 0.97) {

  raw_scenario <- strategy_scenario_filter[[strategy_label]]

  prepped <- raw %>%
    filter(
      model_heating_fuel == "Natural Gas",
      scenario           == raw_scenario
    ) %>%
    mutate(
      scenario  = strategy_label,
      sqft_fine = bin_all(model_geometry_floor_area),
      sqft_att  = bin_to_att(sqft_fine),
      model_vintage_acs = if_else(model_vintage_acs %in% c("2000-09", "2010s"), "2000+", model_vintage_acs) # coarser new build bin
    ) %>%
    apply_strategy_mutate(strategy_label) %>%
    filter(!n < 5)

  # Detached: fine bins, kept as-is
  det_sf <- prepped %>%
    filter(mc_classification == "single_family_detached") %>%
    group_by(scenario, build_year = model_vintage_acs, sqft_bin = sqft_fine, mc_classification) %>%
    summarise(across(all_of(vcols), mean), observed = TRUE, .groups = "drop")

  # Detached collapsed to coarse attached bins — the anchor for imputation
  det_collapsed <- prepped %>%
    filter(mc_classification == "single_family_detached") %>%
    group_by(build_year = model_vintage_acs, sqft_bin = sqft_att) %>%
    summarise(across(all_of(vcols), mean, .names = "{.col}_det"), .groups = "drop")

  # Attached observed means at coarse bins
  att_obs <- prepped %>%
    filter(mc_classification == "single_family_attached") %>%
    group_by(build_year = model_vintage_acs, sqft_bin = sqft_att) %>%
    summarise(across(all_of(vcols), mean), .groups = "drop")

  # ResStock-derived att/det ratios at vintage x sqft_bin level,
  # with a bin-level median fallback for any missing vintage x bin combos
  rs_vintaged     <- resstock_att_det_ratios(rs_key, cap = plausibility_cap)
  rs_bin_fallback <- rs_vintaged %>%
    group_by(sqft_bin) %>%
    summarise(
      ratio_kwh = median(ratio_kwh, na.rm = TRUE),
      ratio_mcf = median(ratio_mcf, na.rm = TRUE),
      .groups = "drop"
    )

  # Map each output column to its ratio (elec -> ratio_kwh, gas -> ratio_mcf)
  vcol_ratio_key <- setNames(
    if_else(str_detect(vcols, "elec"), "ratio_kwh", "ratio_mcf"),
    vcols
  )

  # Full vintage x bin grid: join all pieces
  all_vintages <- unique(prepped$model_vintage_acs)

  att_full <- tidyr::expand_grid(
    build_year = all_vintages,
    sqft_bin   = BINS_ATT
  ) %>%
    left_join(att_obs,        by = c("build_year", "sqft_bin")) %>%
    left_join(det_collapsed,  by = c("build_year", "sqft_bin")) %>%
    left_join(rs_vintaged,    by = c("build_year", "sqft_bin")) %>%
    left_join(
      rs_bin_fallback %>% rename(ratio_kwh_bin = ratio_kwh, ratio_mcf_bin = ratio_mcf),
      by = "sqft_bin"
    ) %>%
    mutate(
      # Use vintage-level ratio where available, fall back to bin-level median
      ratio_kwh = coalesce(ratio_kwh, ratio_kwh_bin),
      ratio_mcf = coalesce(ratio_mcf, ratio_mcf_bin),

      # A CEEStock observed cell is kept only if it passes the physical plausibility
      # check: attached elec use must be below det * cap. Cells that fail (spikes
      # from low n) are replaced with det * ResStock ratio.
      has_obs   = !is.na(elec_mwh),
      plausible = has_obs & (elec_mwh <= coalesce(elec_mwh_det, Inf) * plausibility_cap),

      across(
        all_of(vcols),
        ~ {
          det_v    <- get(paste0(cur_column(), "_det"))
          ratio_v  <- get(vcol_ratio_key[[cur_column()]])
          smoothed <- det_v * ratio_v
          if_else(plausible, .x, smoothed)
        }
      ),

      observed          = plausible,
      scenario          = strategy_label,
      mc_classification = "single_family_attached"
    ) %>%
    select(scenario, build_year, sqft_bin, mc_classification, all_of(vcols), observed)

  bind_rows(det_sf, att_full) %>%
    arrange(mc_classification, build_year, sqft_bin)
}

# build all strategy tables

cee_baseline_sf <- build_strategy_sf(
  ceestock_raw, "Baseline", strategy_vcols[["Baseline"]]
)

cee_retrofit_sf <- build_strategy_sf(
  ceestock_raw, "Retrofit", strategy_vcols[["Retrofit"]]
)

cee_heatpump_sf <- build_strategy_sf(
  ceestock_raw, "Heatpump", strategy_vcols[["Heatpump"]]
)

cee_appliance_sf <- build_strategy_sf(
  ceestock_raw, "Electric appliances", strategy_vcols[["Electric appliances"]]
)

cee_combined_sf <- build_strategy_sf(
  ceestock_raw, "Retrofit and heatpump", strategy_vcols[["Retrofit and heatpump"]]
)


plot_cee_baseline <- function(data, strategy_label = "Baseline") {

  # sqft bin order differs between sfa (coarse) and sfd (fine)
  sqft_levels <- c(
    "<1000",
    "1000 to 1499", "1000 to 1999",
    "1500 to 1999",
    "2000 to 2499", "2000 to 2999",
    "2500 to 2999",
    "3000 to 3999", "3000+",
    "4000+"
  )

  vintage_levels <- c("<1940", "1940-59", "1960-79", "1980-99", "2000-09", "2010s")

  plot_data <- data %>%
    filter(scenario == strategy_label) %>%
    mutate(
      sqft_bin   = factor(sqft_bin, levels = sqft_levels),
      build_year = factor(build_year, levels = vintage_levels),
      housing    = if_else(
        mc_classification == "single_family_attached",
        "SF Attached", "SF Detached"
      )
    ) %>%
    # pivot to long for faceting by fuel
    pivot_longer(
      cols      = c(elec_mwh, gas_mcf),
      names_to  = "fuel",
      values_to = "value"
    ) %>%
    mutate(
      fuel = case_match(fuel,
                        "elec_mwh" ~ "Electricity (MWh)",
                        "gas_mcf"  ~ "Natural Gas (MCF)"
      )
    )

  ggplot(plot_data, aes(x = sqft_bin, y = value, color = housing, shape = observed)) +
    geom_point(size = 2.5, alpha = 0.85) +
    geom_line(aes(group = housing), linewidth = 0.6, alpha = 0.6) +
    facet_grid(fuel ~ build_year, scales = "free_y") +
    scale_shape_manual(
      values = c("TRUE" = 16, "FALSE" = 1),
      labels = c("TRUE" = "Observed", "FALSE" = "Imputed")
    ) +
    scale_color_manual(values = c("SF Attached" = "#E07B39", "SF Detached" = "#3B7DBF")) +
    labs(
      title   = paste("CEEStock Energy Use by Building Type —", strategy_label),
      x       = "Square Footage Bin",
      y       = NULL,
      color   = NULL,
      shape   = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x     = element_text(angle = 40, hjust = 1, size = 8),
      strip.text.x    = element_text(face = "bold"),
      strip.text.y    = element_text(face = "bold", angle = 0),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

# usage
plot_cee_baseline(cee_baseline_sf)

# or any other strategy
plot_cee_baseline(cee_heatpump_sf, "Heatpump")

#  Coverage summary

purrr::iwalk(
  list(
    baseline = cee_baseline_sf,  retrofit = cee_retrofit_sf,
    heatpump = cee_heatpump_sf,  appliance = cee_appliance_sf,
    combined = cee_combined_sf
  ),
  ~ message(sprintf(
    "%-10s  det=%d  att observed=%d  att imputed=%d",
    .y,
    sum(.x$mc_classification == "single_family_detached"),
    sum(.x$mc_classification == "single_family_attached" & .x$observed),
    sum(.x$mc_classification == "single_family_attached" & !.x$observed)
  ))
)


# package and save

ceestock_summaries <- list(
  cee_baseline_sf  = cee_baseline_sf,
  cee_heatpump_sf  = cee_heatpump_sf,
  cee_retrofit_sf  = cee_retrofit_sf,
  cee_appliance_sf = cee_appliance_sf,
  cee_combined_sf  = cee_combined_sf
)


usethis::use_data(ceestock_summaries, overwrite = TRUE)
