### import CEEStock data for electrification and retrofit elasticities
### single_family_attached is sparsely populated and will use coarser sq_ft bins
### and imputations anchored to single_family_detached to fill in

# library(dplyr, tidyr, readr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

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

build_strategy_sf <- function(raw, strategy_label, vcols) {
  raw_scenario <- strategy_scenario_filter[[strategy_label]]

  prepped <- raw %>%
    filter(
      model_heating_fuel == "Natural Gas",
      scenario == raw_scenario
    ) %>%
    mutate(
      scenario  = strategy_label,
      sqft_fine = bin_all(model_geometry_floor_area),
      sqft_att  = bin_to_att(sqft_fine)
    ) %>%
    apply_strategy_mutate(strategy_label)

  # detached — fine bins for their use
  det_sf <- prepped %>%
    filter(mc_classification == "single_family_detached") %>%
    group_by(scenario, build_year = model_vintage_acs, sqft_bin = sqft_fine, mc_classification) %>%
    summarise(across(all_of(vcols), mean), observed = TRUE, .groups = "drop")

  # detached collapsed to attached bins for ratio scaffolding
  det_collapsed <- prepped %>%
    filter(mc_classification == "single_family_detached") %>%
    group_by(build_year = model_vintage_acs, sqft_bin = sqft_att) %>%
    summarise(across(all_of(vcols), mean, .names = "{.col}_det"), .groups = "drop")

  # attached — observed, coarse bins
  att_obs <- prepped %>%
    filter(mc_classification == "single_family_attached") %>%
    group_by(build_year = model_vintage_acs, sqft_bin = sqft_att) %>%
    summarise(across(all_of(vcols), mean), .groups = "drop")

  #  Per-bin median att/det ratios
  bin_ratios <- att_obs %>%
    inner_join(det_collapsed, by = c("build_year", "sqft_bin")) %>%
    mutate(across(
      all_of(vcols),
      ~ .x / get(paste0(cur_column(), "_det")),
      .names = "ratio_{.col}"
    )) %>%
    group_by(sqft_bin) %>%
    summarise(across(starts_with("ratio_"), median, na.rm = TRUE), .groups = "drop")

  # full grid with imputation
  all_vintages <- unique(prepped$model_vintage_acs)

  att_full <- tidyr::expand_grid(
    build_year = all_vintages,
    sqft_bin   = BINS_ATT
  ) %>%
    left_join(att_obs, by = c("build_year", "sqft_bin")) %>%
    left_join(det_collapsed, by = c("build_year", "sqft_bin")) %>%
    left_join(bin_ratios, by = "sqft_bin") %>%
    mutate(
      observed = !is.na(elec_mwh),
      # fill missing with det × ratio
      across(
        all_of(vcols),
        ~ coalesce(.x, get(paste0(cur_column(), "_det")) * get(paste0("ratio_", cur_column())))
      ),
      scenario = strategy_label,
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
