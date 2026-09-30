### compile_ceestock_data.R
###
### Import CEEStock data for single-family DETACHED energy profiles.
### SFA, MF, and manufactured profiles are built downstream in
### compile_building_summaries.R using ResStock data corrected by
### CEEStock-derived scalars.
###
### This script produces:
###   ceestock_summaries   — SFD profiles per strategy (baseline through
###                          full electrification), at vintage x fine sqft
###   electrification_ratios — three fuel-agnostic scalars for constructing
###                            full-electrification profiles from baseline
###                            end-use data (heating retention, appliance
###                            mcf->mwh, heating mcf->mwh)
###
### Dependency chain:
###   this script              ->  ceestock_summaries, electrification_ratios

library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)

ceestock_raw <- readr::read_csv(
  "./data-raw/building_energy_data_processing/ceestock/ceestcok_savings_v1.csv",
  show_col_types = FALSE
) %>%
  janitor::clean_names() %>%
  mutate(
    across(.cols = matches("elec") & !matches("pct"), .fns = ~ .x * 0.2933),
    across(.cols = matches("gas") & !matches("pct"), .fns = ~ .x * 0.963),
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

#' Collapse the three sub-1000 sqft bins
bin_all <- function(x) {
  case_when(
    x %in% c("0-499", "500-749", "750-999") ~ "<1000",
    TRUE ~ str_replace_all(x, "-", " to ")
  )
}


# -- Strategy maps -------------------------------------------------------------

strategy_scenario_filter <- list(
  "Baseline" = "Baseline",
  "Retrofit" = "Only Wx",
  # "Heatpump"               = "Dual Fuel 80% No Wx",
  # "Electric appliances"    = "Dual Fuel 80% No Wx",
  # "Retrofit and heatpump"  = "Dual Fuel 80%",
  "Full electrification" = "Dual Fuel 80% No Wx",
  "Retrofit and full electrification" = "Dual Fuel 80%"
)

strategy_vcols <- list(
  "Baseline" = c("elec_mwh", "gas_heat_mcf", "gas_mcf", "gas_other_mcf"),
  "Retrofit" = c("elec_mwh", "elec_change_mwh", "gas_mcf", "gas_change_mcf"),
  # "Heatpump"               = c("elec_mwh", "elec_change_mwh", "gas_mcf", "gas_change_mcf"),
  # "Electric appliances"    = c("elec_mwh", "elec_change_mwh", "gas_mcf", "gas_change_mcf"),
  # "Retrofit and heatpump"  = c("elec_mwh", "elec_change_mwh", "gas_mcf", "gas_change_mcf"),
  "Full electrification" = c("elec_mwh", "elec_change_mwh", "gas_mcf", "gas_change_mcf"),
  "Retrofit and full electrification" = c("elec_mwh", "elec_change_mwh", "gas_mcf", "gas_change_mcf")
)


# -- Strategy-specific column mutations ----------------------------------------
#
# Each strategy isolates a particular energy change from the raw CEEStock
# scenario. "Full electrification" variants are no-ops: they use the raw
# scenario totals (heat pump + appliance electrification combined).

apply_strategy_mutate <- function(data, strategy) {
  switch(strategy,
    "Baseline" = data,
    "Retrofit" = data,
    # "Heatpump" = data %>% mutate(
    #   gas_other_mcf        = gas_mcf - gas_heat_mcf,
    #   gas_other_change_mcf = gas_change_mcf - gas_heat_change_mcf,
    #   gas_mcf              = gas_mcf - gas_other_change_mcf,
    #   elec_mwh             = elec_mwh - elec_other_change_mwh,
    #   gas_change_mcf       = gas_heat_change_mcf,
    #   elec_change_mwh      = elec_heat_change_mwh + elec_cool_change_mwh
    # ),
    #  "Electric appliances" = data %>% mutate(
    #    gas_other_mcf    = gas_mcf - gas_heat_mcf,
    #    gas_change_mcf   = gas_change_mcf - gas_heat_change_mcf,
    #    gas_mcf          = gas_mcf - gas_heat_change_mcf,
    #    elec_mwh         = elec_mwh - (elec_heat_change_mwh + elec_cool_change_mwh),
    #    elec_change_mwh  = elec_other_change_mwh
    #  ),
    #  "Retrofit and heatpump" = data %>% mutate(
    #    gas_other_mcf        = gas_mcf - gas_heat_mcf,
    #    gas_other_change_mcf = gas_change_mcf - gas_heat_change_mcf,
    #    gas_mcf              = gas_mcf - gas_other_change_mcf,
    #    elec_mwh             = elec_mwh - elec_other_change_mwh,
    #    gas_change_mcf       = gas_heat_change_mcf,
    #    elec_change_mwh      = elec_heat_change_mwh + elec_cool_change_mwh
    #  ),
    "Full electrification" = data,
    "Retrofit and full electrification" = data
  )
}


# -- SFD profile builder -------------------------------------------------------
#
# Builds a single-family DETACHED profile for one strategy.
# Filters to Natural Gas, applies strategy mutations, groups by
# vintage x fine sqft bin.

build_strategy_sfd <- function(raw, strategy_label) {
  vcols <- strategy_vcols[[strategy_label]]
  raw_scenario <- strategy_scenario_filter[[strategy_label]]

  raw %>%
    filter(
      model_heating_fuel == "Natural Gas",
      mc_classification == "single_family_detached",
      scenario == raw_scenario,
      n >= 5
    ) %>%
    mutate(
      scenario = strategy_label,
      sqft_bin = bin_all(model_geometry_floor_area),
      model_vintage_acs = if_else(
        model_vintage_acs %in% c("2000-09", "2010s"), "2000+", model_vintage_acs
      )
    ) %>%
    apply_strategy_mutate(strategy_label) %>%
    group_by(scenario, build_year = model_vintage_acs, sqft_bin, mc_classification) %>%
    summarise(across(all_of(vcols), mean), .groups = "drop")
}


# -- Build all SFD strategies --------------------------------------------------

cee_baseline_sfd <- build_strategy_sfd(ceestock_raw, "Baseline")
cee_retrofit_sfd <- build_strategy_sfd(ceestock_raw, "Retrofit")
# cee_heatpump_sfd        <- build_strategy_sfd(ceestock_raw, "Heatpump")
# cee_appliance_sfd       <- build_strategy_sfd(ceestock_raw, "Electric appliances")
# cee_combined_sfd        <- build_strategy_sfd(ceestock_raw, "Retrofit and heatpump")
cee_full_elec_sfd <- build_strategy_sfd(ceestock_raw, "Full electrification")
cee_retrofit_full_elec_sfd <- build_strategy_sfd(ceestock_raw, "Retrofit and full electrification")


# ==============================================================================
# Electrification transfer ratios
# ==============================================================================
#
# Three fuel-agnostic scalars derived from CEEStock SFD "Dual Fuel 80% No Wx"
# scenario vs Baseline. Used downstream to construct full-electrification
# profiles for housing types without direct CEEStock coverage.
#
# Ratios are in MWh per mmBtu, so they apply to both natural gas and propane:
#   - Natural gas: MCF * 1.037 = mmBtu
#   - Propane: already in mmBtu in our pipeline
#
# A single n-weighted mean is the most defensible summary. Vintage variation
# is printed for QC but not exported (see earlier analysis).

compute_electrification_ratios <- function(raw) {
  sfd_natgas <- raw %>%
    filter(
      model_heating_fuel == "Natural Gas",
      mc_classification == "single_family_detached",
      scenario %in% c("Baseline", "Dual Fuel 80% No Wx"),
      n >= 5
    ) %>%
    mutate(
      build_year = if_else(
        model_vintage_acs %in% c("2000-09", "2010s"), "2000+", model_vintage_acs
      ),
      sqft_fine = bin_all(model_geometry_floor_area)
    )

  bl <- sfd_natgas %>%
    filter(scenario == "Baseline") %>%
    group_by(build_year, sqft_fine) %>%
    summarise(
      bl_gas_heat_mcf = mean(gas_heat_mcf),
      bl_gas_total_mcf = mean(gas_mcf),
      bl_n = sum(n), .groups = "drop"
    ) %>%
    mutate(bl_gas_app_mcf = bl_gas_total_mcf - bl_gas_heat_mcf)

  sc <- sfd_natgas %>%
    filter(scenario == "Dual Fuel 80% No Wx") %>%
    group_by(build_year, sqft_fine) %>%
    summarise(
      sc_gas_heat_mcf = mean(gas_heat_mcf),
      sc_gas_total_mcf = mean(gas_mcf),
      sc_elec_heat_chg_mwh = mean(elec_heat_change_mwh),
      sc_elec_cool_chg_mwh = mean(elec_cool_change_mwh),
      sc_elec_other_chg_mwh = mean(elec_other_change_mwh),
      sc_n = sum(n), .groups = "drop"
    ) %>%
    mutate(sc_gas_app_mcf = sc_gas_total_mcf - sc_gas_heat_mcf)

  paired <- inner_join(bl, sc, by = c("build_year", "sqft_fine")) %>%
    mutate(
      pair_n = pmin(bl_n, sc_n),
      heating_retention_frac = sc_gas_heat_mcf / bl_gas_heat_mcf,
      gas_app_elim_mmbtu = (bl_gas_app_mcf - sc_gas_app_mcf) * 1.037,
      appliance_mwh_per_mmbtu = sc_elec_other_chg_mwh / gas_app_elim_mmbtu,
      gas_heat_elim_mmbtu = (bl_gas_heat_mcf - sc_gas_heat_mcf) * 1.037,
      heating_mwh_per_mmbtu = (sc_elec_heat_chg_mwh + sc_elec_cool_chg_mwh) / gas_heat_elim_mmbtu
    )

  # QC: vintage breakdown
  vintage_qc <- paired %>%
    group_by(build_year) %>%
    summarise(
      heating_retention = weighted.mean(heating_retention_frac, pair_n),
      app_mwh_per_mmbtu = weighted.mean(appliance_mwh_per_mmbtu, pair_n),
      heat_mwh_per_mmbtu = weighted.mean(heating_mwh_per_mmbtu, pair_n),
      n = sum(pair_n), .groups = "drop"
    )
  message("\n-- Electrification ratios by vintage (QC only) --")
  print(vintage_qc, n = Inf)

  # Overall (exported)
  tibble(
    heating_retention_frac  = weighted.mean(paired$heating_retention_frac, paired$pair_n),
    appliance_mwh_per_mmbtu = weighted.mean(paired$appliance_mwh_per_mmbtu, paired$pair_n),
    heating_mwh_per_mmbtu   = weighted.mean(paired$heating_mwh_per_mmbtu, paired$pair_n),
    total_n                 = sum(paired$pair_n)
  )
}

electrification_ratios <- compute_electrification_ratios(ceestock_raw)

message("\n-- Electrification ratios (exported) --")
message(sprintf(
  "  Heating retention:   %.1f%%",
  electrification_ratios$heating_retention_frac * 100
))
message(sprintf(
  "  Appliance MWh/mmBtu: %.4f (implied COP %.1f)",
  electrification_ratios$appliance_mwh_per_mmbtu,
  0.2933 / electrification_ratios$appliance_mwh_per_mmbtu
))
message(sprintf(
  "  Heating MWh/mmBtu:   %.4f (implied COP %.1f)",
  electrification_ratios$heating_mwh_per_mmbtu,
  0.2933 / electrification_ratios$heating_mwh_per_mmbtu
))


# -- Package and save ----------------------------------------------------------

ceestock_summaries <- list(
  cee_baseline_sfd = cee_baseline_sfd,
  cee_retrofit_sfd = cee_retrofit_sfd,
  # cee_heatpump_sfd = cee_heatpump_sfd,
  # cee_appliance_sfd = cee_appliance_sfd,
  # cee_combined_sfd = cee_combined_sfd,
  cee_full_elec_sfd = cee_full_elec_sfd,
  cee_retrofit_full_elec_sfd = cee_retrofit_full_elec_sfd
)

# QC: coverage summary
message("\n-- SFD profile coverage --")
purrr::iwalk(ceestock_summaries, ~ message(sprintf(
  "  %-30s  rows=%d  vintages=%d  sqft_bins=%d",
  .y, nrow(.x), n_distinct(.x$build_year), n_distinct(.x$sqft_bin)
)))

usethis::use_data(ceestock_summaries, overwrite = TRUE)
usethis::use_data(electrification_ratios, overwrite = TRUE)
