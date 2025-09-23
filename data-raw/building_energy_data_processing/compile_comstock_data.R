### import ComStock data for electrification and retrofit elasticities

library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# helper to bin year in data -- for commercial buildings, we're just looking for 2010s, but labeling all for now in case we want to do deeper analysis later
bin_year <- function(year) {
  cut(year,
      breaks = c(0, 1939, 1959, 1979, 1999, 2009, 2025.1),
      labels = c(
        "<1940", "1940-59", "1960-79", "1980-99",
        "2000-09", "2010s"
      ),
      right = TRUE
  )
}

# function to load in comstock files
load_comstock <- function(path) {
  readr::read_csv(path) %>%
    janitor::clean_names() %>%
    select(
      sq_ft = in_sqft_ft2,
      build_year = in_year_built,
      building_subtype = in_building_subtype,
      comstock_building_type = in_comstock_building_type,
      comstock_building_type_group = in_comstock_building_type_group,
      heating_fuel = in_heating_fuel,
      hvac_heating_type = in_hvac_heat_type,
      building_kwh = out_electricity_net_energy_consumption_kwh,
      building_nat_gas_kwh = out_natural_gas_total_energy_consumption_kwh,
      building_kwh_savings = out_electricity_net_energy_savings_kwh,
      building_nat_gas_kwh_savings = out_natural_gas_total_energy_savings_kwh
    ) %>%
    mutate(
      year_bin = as.character(bin_year(build_year)),
      building_mcf = building_nat_gas_kwh * 0.00329026,
      building_mcf_savings = building_nat_gas_kwh_savings * 0.00329026,
      mc_classification = "jobs"
    )
}

## summarize function
summarize_comstock <- function(df) {
  df %>%
    summarize(
      median_kwh = median(building_kwh, na.rm = TRUE),
      median_mcf = median(building_mcf, na.rm = TRUE),
      mean_kwh_savings_nonzero = mean(building_kwh_savings[building_kwh_savings != 0], na.rm = TRUE),
      mean_mcf_savings_nonzero = mean(building_mcf_savings[building_mcf_savings != 0], na.rm = TRUE),
      .groups = "drop"
    )
}


# Manually performed datasurgery on first column name (e.g., ) by renaming to simply bldg_id
# Addresses error:
#  Error in vroom_(file, delim = delim %||% col_types$delim, col_names = col_names, : embedded nul in string: 'MN_upgrade0_agg.csv\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\00000644\00000000\00000000\000236171124\000000000000\0011422\0 0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0ustar\000\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0bldg_id'

# "0": "Baseline"
baseline <- load_comstock(
  "./data-raw/building_energy_data_processing/comstock_data/MN_upgrade0_agg.csv")

# "50": "Package 1, Wall + Roof Insulation + New Windows",
retrofit_efficiency <- load_comstock(
  "./data-raw/building_energy_data_processing/comstock_data/MN_upgrade50_agg.csv")

# "10": "Cold Climate Challenge HP RTU, Electric Backup",
electrification <- load_comstock(
  "./data-raw/building_energy_data_processing/comstock_data/MN_upgrade10_agg.csv")

# Limit to 2010s
load_comstock_new <- function(path) {
  load_comstock(path) %>% filter(year_bin == "2010s")
}

# Load filtered data (same files, just filtered)
baseline_newBuild <- load_comstock_new("./data-raw/building_energy_data_processing/comstock_data/MN_upgrade0_agg.csv")
retrofit_efficiency_newBuild <- load_comstock_new("./data-raw/building_energy_data_processing/comstock_data/MN_upgrade50_agg.csv")
electrification_newBuild <- load_comstock_new("./data-raw/building_energy_data_processing/comstock_data/MN_upgrade10_agg.csv")

# Summaries (reuse summarize_comstock)
comstock_summaries <- list(
  baseline                         = summarize_comstock(baseline),
  retrofit_efficiency              = summarize_comstock(retrofit_efficiency),
  electrification                  = summarize_comstock(electrification)
  # baseline_newBuild                = summarize_comstock(baseline_newBuild),
  # retrofit_efficiency_newBuild     = summarize_comstock(retrofit_efficiency_newBuild),
  # electrification_newBuild         = summarize_comstock(electrification_newBuild)
)

baseline_tbl <- ghg.ccap::imagine_commDesgn_mwh_mcf_perJob_coefficients

# Pull medians for the two reference baselines (full + newBuild)
baseline_mcf     <- comstock_summaries$baseline$median_mcf
baseline_kwh     <- comstock_summaries$baseline$median_kwh
baseline_new_mcf <- comstock_summaries$baseline_newBuild$median_mcf
baseline_new_kwh <- comstock_summaries$baseline_newBuild$median_kwh

# Look up the scenario’s medians, picks the correct reference, computes ratios, and scales baseline_tbl
scale_scenario <- function(scenario_key) {
  mv <- comstock_summaries[[scenario_key]]

  # Use the 2010s baseline when the scenario is *_newBuild, else full baseline
  use_newbuild_ref <- grepl("_newBuild$", scenario_key)
  ref_mcf <- if (use_newbuild_ref) baseline_new_mcf else baseline_mcf
  ref_kwh <- if (use_newbuild_ref) baseline_new_kwh else baseline_kwh

  mcf_ratio <- mv$median_mcf / ref_mcf
  mwh_ratio <- mv$median_kwh / ref_kwh

  baseline_tbl %>%
    mutate(
      mcf_per_job = mcf_per_job * mcf_ratio,
      mwh_per_job = mwh_per_job * mwh_ratio,
      scenario    = scenario_key
    )
}

# Scenarios to emit -- new build logic is faulty given data availability.
scenarios <- c(
  "baseline",
  "retrofit_efficiency",
  "electrification"
  # "baseline_newBuild",
  # "retrofit_efficiency_newBuild",
  # "electrification_newBuild"
)

# Build the final table -- first, take baseline, retrofit, and heatpump
combined_scenarios <-
  map_dfr(scenarios, scale_scenario) %>%
  select(scenario, imagine_designation, mcf_per_job, mwh_per_job)

# Add a placeholder for new builds... use the baseline data as an analog. New build analysis was inconclusive
imagine_commDesgn_mwh_mcf_perJob_perScenario_coefficients <- combined_scenarios %>%
  bind_rows(
    combined_scenarios %>%
      filter(scenario == "baseline") %>%
      mutate(scenario = "new_build")
  )

usethis::use_data(comstock_summaries, overwrite = TRUE)
usethis::use_data(imagine_commDesgn_mwh_mcf_perJob_perScenario_coefficients, overwrite = TRUE)
