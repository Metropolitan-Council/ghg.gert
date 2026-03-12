### import ComStock data for electrification and retrofit elasticities

library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# helper to bin year in data -- for commercial buildings, we're just looking for 2010s, but labeling all for now in case we want to do deeper analysis later
bin_year <- function(year) {
  cut(year,
    breaks = c(0, 1939, 1959, 1979, 1999, 2025.1),
    labels = c(
      "<1940", "1940-59", "1960-79", "1980-99",
      "2000+"
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
summarize_comstock <- function(df, new_build = FALSE) {
  base_df <- df %>%
    filter(heating_fuel == "NaturalGas") %>%
    mutate(kwh_per_sqft = building_kwh / sq_ft,
           mcf_per_sqft = building_mcf / sq_ft,
           kwh_savings_sqft = building_kwh_savings / sq_ft,
           mcf_savings_sqft = building_mcf_savings / sq_ft)

  summarize_groups <- function(data) {
    data %>%
      summarize(
        n = n(),
        mean_kwh_sqft = mean(kwh_per_sqft, na.rm = TRUE),
        mean_mcf_sqft = mean(mcf_per_sqft, na.rm = TRUE),
        mean_kwh_savings = mean(kwh_savings_sqft, na.rm = TRUE),
        mean_mcf_savings = mean(mcf_savings_sqft, na.rm = TRUE),
        .groups = "drop"
      )
  }

  if(new_build == TRUE) {
    base_df %>% group_by(year_bin) %>% summarize_groups() %>%
    filter(year_bin == "2000+") } else {
    base_df  %>% summarize_groups() %>% mutate(year_bin = "Total")
    }


}


# Manually performed datasurgery on first column name (e.g., ) by renaming to simply bldg_id
# Addresses error:
#  Error in vroom_(file, delim = delim %||% col_types$delim, col_names = col_names, : embedded nul in string: 'MN_upgrade0_agg.csv\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\00000644\00000000\00000000\000236171124\000000000000\0011422\0 0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0ustar\000\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0bldg_id'


# "0": "Baseline"
baseline <- load_comstock(
  "./data-raw/building_energy_data_processing/comstock_data/MN_upgrade0_agg.csv"
)

# "54": "Package 1, Wall + Roof Insulation + New Windows",
retrofit_efficiency <- load_comstock(
  "./data-raw/building_energy_data_processing/comstock_data/MN_upgrade54_agg.csv"
)

# 58: "Package 5, Variable Speed HP RTU or HP Boilers + Economizer + DCV + Energy Recovery",
electrification <- load_comstock(
  "./data-raw/building_energy_data_processing/comstock_data/MN_upgrade58_agg.csv"
)

# 64: Package 11: Wall + Roof Insulation + New Windows + LED Lighting + Hydronic GHP or Packaged GHP or Console GHP

high_efficient <- load_comstock(
  "./data-raw/building_energy_data_processing/comstock_data/MN_upgrade64_agg.csv"
)

# Load filtered data (same files, just filtered)
# baseline_newBuild <- load_comstock_new("./data-raw/building_energy_data_processing/comstock_data/MN_upgrade0_agg.csv")
# retrofit_efficiency_newBuild <- load_comstock_new("./data-raw/building_energy_data_processing/comstock_data/MN_upgrade50_agg.csv")
# electrification_newBuild <- load_comstock_new("./data-raw/building_energy_data_processing/comstock_data/MN_upgrade10_agg.csv")

# Summaries (reuse summarize_comstock)
comstock_summaries <- list(
  baseline                         = summarize_comstock(baseline, new_build = FALSE),
  new_build                         = summarize_comstock(baseline, new_build = TRUE),
  retrofit_efficiency              = summarize_comstock(retrofit_efficiency, new_build = FALSE),
  electrification                  = summarize_comstock(electrification, new_build = FALSE),
  new_build_efficient                = summarize_comstock(high_efficiency, new_build = TRUE)
)

baseline_tbl <- ghg.ccap::imagine_commDesgn_mwh_mcf_perJob_coefficients

# Pull medians for the two reference baselines (full + newBuild)
baseline_mcf <- comstock_summaries$baseline %>% pull(mean_mcf_sqft)
baseline_kwh <- comstock_summaries$baseline %>% pull(mean_kwh_sqft)
baseline_new_mcf <- comstock_summaries$new_build %>% pull(mean_mcf_sqft)
baseline_new_kwh <- comstock_summaries$new_build %>% pull(mean_kwh_sqft)

# Look up the scenario’s medians, picks the correct reference, computes ratios, and scales baseline_tbl
scale_scenario <- function(scenario_key) {
  mv <- comstock_summaries[[scenario_key]]

  # Use the 2010s baseline when the scenario is *_new_build, else full baseline
  use_newbuild_ref <- grepl("_new_build$", scenario_key)
  ref_mcf <- if (use_newbuild_ref) baseline_new_mcf else baseline_mcf
  ref_kwh <- if (use_newbuild_ref) baseline_new_kwh else baseline_kwh

  mcf_ratio <- mv$mean_mcf_sqft / ref_mcf
  mwh_ratio <- mv$mean_kwh_sqft / ref_kwh

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
  "electrification",
  "new_build",
  "new_build_efficient"
  # "electrification_newBuild"
)

# Build the final table -- first, take baseline, retrofit, and heatpump
scenario_comm_des <-
  map_dfr(scenarios, scale_scenario) %>%
  select(scenario, imagine_designation, mcf_per_job, mwh_per_job)


usethis::use_data(comstock_summaries, overwrite = TRUE)
usethis::use_data(scenario_comm_des, overwrite = TRUE)
