### import ComStock data for electrification and retrofit elasticities

library(dplyr)
library(tidyr)
library(readr)

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

# function to load in resstock files
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

## summarize function
summarize_comstock_new <- function(df) {
  df %>%
    filter(year_bin == "2010s") %>%
    summarize(
      median_kwh = median(building_kwh, na.rm = TRUE),
      median_mcf = median(building_mcf, na.rm = TRUE),
      mean_kwh_savings_nonzero = mean(building_kwh_savings[building_kwh_savings != 0], na.rm = TRUE),
      mean_mcf_savings_nonzero = mean(building_mcf_savings[building_mcf_savings != 0], na.rm = TRUE),
      .groups = "drop"
    )
}

# "0": "Baseline"
baseline <- load_comstock(
  "./data-raw/building_energy_data_processing/comstock_data/MN_upgrade0_agg.csv")

# "50": "Package 1, Wall + Roof Insulation + New Windows",
retrofit_efficiency <- load_comstock(
  "./data-raw/building_energy_data_processing/comstock_data/MN_upgrade50_agg.csv")

# "10": "Cold Climate Challenge HP RTU, Electric Backup",
electrification <- load_comstock(
  "./data-raw/building_energy_data_processing/comstock_data/MN_upgrade10_agg.csv")

# summary list
comstock_summaries <- list(
  baseline = summarize_comstock(baseline),
  retrofit_efficiency = summarize_comstock(retrofit_efficiency),
  electrification = summarize_comstock(electrification)

)

# JOIN COMSTOCK DATA TO COMMUNITY DESIGNATION FACTORS TO DERIVE PER-SCENARIO, PER-JOB MWH & MWH NUMBERS FOR EACH COMM DESIGNATION

# Baseline per job numbers for each community designation
baseline_tbl <- ghg.ccap::imagine_commDesgn_mwh_mcf_perJob_coefficients

# Extract baseline medians
baseline_mcf <- comstock_summaries$baseline$median_mcf
baseline_kwh <- comstock_summaries$baseline$median_kwh

# Function to compute scaled per-job values for each community designation
scale_scenario <- function(scenario_name) {
  median_vals <- comstock_summaries[[scenario_name]]
  mcf_ratio <- median_vals$median_mcf / baseline_mcf
  mwh_ratio <- median_vals$median_kwh / baseline_kwh

  baseline_tbl %>%
    mutate(
      mcf_per_job = mcf_per_job * mcf_ratio,
      mwh_per_job = mwh_per_job * mwh_ratio,
      scenario = scenario_name
    )
}

# Bind all scenarios together -- probably a nicer way to name this
imagine_commDesgn_mwh_mcf_perJob_perScenario_coefficients <- bind_rows(
  baseline_tbl %>% mutate(scenario = "baseline"),
  scale_scenario("retrofit_efficiency"),
  scale_scenario("electrification")
) %>%
  select(scenario, imagine_designation, mcf_per_job, mwh_per_job)

usethis::use_data(comstock_summaries, overwrite = TRUE)
usethis::use_data(imagine_commDesgn_mwh_mcf_perJob_perScenario_coefficients, overwrite = TRUE)
