### import ResStock data for electrification and retrofit elasticities

library(dplyr)
library(tidyr)
library(readr)


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


# name changed from
baseline <- load_comstock(
  "./data-raw/building_energy_data_processing/comstock_data/MN_business_baseline.csv")

# pkg1 --- energy efficiency-focused basic retrofit (wall, roof and windows)
retrofit_efficiency <- load_comstock(
  "./data-raw/building_energy_data_processing/comstock_data/MN_upgrade47_agg.csv")

# Upgrade 50: LED Lighting, Standard Performance HP-RTU and ASHP-Boiler
electrification <- load_comstock(
  "./data-raw/building_energy_data_processing/comstock_data/MN_upgrade50_agg.csv")

# Upgrade 10: CCHPC — Cold-Climate Heat Pump conversion case (10)
# Upgrade 55: pkg_0009 -- Demand Flexibility, Lighting + Thermostat Control, Load Shed for Daily Bldg Peak Reduction
# Upgrade 49: pkg_0003 -- Wall and Roof Insulation, New Windows, LED Lighting,  HP-RTU and ASHP-Boiler

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
