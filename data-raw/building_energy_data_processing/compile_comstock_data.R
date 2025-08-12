### import ResStock data for electrification and retrofit elasticities

library(dplyr)
library(tidyr)
library(readr)


# function to load in resstock files
load_comstock <- function(path) {
  readr::read_csv(path) %>%
    janitor::clean_names() %>%
    select(
      heating_fuel = in_heating_fuel,
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

summarize_comstock <- function(df, building_pattern, grouping_vars) {
  df %>%
    filter(grepl(building_pattern, building_type, ignore.case = TRUE)) %>%
    summarize(
      median_kwh = median(building_kwh, na.rm = TRUE),
      median_mcf = median(building_mcf, na.rm = TRUE),
      .groups = "drop"
    )
}

baseline <- load_comstock(
  "./data-raw/building_energy_data_processing/comstock data/MN_baseline_metadata_and_annual_results.csv")


# CCHPC — Cold-Climate Heat Pump conversion case
heatpump <- load_comstock(
  "./data-raw/building_energy_data_processing/resstock data/MN_upgrade02_metadata_and_annual_results_heat_pump.csv")

envelope <- load_resstock(
  "./data-raw/building_energy_data_processing/resstock data/MN_upgrade2.04_metadata_and_annual_results_intermediate_envelope.csv")

electrification

# pkg_0009	Demand Flexibility, Lighting + Thermostat Control, Load Shed for Daily Bldg Peak Reduction
demand_flex <-

deep_efficiency



# summary list

resstock_summaries <- list(

  #baseline summaries
  mf_baseline = summarize_resstock(baseline, "Multi", "mc_classification"),
  manufactured_baseline = summarize_resstock(baseline, "Mobile", "mc_classification"),
  sf_attached_sqft_baseline = summarize_resstock(baseline, "Attached", c("mc_classification", "sqft_bin")),
  sf_attached_year_baseline = summarize_resstock(baseline, "Attached", c("mc_classification", "build_year")),
  sf_detached_sqft_baseline = summarize_resstock(baseline, "Detached", c("mc_classification", "sqft_bin")),
  sf_detached_year_baseline = summarize_resstock(baseline, "Detached", c("mc_classification", "build_year")),

  # Heat pump summaries
  mf_heatpump = summarize_resstock(heatpump, "Multi", "mc_classification"),
  manufactured_heatpump = summarize_resstock(heatpump, "Mobile", "mc_classification"),
  sf_attached_sqft_heatpump = summarize_resstock(heatpump, "Attached", c("mc_classification", "sqft_bin")),
  sf_attached_year_heatpump = summarize_resstock(heatpump, "Attached", c("mc_classification", "build_year")),
  sf_detached_sqft_heatpump = summarize_resstock(heatpump, "Detached", c("mc_classification", "sqft_bin")),
  sf_detached_year_heatpump = summarize_resstock(heatpump, "Detached", c("mc_classification", "build_year")),

  # Retrofit summaries
  mf_envelope = summarize_resstock(envelope, "Multi", "mc_classification"),
  manufactured_envelope = summarize_resstock(envelope, "Mobile", "mc_classification"),
  sf_attached_sqft_envelope = summarize_resstock(envelope, "Attached", c("mc_classification", "sqft_bin")),
  sf_attached_year_envelope = summarize_resstock(envelope, "Attached", c("mc_classification", "build_year")),
  sf_detached_sqft_envelope = summarize_resstock(envelope, "Detached", c("mc_classification", "sqft_bin")),
  sf_detached_year_envelope = summarize_resstock(envelope, "Detached", c("mc_classification", "build_year"))
)


usethis::use_data(resstock_summaries, overwrite = TRUE)
