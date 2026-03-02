### import ResStock data for electrification and retrofit elasticities

# library(dplyr, tidyr, readr)

## function to match eia sqft bins
bin_sqft <- function(sqft) {
  cut(sqft,
    breaks = c(0, 999, 1499, 1999, 2499, 2999, Inf),
    labels = c(
      "Less than 1,000", "1,000 to 1,499", "1,500 to 1,999",
      "2,000 to 2,499", "2,500 to 2,999", "3,000 or more"
    ),
    right = TRUE
  )
}

# function to load in resstock files
load_resstock <- function(path) {
  readr::read_csv(path) %>%
    janitor::clean_names() %>%
    select(
      sqft = in_sqft,
      build_year = in_vintage_acs,
      building_type = in_geometry_building_type_recs,
      heating_fuel = in_heating_fuel,
      hvac_heating_type = in_hvac_heating_type,
      building_kwh = out_electricity_net_energy_consumption_kwh,
      building_nat_gas_kwh = out_natural_gas_total_energy_consumption_kwh
    ) %>%
    mutate(
      building_mcf = building_nat_gas_kwh * 0.00329026,
      mc_classification = case_when(
        grepl("Multi", building_type, ignore.case = TRUE) ~ "multifamily_units",
        grepl("Detached", building_type, ignore.case = TRUE) ~ "single_family_detached",
        grepl("Attached", building_type, ignore.case = TRUE) ~ "single_family_attached",
        grepl("Mobile", building_type, ignore.case = TRUE) ~ "manufactured_home",
        TRUE ~ "other"
      ),
      sqft_bin = bin_sqft(sqft)
    )
}

## summarize function

summarize_resstock <- function(df, building_pattern, grouping_vars) {
  df %>%
    filter(grepl(building_pattern, building_type, ignore.case = TRUE)) %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarize(
      median_kwh = median(building_kwh, na.rm = TRUE),
      median_mcf = median(building_mcf, na.rm = TRUE),
      .groups = "drop"
    )
}

baseline <- load_resstock(
  "./data-raw/building_energy_data_processing/resstock data/MN_baseline_metadata_and_annual_results.csv"
)

heatpump <- load_resstock(
  "./data-raw/building_energy_data_processing/resstock data/MN_upgrade02_metadata_and_annual_results_heat_pump.csv"
)

envelope <- load_resstock(
  "./data-raw/building_energy_data_processing/resstock data/MN_upgrade2.04_metadata_and_annual_results_intermediate_envelope.csv"
)

combo <- load_resstock(
  "./data-raw/building_energy_data_processing/resstock data/MN_upgrade07_metadata_and_annual_results.csv"
)


# summary list

resstock_summaries <- list(

  # baseline summaries
  mf_baseline = summarize_resstock(baseline, "Multi", c("mc_classification", "build_year")),
  manufactured_baseline = summarize_resstock(baseline, "Mobile", c("mc_classification", "build_year")),
  sf_attached_sqft_baseline = summarize_resstock(baseline, "Attached", c("mc_classification", "sqft_bin")),
  sf_attached_year_baseline = summarize_resstock(baseline, "Attached", c("mc_classification", "build_year")),
  sf_detached_sqft_baseline = summarize_resstock(baseline, "Detached", c("mc_classification", "sqft_bin")),
  sf_detached_year_baseline = summarize_resstock(baseline, "Detached", c("mc_classification", "build_year")),

  # Heat pump summaries
  mf_heatpump = summarize_resstock(heatpump, "Multi", c("mc_classification", "build_year")),
  manufactured_heatpump = summarize_resstock(heatpump, "Mobile", c("mc_classification", "build_year")),
  sf_attached_sqft_heatpump = summarize_resstock(heatpump, "Attached", c("mc_classification", "sqft_bin")),
  sf_attached_year_heatpump = summarize_resstock(heatpump, "Attached", c("mc_classification", "build_year")),
  sf_detached_sqft_heatpump = summarize_resstock(heatpump, "Detached", c("mc_classification", "sqft_bin")),
  sf_detached_year_heatpump = summarize_resstock(heatpump, "Detached", c("mc_classification", "build_year")),

  # Retrofit summaries
  mf_envelope = summarize_resstock(envelope, "Multi", c("mc_classification", "build_year")),
  manufactured_envelope = summarize_resstock(envelope, "Mobile", c("mc_classification", "build_year")),
  sf_attached_sqft_envelope = summarize_resstock(envelope, "Attached", c("mc_classification", "sqft_bin")),
  sf_attached_year_envelope = summarize_resstock(envelope, "Attached", c("mc_classification", "build_year")),
  sf_detached_sqft_envelope = summarize_resstock(envelope, "Detached", c("mc_classification", "sqft_bin")),
  sf_detached_year_envelope = summarize_resstock(envelope, "Detached", c("mc_classification", "build_year")),

  # Combination summaries
  mf_combo = summarize_resstock(combo, "Multi", c("mc_classification", "build_year")),
  manufactured_combo = summarize_resstock(combo, "Mobile", c("mc_classification", "build_year")),
  sf_attached_sqft_combo = summarize_resstock(combo, "Attached", c("mc_classification", "sqft_bin")),
  sf_attached_year_combo = summarize_resstock(combo, "Attached", c("mc_classification", "build_year")),
  sf_detached_sqft_combo = summarize_resstock(combo, "Detached", c("mc_classification", "sqft_bin")),
  sf_detached_year_combo = summarize_resstock(combo, "Detached", c("mc_classification", "build_year"))
)


usethis::use_data(resstock_summaries, overwrite = TRUE)
