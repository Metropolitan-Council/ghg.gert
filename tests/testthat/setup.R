# pkgload::load_all()

st_paul_passenger <- transportation_data$passenger %>%
  filter(ctu == "St. Paul" | ctu == "All")

st_paul_freight <- transportation_data$freight %>%
  filter(ctu == "St. Paul" | ctu == "All")

lake_elmo_res <- building_data$residential %>%
  dplyr::filter(ctu_name == "Lake Elmo")

lake_elmo_non_res <- building_data$non_residential %>%
  dplyr::filter(ctu_name == "Lake Elmo")

si_fcm_test <- calc_fuel_cost_mile(
  st_paul_passenger,
  .mode = "PLDV",
  .aeo_scenario = "REF",
  .miles_per_gallon =  "SIMPG",
  .fuel_cost_gallon = 239.8,
  .av_pct = 0
)


ci_fcm_test <- calc_fuel_cost_mile(
  st_paul_passenger,
  .mode = "PLDV",
  .aeo_scenario = "REF",
  .miles_per_gallon =  "CIMPG",
  .fuel_cost_gallon = enviro_factors$CI_FUEL_COST_GAL,
  .av_pct = 0
)
