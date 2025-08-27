# pkgload::load_all()

st_paul_passenger <- transportation_data$passenger %>%
  filter(geog_name == "Saint Paul" | geog_name == "All")

st_paul_freight <- transportation_data$freight %>%
  filter(geog_name == "Saint Paul" | geog_name == "All")

lake_elmo_res <- building_data$residential %>%
  dplyr::filter(geog_name == "Lake Elmo")

lake_elmo_non_res <- building_data$non_residential %>%
  dplyr::filter(geog_name == "Lake Elmo")

si_fcm_test <- calc_fuel_cost_mile(
  st_paul_passenger,
  .mode = "PLDV",
  .aeo_scenario = "REF",
  .miles_per_gallon = "SIMPG",
  .fuel_cost_gallon = 239.8
)


ci_fcm_test <- calc_fuel_cost_mile(
  st_paul_passenger,
  .mode = "PLDV",
  .aeo_scenario = "REF",
  .miles_per_gallon = "CIMPG",
  .fuel_cost_gallon = enviro_factors$CI_FUEL_COST_GAL
)
