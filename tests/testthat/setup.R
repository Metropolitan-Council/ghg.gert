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

# Geography test lists for consistent testing across different CTUs and counties

# Core list - fast smoke tests for development (run on every test)
# Includes: 2 major cities, 2 large suburbs, 2 small CTUs, 2 counties
geography_test_list_core <- c(
  # Major cities
  "Minneapolis",
  "Saint Paul",
  # Large suburbs
  "Bloomington",
  "Burnsville",
  "Shakopee",
  # Smaller CTUs
  "Lake Elmo",
  "Andover",
  "Landfall",
  "Saint Bonifacius",
  "Bethel",
  # Townships
  "Benton Twp.",
  "Denmark Twp.",
  # Counties
  "Hennepin County",
  "Ramsey County"
)

# Full list - comprehensive testing for CI/PR validation
geography_test_list_full <- c(
  # CTUs (alphabetical)
  "Arden Hills",
  "Andover",
  "Anoka",
  "Apple Valley",
  "Bethel",
  "Birchwood Village",
  "Bloomington",
  "Burnsville",
  "Centerville",
  "Champlin",
  "Chanhassen",
  "Chaska",
  "Cottage Grove",
  "Crystal",
  "Eden Prairie",
  "Fridley",
  "Hanover",
  "Hopkins",
  "Lake Elmo",
  "Lakeville",
  "Landfall",
  "Maplewood",
  "Minneapolis",
  "Minnetonka",
  "New Trier",
  "Orono",
  "Plymouth",
  "Richfield",
  "Rosemount",
  "Saint Bonifacius",
  "Saint Paul",
  "Shakopee",
  "South Saint Paul",
  "Victoria",
  "White Bear Twp.",
  "Woodbury",
  # Counties (alphabetical)
  "Anoka County",
  "Carver County",
  "Dakota County",
  "Hennepin County",
  "Ramsey County",
  "Scott County",
  "Washington County"
)

# Default to core list for fast development testing
# Default to core list for fast development testing
# Set environment variable GHGCCAP_FULL_TESTS=1 to run full test suite
geography_test_list <- if (testthat:::on_ci()) {
  geography_test_list_full
} else {
  geography_test_list_core
}
