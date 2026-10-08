# pkgload::load_all()

st_paul_passenger <- transportation_data$passenger %>%
  filter(geog_name == "Saint Paul" | geog_name == "All")

st_paul_freight <- transportation_data$freight %>%
  filter(geog_name == "Saint Paul" | geog_name == "All")

lake_elmo_res <- building_energy_data$residential %>%
  dplyr::filter(geog_name == "Lake Elmo")

lake_elmo_non_res <- building_energy_data$non_residential %>%
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
  "Bethel",
  "Chanhassen",
  "Lake Elmo",
  "Saint Bonifacius", # test st vs saint
  # Unique CTUs
  "Chaska", # split county
  "Credit River", # recently incorporated into city from township
  "Hilltop", # all manufactured homes
  "Landfall", # all manufactured homes
  # Townships
  "Benton Twp.",
  "Denmark Twp.",
  # Counties
  "Hennepin County",
  "Scott County"
)

# Full list - comprehensive testing for CI/PR validation
geography_test_list_full <- c(
  # CTUs (alphabetical)
  "Andover",
  "Anoka",
  "Apple Valley",
  "Arden Hills",
  "Bethel",
  "Birchwood Village",
  "Blaine",
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
  "Hastings",
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
  "Robbinsdale",
  "Rosemount",
  "Saint Anthony",
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
# Otherwise test full list on continuous integration (CI) via GitHub Actions
geography_test_list <- if (testthat:::on_ci()) {
  geography_test_list_full
} else {
  geography_test_list_core
}

# Shared BAU run_module_transportation() results, keyed by geography.
# Reused across module- and mode-level tests to avoid recomputing the
# same default scenario multiple times per geography.
transportation_bau_by_geog <- stats::setNames(
  purrr::map(geography_test_list, function(geog) {
    run_module_transportation(.selected_ctu = geog) %>%
      suppressMessages() %>%
      suppressWarnings()
  }),
  geography_test_list
)
