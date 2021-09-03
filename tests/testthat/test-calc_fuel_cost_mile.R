
# this test won't pass until we add global constants as function parameters
# Gasoline------
MPG_AV <- 0.85


fcm <- calc_fuel_cost_mile(
  transportation_data$passenger,
  .mode = "PLDV",
  .aeo_scenario = "REF",
  .miles_per_gallon =  "SIMPG",
  .fuel_cost_gallon = 239.8,
  .av_pct = 0
) %>%
  select(year, fuel_cost_mile)


testthat::expect_equal(fcm$fuel_cost_mile, c(
  8.98888151338707, 8.9087031854957, 8.85604090006863, 8.72516345020066,
  8.59622014714893, 8.46918241267415, 8.34402207896999, 8.22071140965584,
  8.0992230643295
))

# Diesel ------

fcm <- calc_fuel_cost_mile(
  transportation_data$passenger,
  .mode = "PLDV",
  .aeo_scenario = "REF",
  .miles_per_gallon =  "CIMPG",
  .fuel_cost_gallon = 264,
  .av_pct = 0
)



# potential correct solution
tibble::tribble(
  ~year, ~mode, ~var, ~ctu, ~fuel_cost_mile,
  "2015", "PLDV", "CIMPG", "All", 8.33756659328527,
  "2018", "PLDV", "CIMPG", "All", 8.31262870848034,
  "2020", "PLDV", "CIMPG", "All", 8.29608616378944,
  "2025", "PLDV", "CIMPG", "All", 8.25481210144047,
  "2030", "PLDV", "CIMPG", "All", 8.21374338421122,
  "2035", "PLDV", "CIMPG", "All", 8.17287898988479,
  "2040", "PLDV", "CIMPG", "All", 8.13221790132226,
  "2045", "PLDV", "CIMPG", "All", 8.09175910646294,
  "2050", "PLDV", "CIMPG", "All", 8.05150159832325
)
