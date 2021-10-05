
# Gasoline------


fcm <- calc_fuel_cost_mile(
  transportation_data$passenger,
  .mode = "PLDV",
  .aeo_scenario = "REF",
  .miles_per_gallon =  "SIMPG",
  .fuel_cost_gallon = 239.8,
  .av_pct = 0
) %>%
  select(year, fuel_cost_mile)


testthat::expect_equal(fcm$fuel_cost_mile,
                       c(8.98888151338707, 8.9087031854957, 8.85604090006863, 8.72516345020065,
                         8.59622014714893, 8.46918241267415, 8.34402207896999, 8.22071140965584,
                         8.0992230643295))

# Diesel ------

fcm <- calc_fuel_cost_mile(
  transportation_data$passenger,
  .mode = "PLDV",
  .aeo_scenario = "REF",
  .miles_per_gallon =  "CIMPG",
  .fuel_cost_gallon = 264,
  .av_pct = 0
)


testthat::expect_equal(
  fcm$fuel_cost_mile,
  c(
    8.33756659328527, 8.31262870848034,
    8.29608616378944, 8.25481210144047,
    8.21374338421122, 8.17287898988479,
    8.13221790132226, 8.09175910646294,
    8.05150159832325
  )
)

