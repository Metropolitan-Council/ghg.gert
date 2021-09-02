
# pass_transpo <- read_csv("pass_transpo_dat.csv")

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
