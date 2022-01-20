
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


testthat::expect_equal(
  fcm$fuel_cost_mile,
  c(
    8.98888197500506, 8.90870251694994, 8.85604168174681, 8.7251621231907,
    8.59622067408996, 8.46918284040429, 8.34402142861617, 8.22071276802163,
    8.09922256920309
  )
)

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
    8.33756527934633, 8.31262785782006, 8.29608735779988, 8.25481100704004,
    8.21374402053436, 8.17287867361609, 8.13221753558692, 8.09175808463182,
    8.05150277944585
  )
)
