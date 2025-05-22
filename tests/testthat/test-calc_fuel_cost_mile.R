# Gasoline------


fcm <- calc_fuel_cost_mile(
  transportation_data$passenger,
  .mode = "PLDV",
  .aeo_scenario = "REF",
  .miles_per_gallon = "SIMPG",
  .fuel_cost_gallon = 239.8
) %>%
  dplyr::select(year, fuel_cost_mile)


testthat::expect_equal(
  fcm$fuel_cost_mile,
  c(
    8.98888197500506, 8.90870251694994, 8.85604168174681, 5.85626050365989,
    5.55347924084912, 5.46629515007636, 5.38455114792545, 5.36986730090517,
    5.38087411155553
  )
)

# Diesel ------

fcm <- calc_fuel_cost_mile(
  transportation_data$passenger,
  .mode = "PLDV",
  .aeo_scenario = "REF",
  .miles_per_gallon = "CIMPG",
  .fuel_cost_gallon = 264
)


testthat::expect_equal(
  fcm$fuel_cost_mile,
  c(
    8.33756527934633, 8.31262785782006, 8.29608735779988, 8.23345384531611,
    8.48006305055364, 8.50916688680913, 8.51511161376073, 8.54775489817924,
    8.56691981265704
  )
)
