
testthat::test_that("Gasoline cost mile correct", {
  fcm <- calc_fuel_cost_mile(
    transportation_data$passenger,
    .mode = "PLDV",
    .aeo_scenario = "REF",
    .miles_per_gallon = "SIMPG",
    .fuel_cost_gallon = 239.8
  ) %>%
    dplyr::select(year, fuel_cost_mile)

  # expect first year is higher than last year
  testthat::expect_gt(dplyr::first(fcm$fuel_cost_mile),
                      dplyr::last(fcm$fuel_cost_mile))

  testthat::expect_equal(
    fcm$fuel_cost_mile,
    c(10.9102588823478, 10.659056319038, 10.4351570507155, 10.4105616406837,
      9.25616593626717, 7.85073519614234, 6.58964386080144, 5.81560501439023,
      5.35514241507722))
})

testthat::test_that("Diesel cost mile correct", {
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


  purrr::map(
    fcm$fuel_cost_mile,
    testthat::expect_lt,
    10
  )


})

testthat::test_that("Gasoline cost mile correct, different AEO scenarios", {

  testthat::expect_error(
    calc_fuel_cost_mile(
      transportation_data$passenger,
      .mode = "PLDV",
      .aeo_scenario = "RN",
      .miles_per_gallon = "SIMPG",
      .fuel_cost_gallon = 239.8
    ))

  testthat::expect_error(
    calc_fuel_cost_mile(
      transportation_data$passenger,
      .mode = "PLDV",
      .aeo_scenario = "REF",
      .miles_per_gallon = "gasoline",
      .fuel_cost_gallon = 239.8
    ))



  fcm_ref <- calc_fuel_cost_mile(
    transportation_data$passenger,
    .mode = "PLDV",
    .aeo_scenario = "REF",
    .miles_per_gallon = "SIMPG",
    .fuel_cost_gallon = 239.8
  ) %>%
    dplyr::select(year, fuel_cost_mile)

  fcm_hogs <- calc_fuel_cost_mile(
    transportation_data$passenger,
    .mode = "PLDV",
    .aeo_scenario = "HOGS",
    .miles_per_gallon = "SIMPG",
    .fuel_cost_gallon = 239.8
  ) %>%
    dplyr::select(year, fuel_cost_mile)


  fcm_hm <- calc_fuel_cost_mile(
    transportation_data$passenger,
    .mode = "PLDV",
    .aeo_scenario = "HM",
    .miles_per_gallon = "SIMPG",
    .fuel_cost_gallon = 239.8
  ) %>%
    dplyr::select(year, fuel_cost_mile)


  fcm_lm <- calc_fuel_cost_mile(
    transportation_data$passenger,
    .mode = "PLDV",
    .aeo_scenario = "LM",
    .miles_per_gallon = "SIMPG",
    .fuel_cost_gallon = 239.8
  ) %>%
    dplyr::select(year, fuel_cost_mile)

  # expect HOGS to increase fuel cost per gallon
  testthat::expect_lt(dplyr::last(fcm_ref$fuel_cost_mile),
                      dplyr::last(fcm_hogs$fuel_cost_mile))

  # expect HM to be decrease fuel cost
  testthat::expect_gt(dplyr::last(fcm_ref$fuel_cost_mile),
                      dplyr::last(fcm_hm$fuel_cost_mile))

  # expect LM to increase fuel cost
  testthat::expect_lt(dplyr::last(fcm_ref$fuel_cost_mile),
                      dplyr::last(fcm_lm$fuel_cost_mile))


  purrr::map(
    fcm_ref$fuel_cost_mile,
    testthat::expect_lt,
    20
  )

})


testthat::test_that("Diesel cost mile correct, different AEO scenarios", {

  testthat::expect_error(
    calc_fuel_cost_mile(
      transportation_data$passenger,
      .mode = "PLDV",
      .aeo_scenario = "RN",
      .miles_per_gallon = "CIMPG",
      .fuel_cost_gallon = 264
    ))

  fcm_ref <- calc_fuel_cost_mile(
    transportation_data$passenger,
    .mode = "PLDV",
    .aeo_scenario = "REF",
    .miles_per_gallon = "CIMPG",
    .fuel_cost_gallon = 264
  ) %>%
    dplyr::select(year, fuel_cost_mile)

  fcm_hogs <- calc_fuel_cost_mile(
    transportation_data$passenger,
    .mode = "PLDV",
    .aeo_scenario = "HOGS",
    .miles_per_gallon = "CIMPG",
    .fuel_cost_gallon = 264
  ) %>%
    dplyr::select(year, fuel_cost_mile)

  fcm_hm <- calc_fuel_cost_mile(
    transportation_data$passenger,
    .mode = "PLDV",
    .aeo_scenario = "HM",
    .miles_per_gallon = "CIMPG",
    .fuel_cost_gallon = 264
  ) %>%
    dplyr::select(year, fuel_cost_mile)

  fcm_lm <- calc_fuel_cost_mile(
    transportation_data$passenger,
    .mode = "PLDV",
    .aeo_scenario = "LM",
    .miles_per_gallon = "CIMPG",
    .fuel_cost_gallon = 264
  ) %>%
    dplyr::select(year, fuel_cost_mile)


  # expect HOGS to increase fuel cost per gallon
  testthat::expect_lt(dplyr::last(fcm_ref$fuel_cost_mile),
                      dplyr::last(fcm_hogs$fuel_cost_mile))

  # expect HM to be decrease fuel cost
  testthat::expect_gt(dplyr::last(fcm_ref$fuel_cost_mile),
                      dplyr::last(fcm_hm$fuel_cost_mile))

  # expect LM to increase fuel cost
  testthat::expect_lt(dplyr::last(fcm_ref$fuel_cost_mile),
                      dplyr::last(fcm_lm$fuel_cost_mile))

  purrr::map(
    fcm_ref$fuel_cost_mile,
    testthat::expect_lt,
    20
  )

})


testthat::test_that("BEV cost mile correct, different AEO scenarios", {

  testthat::expect_error(
    calc_fuel_cost_mile(
      transportation_data$passenger,
      .mode = "PLDV",
      .aeo_scenario = "RN",
      .miles_per_gallon = "BEV",
      .fuel_cost_gallon = enviro_factors$ELEC_FUEL_COST_KWH
    ))

  fcm_ref <- calc_fuel_cost_mile(
    transportation_data$passenger,
    .mode = "PLDV",
    .aeo_scenario = "REF",
    .miles_per_gallon = "BEVElec",
    .fuel_cost_gallon = enviro_factors$ELEC_FUEL_COST_KWH
  ) %>%
    dplyr::select(year, fuel_cost_mile)

  fcm_hogs <- calc_fuel_cost_mile(
    transportation_data$passenger,
    .mode = "PLDV",
    .aeo_scenario = "HOGS",
    .miles_per_gallon = "BEVElec",
    .fuel_cost_gallon = enviro_factors$ELEC_FUEL_COST_KWH
  ) %>%
    dplyr::select(year, fuel_cost_mile)

  fcm_hm <- calc_fuel_cost_mile(
    transportation_data$passenger,
    .mode = "PLDV",
    .aeo_scenario = "HM",
    .miles_per_gallon = "BEVElec",
    .fuel_cost_gallon = enviro_factors$ELEC_FUEL_COST_KWH
  ) %>%
    dplyr::select(year, fuel_cost_mile)


  fcm_lm <- calc_fuel_cost_mile(
    transportation_data$passenger,
    .mode = "PLDV",
    .aeo_scenario = "LM",
    .miles_per_gallon = "BEVElec",
    .fuel_cost_gallon = enviro_factors$ELEC_FUEL_COST_KWH
  ) %>%
    dplyr::select(year, fuel_cost_mile)


  # expect HOGS to increase fuel cost per gallon
  testthat::expect_lt(dplyr::last(fcm_ref$fuel_cost_mile),
                      dplyr::last(fcm_hogs$fuel_cost_mile))

  # expect HM to be decrease fuel cost
  testthat::expect_gt(dplyr::last(fcm_ref$fuel_cost_mile),
                      dplyr::last(fcm_hm$fuel_cost_mile))

  # expect LM to increase fuel cost
  testthat::expect_lt(dplyr::last(fcm_ref$fuel_cost_mile),
                      dplyr::last(fcm_lm$fuel_cost_mile))


  purrr::map(
    fcm_ref$fuel_cost_mile,
    testthat::expect_lt,
    20
  )

})

