test_that("Parking policy effects correct", {
  parking_adj <- vmt_parking_policy(
    tb = st_paul_passenger,
    .mode = "PLDV",
    .parking_price = 44,
    .enviro_factors = enviro_factors
  )

  testthat::expect_equal(
    parking_adj,
    tibble::tribble(
      ~year, ~ctu, ~park_price_adj,
      "2015", "St. Paul", 1,
      "2018", "St. Paul", 1,
      "2020", "St. Paul", 1,
      "2025", "St. Paul", 0.4500000,
      "2030", "St. Paul", 0.4500000,
      "2035", "St. Paul", 0.4500000,
      "2040", "St. Paul", 0.4500000,
      "2045", "St. Paul", 0.4500000,
      "2050", "St. Paul", 0.4500000
    )
  )


  testthat::expect_equal(
    vmt_parking_policy(
      tb = st_paul_passenger,
      .mode = "PLDV",
      .parking_price = 66,
      .enviro_factors = enviro_factors
    ),
    tibble::tribble(
      ~year, ~ctu, ~park_price_adj,
      "2015", "St. Paul", 1,
      "2018", "St. Paul", 1,
      "2020", "St. Paul", 1,
      "2025", "St. Paul", 0.4500000,
      "2030", "St. Paul", 0.4500000,
      "2035", "St. Paul", 0.4500000,
      "2040", "St. Paul", 0.4500000,
      "2045", "St. Paul", 0.4500000,
      "2050", "St. Paul", 0.4500000
    )
  )


  testthat::expect_equal(
    vmt_parking_policy(
      tb = st_paul_passenger,
      .mode = "BU",
      .parking_price = 66,
      .enviro_factors = enviro_factors
    ),
    tibble::tribble(
      ~year, ~ctu, ~park_price_adj,
      "2015", "St. Paul", 1,
      "2018", "St. Paul", 1,
      "2020", "St. Paul", 1,
      "2025", "St. Paul", 1.29727734,
      "2030", "St. Paul", 1.29727734,
      "2035", "St. Paul", 1.29727734,
      "2040", "St. Paul", 1.29727734,
      "2045", "St. Paul", 1.29727734,
      "2050", "St. Paul", 1.29727734
    )
  )



  sut_park <- vmt_parking_policy(
    tb = st_paul_freight,
    .mode = "SUT",
    .freight_parking_price = 2,
    .enviro_factors = enviro_factors
  )

  testthat::expect_equal(
    sut_park,
    tibble::tribble(
      ~year, ~ctu, ~park_price_adj,
      "2015", "St. Paul", 1,
      "2018", "St. Paul", 1,
      "2020", "St. Paul", 1,
      "2025", "St. Paul", 0.86,
      "2030", "St. Paul", 0.86,
      "2035", "St. Paul", 0.86,
      "2040", "St. Paul", 0.86,
      "2045", "St. Paul", 0.86,
      "2050", "St. Paul", 0.86
    )
  )



  testthat::expect_error(vmt_parking_policy(
    tb = st_paul_passenger,
    .mode = "SUT",
    .freight_parking_price = 2,
    .enviro_factors = enviro_factors
  ))

  testthat::expect_error(vmt_parking_policy(
    tb = st_paul_passenger,
    .mode = "BIKE",
    .parking_price = 1,
    .freight_parking_price = 2,
    .enviro_factors = enviro_factors
  ))


  testthat::expect_equal(
    vmt_parking_policy(
      tb = st_paul_passenger,
      .mode = "PLDV",
      .parking_price = 0,
      .enviro_factors = enviro_factors
    ),
    tibble::tribble(
      ~year, ~ctu, ~park_price_adj,
      "2015", "St. Paul", 1,
      "2018", "St. Paul", 1,
      "2020", "St. Paul", 1,
      "2025", "St. Paul", 1,
      "2030", "St. Paul", 1,
      "2035", "St. Paul", 1,
      "2040", "St. Paul", 1,
      "2045", "St. Paul", 1,
      "2050", "St. Paul", 1
    )
  )



  parking_adj5 <- vmt_parking_policy(
    tb = st_paul_passenger,
    .mode = "PLDV",
    .parking_price = 5,
    .enviro_factors = enviro_factors
  )

  parking_adj10 <- vmt_parking_policy(
    tb = st_paul_passenger,
    .mode = "PLDV",
    .parking_price = 10,
    .enviro_factors = enviro_factors
  )

  # expect the total effect to be greater the higher
  # you go
  testthat::expect_lte(
    sum(parking_adj10$park_price_adj),
    sum(parking_adj5$park_price_adj)
  )
})
