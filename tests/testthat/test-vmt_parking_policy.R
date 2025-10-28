test_that("Parking policy effects correct", {
  parking_adj <- vmt_parking_policy(
    tb = st_paul_passenger,
    .parking_cost = parking_cost,
    .mode = "PLDV",
    .parking_price = 44,
    .enviro_factors = enviro_factors
  )

  testthat::expect_equal(
    parking_adj %>%
      select(-geog_id),
    tibble::tribble(
      ~year, ~geog_name, ~park_price_adj,
      "2015", "Saint Paul", 1,
      "2018", "Saint Paul", 1,
      "2020", "Saint Paul", 1,
      "2025", "Saint Paul", 0.731618027,
      "2030", "Saint Paul", 0.731618027,
      "2035", "Saint Paul", 0.731618027,
      "2040", "Saint Paul", 0.731618027,
      "2045", "Saint Paul", 0.731618027,
      "2050", "Saint Paul", 0.731618027
    ),
    tolerance = 0.01

  )


  testthat::expect_equal(
    vmt_parking_policy(
      tb = st_paul_passenger,
      .parking_cost = parking_cost,
      .mode = "PLDV",
      .parking_price = 66,
      .enviro_factors = enviro_factors
    ) %>% select(-geog_id),
    tibble::tribble(
      ~year, ~geog_name, ~park_price_adj,
      "2015", "Saint Paul", 1,
      "2018", "Saint Paul", 1,
      "2020", "Saint Paul", 1,
      "2025", "Saint Paul", 0.597427040,
      "2030", "Saint Paul", 0.597427040,
      "2035", "Saint Paul", 0.597427040,
      "2040", "Saint Paul", 0.597427040,
      "2045", "Saint Paul", 0.597427040,
      "2050", "Saint Paul", 0.597427040
    ),
    tolerance = 0.01
  )


  testthat::expect_equal(
    vmt_parking_policy(
      tb = st_paul_passenger,
      .parking_cost = parking_cost,
      .mode = "BU",
      .parking_price = 66,
      .enviro_factors = enviro_factors
    ) %>% select(-geog_id),
    tibble::tribble(
      ~year, ~geog_name, ~park_price_adj,
      "2015", "Saint Paul", 1,
      "2018", "Saint Paul", 1,
      "2020", "Saint Paul", 1,
      "2025", "Saint Paul", 1.057510,
      "2030", "Saint Paul", 1.057510,
      "2035", "Saint Paul", 1.057510,
      "2040", "Saint Paul", 1.057510,
      "2045", "Saint Paul", 1.057510,
      "2050", "Saint Paul", 1.057510
    ),
    tolerance = 0.001
  )



  sut_park <- vmt_parking_policy(
    tb = st_paul_freight,
    .parking_cost = parking_cost,
    .mode = "SUT",
    .freight_parking_price = 2,
    .enviro_factors = enviro_factors
  )

  testthat::expect_equal(
    sut_park %>% select(-geog_id),
    tibble::tribble(
      ~year, ~geog_name, ~park_price_adj,
      "2015", "Saint Paul", 1,
      "2018", "Saint Paul", 1,
      "2020", "Saint Paul", 1,
      "2025", "Saint Paul", 0.86,
      "2030", "Saint Paul", 0.86,
      "2035", "Saint Paul", 0.86,
      "2040", "Saint Paul", 0.86,
      "2045", "Saint Paul", 0.86,
      "2050", "Saint Paul", 0.86
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
      .parking_cost = parking_cost,
      .mode = "PLDV",
      .parking_price = 0,
      .enviro_factors = enviro_factors
    ) %>% select(-geog_id),
    tibble::tribble(
      ~year, ~geog_name, ~park_price_adj,
      "2015", "Saint Paul", 1,
      "2018", "Saint Paul", 1,
      "2020", "Saint Paul", 1,
      "2025", "Saint Paul", 1,
      "2030", "Saint Paul", 1,
      "2035", "Saint Paul", 1,
      "2040", "Saint Paul", 1,
      "2045", "Saint Paul", 1,
      "2050", "Saint Paul", 1
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
