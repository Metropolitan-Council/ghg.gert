testthat::test_that("Transit adjustment correct", {
  enviro_factors_edit <- enviro_factors

  enviro_factors_edit$TRANSIT_SERVICE_ELAST <- 47 / 100

  pass_trans <- vmt_transit_service(
    tb = st_paul_passenger,
    .mode = "PLDV",
    .transit_service_pct = 0.10,
    .enviro_factors = enviro_factors_edit
  )

  testthat::expect_equal(
    pass_trans %>%
      select(-geog_id),
    tibble::tribble(
      ~year, ~geog_name, ~transit_adj,
      "2015", "Saint Paul", 0,
      "2018", "Saint Paul", 0,
      "2020", "Saint Paul", 0,
      "2025", "Saint Paul", 1358720.061325,
      "2030", "Saint Paul", 2856227.98305,
      "2035", "Saint Paul", 4502815.566675,
      "2040", "Saint Paul", 6295052.2164,
      "2045", "Saint Paul", 6586350.3439,
      "2050", "Saint Paul", 6877648.4667
    )
  )

  # check multipliers
  testthat::expect_equal(
    vmt_transit_service(
      tb = st_paul_passenger,
      .mode = "BU",
      .transit_service_pct = 0.1,
      .enviro_factors = enviro_factors_edit
    ) %>% select(-geog_id),
    tibble::tribble(
      ~year, ~geog_name, ~transit_adj,
      "2015", "Saint Paul", 1,
      "2018", "Saint Paul", 1,
      "2020", "Saint Paul", 1,
      "2025", "Saint Paul", 1.025,
      "2030", "Saint Paul", 1.05,
      "2035", "Saint Paul", 1.075,
      "2040", "Saint Paul", 1.1,
      "2045", "Saint Paul", 1.1,
      "2050", "Saint Paul", 1.1
    )
  )


  testthat::expect_equal(
    vmt_transit_service(
      tb = st_paul_passenger,
      .mode = "RI",
      .transit_service_pct = 0.1,
      .enviro_factors = enviro_factors_edit
    ) %>% select(-geog_id),
    tibble::tribble(
      ~year, ~geog_name, ~transit_adj,
      "2015", "Saint Paul", 1,
      "2018", "Saint Paul", 1,
      "2020", "Saint Paul", 1,
      "2025", "Saint Paul", 1.025,
      "2030", "Saint Paul", 1.050,
      "2035", "Saint Paul", 1.075,
      "2040", "Saint Paul", 1.1,
      "2045", "Saint Paul", 1.1,
      "2050", "Saint Paul", 1.1
    )
  )
})
