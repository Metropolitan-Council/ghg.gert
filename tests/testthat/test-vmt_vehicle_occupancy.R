testthat::test_that("Vehicle occupancy adjustment correct", {
  si_vmt_test <- tibble::tribble(
    ~type, ~stock, ~scenario, ~geog_name, ~year, ~mode, ~aeo_mode, ~vmt, ~class,
    "P", "SIStock", "BAU", "Saint Paul", "2015", "PLDV", "LDV", 0, "SI",
    "P", "SIStock", "BAU", "Saint Paul", "2018", "PLDV", "LDV", 22.1221709054726, "SI",
    "P", "SIStock", "BAU", "Saint Paul", "2020", "PLDV", "LDV", 0, "SI",
    "P", "SIStock", "BAU", "Saint Paul", "2025", "PLDV", "LDV", 20.8536041006151, "SI",
    "P", "SIStock", "BAU", "Saint Paul", "2030", "PLDV", "LDV", 20.4559473396012, "SI",
    "P", "SIStock", "BAU", "Saint Paul", "2035", "PLDV", "LDV", 20.1702772754979, "SI",
    "P", "SIStock", "BAU", "Saint Paul", "2040", "PLDV", "LDV", 18.8835183369188, "SI",
    "P", "SIStock", "BAU", "Saint Paul", "2045", "PLDV", "LDV", 18.3082802849089, "SI",
    "P", "SIStock", "BAU", "Saint Paul", "2050", "PLDV", "LDV", 17.7590876643119, "SI"
  )

  # .pldv_avo_pct == 0 ----
  si_veh_test_none <- vmt_vehicle_occupancy(
    tb = transportation_data$passenger,
    .tb_vmt = si_vmt_test,
    .mode = "PLDV",
    .stock = "SIStock",
    .vehicle_occupancy = vehicle_occupancy,
    .transit_avo_pct = 0,
    .pldv_avo_pct = 0,
    .enviro_factors = enviro_factors
  ) %>%
    filter(geog_name == "Saint Paul")

  testthat::expect_equal(
    unique(si_veh_test_none$occupancy_adj),
    c(1.6),
    tolerance = 0.01
  )

  # PLDV occupancy of 5% ----
  si_veh_test <- vmt_vehicle_occupancy(
    tb = transportation_data$passenger,
    .tb_vmt = si_vmt_test,
    .mode = "PLDV",
    .stock = "SIStock",
    .vehicle_occupancy = vehicle_occupancy,
    .transit_avo_pct = 0,
    .pldv_avo_pct = 0.05,
    .enviro_factors = enviro_factors
  ) %>%
    filter(geog_name == "Saint Paul")

  testthat::expect_equal(
    si_veh_test$occupancy_adj,
    c(
      1.60003983277867, 1.60003983277867, 1.60003983277867, 1.6200403306884,
      1.64004082859813, 1.66004132650787, 1.6800418244176, 1.6800418244176,
      1.6800418244176
    ),
  tolerance = 0.001
  )
})
