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

test_vehicle_occupancy <- function(x) {
  testthat::test_that(paste0(x, " vehicle occupancy increases with interventions"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x | geog_name == "All")

    si_vmt_test <- tibble::tibble(
      type = "P",
      stock = "SIStock",
      scenario = "BAU",
      geog_name = x,
      year = unique(pass_tb_filtered$year),
      mode = "PLDV",
      aeo_mode = "LDV",
      vmt = 20,
      class = "SI"
    )

    occupancy_bau <- vmt_vehicle_occupancy(
      tb = pass_tb_filtered,
      .tb_vmt = si_vmt_test,
      .mode = "PLDV",
      .stock = "SIStock",
      .vehicle_occupancy = vehicle_occupancy,
      .transit_avo_pct = 0,
      .pldv_avo_pct = 0,
      .enviro_factors = enviro_factors
    )

    testthat::expect_equal(nrow(occupancy_bau), length(unique(pass_tb_filtered$year)))

    testthat::expect_named(occupancy_bau,
      expected = c(
        "avo_elast",
        "geog_id",
        "geog_name",
        "occupancy_adj",
        "year"
      ),
      ignore.order = TRUE
    )

    occupancy_bau_final <- occupancy_bau %>%
      filter(year == max(year)) %>%
      pull(occupancy_adj)


    occupancy_5pct <- vmt_vehicle_occupancy(
      tb = pass_tb_filtered,
      .tb_vmt = si_vmt_test,
      .mode = "PLDV",
      .stock = "SIStock",
      .vehicle_occupancy = vehicle_occupancy,
      .transit_avo_pct = 0,
      .pldv_avo_pct = 0.05,
      .enviro_factors = enviro_factors
    )

    occupancy_10pct <- vmt_vehicle_occupancy(
      tb = pass_tb_filtered,
      .tb_vmt = si_vmt_test,
      .mode = "PLDV",
      .stock = "SIStock",
      .vehicle_occupancy = vehicle_occupancy,
      .transit_avo_pct = 0,
      .pldv_avo_pct = 0.10,
      .enviro_factors = enviro_factors
    )

    occupancy_25pct <- vmt_vehicle_occupancy(
      tb = pass_tb_filtered,
      .tb_vmt = si_vmt_test,
      .mode = "PLDV",
      .stock = "SIStock",
      .vehicle_occupancy = vehicle_occupancy,
      .transit_avo_pct = 0,
      .pldv_avo_pct = 0.25,
      .enviro_factors = enviro_factors
    )

    purrr::map(
      list(
        occupancy_5pct,
        occupancy_10pct,
        occupancy_25pct
      ),
      function(x) {
        test_adj <- x %>%
          filter(year == max(year)) %>%
          pull(occupancy_adj)

        # Higher occupancy increases the adjustment factor
        testthat::expect_gt(test_adj, occupancy_bau_final)
      }
    )
  })
}

purrr::map(
  geography_test_list,
  test_vehicle_occupancy
)
