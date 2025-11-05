testthat::test_that("Saint Paul emissions reduce with interventions", {
  pass <- suppressMessages(
    suppressWarnings(
      mode_school_bus(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = "Saint Paul"
      )
    )
  )

  testthat::expect_length(pass, 2)

  testthat::expect_named(pass,
    expected = c(
      "vmt",
      "dir_ghg"
    ),
    ignore.order = TRUE
  )


  pass_bau <- pass$dir_ghg %>%
    filter(year == "2040") %>%
    group_by(geog_name, year) %>%
    summarise(dir_ghg = sum(dir_ghg), .groups = "keep")


  pass_adj <- adj_fleet_shares(
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .selected_ctu = "Saint Paul",
    .vmt_fee = 0.01,
    .hev_pct_sales = 0.10,
    .bev_pct_sales = 0.30
  )


  pass_transit <- suppressMessages(suppressWarnings(mode_school_bus(
    .pass_tb = pass_adj$pass,
    .selected_ctu = "Saint Paul",
    .scenario = "transit",
    .transit_service_pct = .30,
    .transit_avo_pct = 0.5
  )))

  pass_lu <- suppressMessages(suppressWarnings(mode_school_bus(
    .pass_tb = pass_adj$pass,
    .selected_ctu = "Saint Paul",
    .scenario = "land_use",
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10
  )))


  pass_road <- suppressMessages(suppressWarnings(mode_school_bus(
    .pass_tb = pass_adj$pass,
    .selected_ctu = "Saint Paul",
    .scenario = "road",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .pldv_avo_pct = 0.5,
    .cong_price = 0.01,
    .parking_price = 20
  )))


  pass_tele <- suppressMessages(suppressWarnings(
    mode_school_bus(
      .pass_tb = pass_adj$pass,
      .selected_ctu = "Saint Paul",
      .scenario = "telework",
      .emp_dens_pct_change = 0.10,
      .vmt_fee = 0.01,
      .telework_pct = 0.5,
      .parking_price = 20
    )
  ))



  purrr::map(
    list(
      pass_transit,
      pass_lu,
      pass_road,
      pass_tele
    ),
    function(x) {
      test_ghg <- x$dir_ghg %>%
        filter(year == "2040") %>%
        group_by(geog_name, year) %>%
        summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

      testthat::expect_lte(test_ghg$dir_ghg, pass_bau$dir_ghg)
    }
  )
})


testthat::test_that("Minneapolis emissions reduce with interventions", {
  pass <- suppressMessages(
    suppressWarnings(
      mode_school_bus(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = "Minneapolis"
      )
    )
  )

  testthat::expect_length(pass, 2)

  testthat::expect_named(pass,
    expected = c(
      "vmt",
      "dir_ghg"
    ),
    ignore.order = TRUE
  )


  pass_bau <- pass$dir_ghg %>%
    filter(year == "2040") %>%
    group_by(geog_name, year) %>%
    summarise(dir_ghg = sum(dir_ghg), .groups = "keep")


  pass_transit <- suppressMessages(suppressWarnings(mode_school_bus(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minneapolis",
    .scenario = "transit",
    .transit_service_pct = .30,
    .transit_avo_pct = 0.5
  )))

  pass_lu <- suppressMessages(suppressWarnings(mode_school_bus(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minneapolis",
    .scenario = "land_use",
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10
  )))


  pass_road <- suppressMessages(suppressWarnings(mode_school_bus(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minneapolis",
    .scenario = "road",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .pldv_avo_pct = 0.5,
    .cong_price = 0.01,
    .parking_price = 20
  )))


  pass_tele <- suppressMessages(suppressWarnings(mode_school_bus(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minneapolis",
    .scenario = "telework",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .telework_pct = 0.5,
    .parking_price = 20
  )))



  purrr::map(
    list(
      pass_transit,
      pass_lu,
      pass_road,
      pass_tele
    ),
    function(x) {
      test_ghg <- x$dir_ghg %>%
        filter(year == "2040") %>%
        group_by(geog_name, year) %>%
        summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

      testthat::expect_lte(test_ghg$dir_ghg, pass_bau$dir_ghg)
    }
  )
})
