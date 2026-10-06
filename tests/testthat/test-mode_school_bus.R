test_school_bus <- function(x) {
  testthat::test_that(paste0(x, " emissions reduce with interventions"), {
    # shared BAU from setup.R avoids recomputing the default scenario
    pass <- transportation_bau_by_geog[[x]]$passenger$BS

    testthat::expect_length(pass, 2)

    testthat::expect_named(pass,
      expected = c(
        "vmt",
        "dir_ghg"
      ),
      ignore.order = TRUE
    )


    pass_bau <- pass$dir_ghg %>%
      filter(year == max(year)) %>%
      group_by(geog_name, year) %>%
      summarise(dir_ghg = sum(dir_ghg), .groups = "keep")


    pass_adj <- adj_fleet_shares(
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = x,
      .vmt_fee = 0.01,
      .hev_pct_sales = 0.10,
      .bev_pct_sales = 0.30
    )


    pass_transit <- suppressMessages(suppressWarnings(mode_school_bus(
      .pass_tb = pass_adj$pass,
      .selected_ctu = x,
      .scenario = "transit",
      .transit_service_pct = .30,
      .transit_avo_pct = 0.5
    )))

    pass_lu <- suppressMessages(suppressWarnings(mode_school_bus(
      .pass_tb = pass_adj$pass,
      .selected_ctu = x,
      .scenario = "land_use",
      .emp_dens_pct_change = 0.10,
      .pop_dens_pct_change = 0.10
    )))


    pass_road <- suppressMessages(suppressWarnings(mode_school_bus(
      .pass_tb = pass_adj$pass,
      .selected_ctu = x,
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
        .selected_ctu = x,
        .scenario = "telework",
        .emp_dens_pct_change = 0.10,
        .vmt_fee = 0.01,
        .telework_pct = 0.5,
        .parking_price = 20
      )
    ))


    pass_vmt_reduction <- suppressMessages(suppressWarnings(mode_school_bus(
      .pass_tb = pass_adj$pass,
      .selected_ctu = x,
      .vmt_reduction_pct = 0.10
    )))


    purrr::map(
      list(
        pass_transit,
        pass_lu,
        pass_road,
        pass_tele,
        pass_vmt_reduction
      ),
      function(x) {
        test_ghg <- x$dir_ghg %>%
          filter(year == max(year)) %>%
          group_by(geog_name, year) %>%
          summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

        testthat::expect_lte(test_ghg$dir_ghg, pass_bau$dir_ghg)
      }
    )
  })
}

purrr::map(
  geography_test_list,
  test_school_bus
)
