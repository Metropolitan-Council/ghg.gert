test_walk_bike <- function(x) {
  testthat::test_that(paste0(x, " emissions constant and walk/bike vmt increase"), {
    # shared BAU from setup.R avoids recomputing the default scenario
    pass <- transportation_bau_by_geog[[x]]$passenger$WALK_BIKE

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
      group_by(geog_name, geog_id, year) %>%
      summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

    pass_bau_vmt <- pass$vmt %>%
      filter(year == max(year)) %>%
      group_by(geog_name, geog_id, year) %>%
      summarise(vmt = sum(vmt), .groups = "keep")


    pass_transit <- suppressMessages(suppressWarnings(mode_walk_bike(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .scenario = "transit",
      .transit_service_pct = .30,
      .transit_avo_pct = 0.5
    )))

    pass_lu <- suppressMessages(suppressWarnings(mode_walk_bike(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .scenario = "land_use",
      .emp_dens_pct_change = 0.10,
      .pop_dens_pct_change = 0.10
    )))


    pass_road <- suppressMessages(suppressWarnings(mode_walk_bike(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .scenario = "road",
      .emp_dens_pct_change = 0.10,
      .vmt_fee = 0.01,
      .pldv_avo_pct = 0.5,
      .cong_price = 0.01,
      .parking_price = 20
    )))


    pass_tele <- suppressMessages(suppressWarnings(mode_walk_bike(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .scenario = "telework",
      .emp_dens_pct_change = 0.10,
      .vmt_fee = 0.01,
      .telework_pct = 0.5,
      .parking_price = 20
    )))

    pass_multi <- suppressMessages(suppressWarnings(mode_walk_bike(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .scenario = "telework",
      .emp_dens_pct_change = 0.10,
      .pop_dens_pct_change = 0.10,
      .vmt_fee = 0.01,
      .parking_price = 20
    )))

    # check that emissions stay constant at 0
    purrr::map(
      list(
        pass_transit,
        pass_lu,
        pass_road,
        pass_tele,
        pass_multi
      ),
      function(x) {
        test_ghg <- x$dir_ghg %>%
          filter(year == max(year)) %>%
          group_by(geog_name, geog_id, year) %>%
          summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

        # expect emissions to always be 0
        testthat::expect_equal(test_ghg$dir_ghg, pass_bau$dir_ghg)
      }
    )

    # check that VMT increases or stays constant
    purrr::map(
      list(
        pass_transit,
        pass_lu,
        pass_road,
        pass_tele,
        pass_multi
      ),
      function(x) {
        test_vmt <- x$vmt %>%
          filter(year == max(year)) %>%
          group_by(geog_name, geog_id, year) %>%
          summarise(vmt = sum(vmt), .groups = "keep")

        # expect walk/bike vmt to increase or stay constant
        testthat::expect_gte(test_vmt$vmt, pass_bau_vmt$vmt)
      }
    )
  })
}

purrr::map(
  geography_test_list,
  test_walk_bike
)
