test_transit_bus <- function(x) {
  testthat::test_that(paste0(x, " emissions reduce with interventions"), {
    # shared BAU from setup.R avoids recomputing the default scenario
    pass <- transportation_bau_by_geog[[x]]$passenger$BU_BRT

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

    bus_bau <- pass$dir_ghg %>%
      filter(
        year == max(year),
        mode == "BU"
      ) %>%
      group_by(geog_name, geog_id, year) %>%
      summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

    bus_bau_vmt <- pass$vmt %>%
      filter(year == max(year), mode == "BU") %>%
      group_by(geog_name, geog_id, year) %>%
      summarise(vmt = sum(vmt), .groups = "keep")


    pass_transit <- suppressMessages(
      suppressWarnings(
        mode_transit_bus(
          .pass_tb = transportation_data$passenger,
          .selected_ctu = x,
          .scenario = "transit",
          .transit_service_pct = .30,
          .transit_avo_pct = 0.5
        )
      )
    )

    pass_lu <- suppressMessages(suppressWarnings(mode_transit_bus(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .scenario = "land_use",
      .emp_dens_pct_change = 0.10,
      .pop_dens_pct_change = 0.10
    )))


    pass_road <- suppressMessages(suppressWarnings(mode_transit_bus(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .scenario = "road",
      .emp_dens_pct_change = 0.10,
      .vmt_fee = 0.01,
      .pldv_avo_pct = 0.5,
      .cong_price = 0.01,
      .parking_price = 20
    )))


    pass_tele <- suppressMessages(suppressWarnings(
      mode_transit_bus(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x,
        .scenario = "telework",
        .emp_dens_pct_change = 0.10,
        .pop_dens_pct_change = 0.10,
        .vmt_fee = 0.01,
        .telework_pct = 0.5,
        .parking_price = 20
      )
    ))

    pass_vmt_reduction <- suppressMessages(suppressWarnings(mode_transit_bus(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .vmt_reduction_pct = 0.10
    )))

    # check that overall emissions decrease with transit scenario
    purrr::map(
      list(
        pass_transit
      ),
      function(x) {
        test_ghg <- x$dir_ghg %>%
          filter(year == max(year)) %>%
          group_by(geog_name, geog_id, year) %>%
          summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

        testthat::expect_lt(test_ghg$dir_ghg, pass_bau$dir_ghg)
      }
    )

    # check that overall VMT increases with land_use, road, and tele scenarios
    purrr::map(
      list(
        pass_lu,
        pass_road,
        pass_tele
      ),
      function(x) {
        test_vmt <- x$vmt %>%
          filter(year == max(year)) %>%
          group_by(geog_name, geog_id, year) %>%
          summarise(vmt = sum(vmt), .groups = "keep")

        testthat::expect_gt(test_vmt$vmt, pass_bau_vmt$vmt)
      }
    )

    # check that bus-specific VMT and emissions don't change with vmt_reduction
    purrr::map(
      list(
        pass_vmt_reduction
      ),
      function(x) {
        test_vmt <- x$vmt %>%
          filter(
            year == max(year),
            mode == "BU"
          ) %>%
          group_by(geog_name, geog_id, year) %>%
          summarise(vmt = sum(vmt), .groups = "keep")

        testthat::expect_equal(test_vmt$vmt, bus_bau_vmt$vmt)


        test_ghg <- x$dir_ghg %>%
          filter(
            year == max(year),
            mode == "BU"
          ) %>%
          group_by(geog_name, geog_id, year) %>%
          summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

        testthat::expect_equal(test_ghg$dir_ghg, bus_bau$dir_ghg)
      }
    )
  })
}
purrr::map(
  geography_test_list,
  test_transit_bus
)
