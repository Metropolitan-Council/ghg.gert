test_passenger <- function(x) {
  testthat::test_that(paste0(x, " emissions reduce with interventions"), {
    # shared BAU from setup.R avoids recomputing the default scenario
    pass <- transportation_bau_by_geog[[x]]$passenger$PLDV

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

    pass_bau_vmt <- pass$vmt %>%
      filter(year == max(year)) %>%
      group_by(geog_name, year) %>%
      summarise(vmt = sum(vmt), .groups = "keep")


    pass_transit <- suppressMessages(mode_passenger_light_duty(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .scenario = "transit",
      .transit_service_pct = .30
    ))


    pass_lu <- suppressMessages(mode_passenger_light_duty(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .scenario = "land_use",
      .emp_dens_pct_change = 0.10,
      .pop_dens_pct_change = 0.10,
      .intersection_density_pct_change = 0.10
    ))

    pass_lu_int <- suppressMessages(mode_passenger_light_duty(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .scenario = "land_use",
      .intersection_density_pct_change = 0.15
    ))


    pass_road <- suppressMessages(mode_passenger_light_duty(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .scenario = "road",
      .emp_dens_pct_change = 0.10,
      .vmt_fee = 0.01,
      .pldv_avo_pct = 0.5,
      .cong_price = 0.01,
      .parking_price = 20
    ))


    pass_tele <- suppressMessages(mode_passenger_light_duty(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .scenario = "telework",
      .emp_dens_pct_change = 0.10,
      .vmt_fee = 0.01,
      .telework_pct = 0.5,
      .parking_price = 20
    ))


    pass_vmt_reduction <- suppressMessages(mode_passenger_light_duty(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .vmt_reduction_pct = 0.10
    ))

    pass_ctr <- suppressMessages(mode_passenger_light_duty(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .scenario = "ctr",
      .ctr_employees_targeted = 0.5,
      .ctr_voluntary = FALSE,
      .ctr_start_year = "2025"
    ))

    # check that emissions decrease
    purrr::map(
      list(
        pass_transit,
        pass_lu,
        pass_lu_int,
        pass_road,
        pass_tele,
        pass_vmt_reduction,
        pass_ctr
      ),
      function(x) {
        test_ghg <- x$dir_ghg %>%
          filter(year == max(year)) %>%
          group_by(geog_name, year) %>%
          summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

        testthat::expect_lt(test_ghg$dir_ghg, pass_bau$dir_ghg)
      }
    )

    # check that VMT decreases
    purrr::map(
      list(
        pass_transit,
        pass_lu,
        pass_lu_int,
        pass_road,
        pass_tele,
        pass_vmt_reduction,
        pass_ctr
      ),
      function(x) {
        test_ghg <- x$vmt %>%
          filter(year == max(year)) %>%
          group_by(geog_name, year) %>%
          summarise(vmt = sum(vmt), .groups = "keep")

        testthat::expect_lte(test_ghg$vmt, pass_bau_vmt$vmt)
      }
    )
  })
}

purrr::map(
  geography_test_list,
  test_passenger
)
