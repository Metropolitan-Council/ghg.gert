test_passenger <- function(x) {
  testthat::test_that(paste0(x, " emissions reduce with interventions"), {
    pass <- suppressMessages(
      mode_passenger_light_duty(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x
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
      filter(year == max(year)) %>%
      group_by(geog_name, year) %>%
      summarise(dir_ghg = sum(dir_ghg), .groups = "keep")


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
      .pop_dens_pct_change = 0.10
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

        testthat::expect_lt(test_ghg$dir_ghg, pass_bau$dir_ghg)
      }
    )
  })
}

purrr::map(
  c(
    "Arden Hills",
    "Bloomington",
    "Saint Paul",
    "Lake Elmo",
    "Minneapolis",
    "Crystal",
    "Bethel",
    "Rosemount",
    "White Bear Twp.",
    "Hennepin County",
    "Ramsey County",
    "Washington County",
    "Dakota County",
    "Anoka County",
    "Carver County",
    "Scott County"
  ),
  test_passenger
)
