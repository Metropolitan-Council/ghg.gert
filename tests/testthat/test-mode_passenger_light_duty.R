testthat::test_that("St. Paul emissions reduce with interventions", {
  pass <- suppressMessages(
    mode_passenger_light_duty(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = "St. Paul"
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
    group_by(ctu, year) %>%
    summarise(dir_ghg = sum(dir_ghg), .groups = "keep")


  pass_transit <- suppressMessages(mode_passenger_light_duty(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "St. Paul",
    .scenario = "transit",
    .transit_service_pct = .30
  ))

  pass_lu <- suppressMessages(mode_passenger_light_duty(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "St. Paul",
    .scenario = "land_use",
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10
  ))


  pass_road <- suppressMessages(mode_passenger_light_duty(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "St. Paul",
    .scenario = "road",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .pldv_avo_pct = 0.5,
    .cong_price = 0.01,
    .parking_price = 20
  ))


  pass_tele <- suppressMessages(mode_passenger_light_duty(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "St. Paul",
    .scenario = "telework",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .telework_pct = 0.5,
    .parking_price = 20
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
        group_by(ctu, year) %>%
        summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

      testthat::expect_lt(test_ghg$dir_ghg, pass_bau$dir_ghg)
    }
  )
})


testthat::test_that("Lake Elmo emissions reduce with interventions", {
  pass <- suppressMessages(mode_passenger_light_duty(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Lake Elmo",
    .calc_transp_cost = TRUE,
    .calc_transp_fuel_use = TRUE,
    .calc_transp_ghg_embodied = TRUE
  ))

  testthat::expect_length(pass, 5)

  testthat::expect_named(pass,
                         expected = c(
                           "vmt",
                           "dir_ghg",
                           "emb_ghg",
                           "fuel_use_gallons_kwh",
                           "cost"
                         ),
                         ignore.order = TRUE
  )


  pass_bau <- pass$dir_ghg %>%
    filter(year == "2040") %>%
    group_by(ctu, year) %>%
    summarise(dir_ghg = sum(dir_ghg), .groups = "keep")


  pass_transit <- suppressMessages(mode_passenger_light_duty(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Lake Elmo",
    .scenario = "transit",
    .calc_transp_cost = FALSE,
    .calc_transp_fuel_use = FALSE,
    .calc_transp_ghg_embodied = FALSE,
    .transit_service_pct = .30
  ))

  pass_lu <- suppressMessages(mode_passenger_light_duty(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Lake Elmo",
    .scenario = "land_use",
    .calc_transp_cost = FALSE,
    .calc_transp_fuel_use = FALSE,
    .calc_transp_ghg_embodied = FALSE,
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10
  ))


  pass_road <- suppressMessages(mode_passenger_light_duty(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Lake Elmo",
    .scenario = "road",
    .calc_transp_cost = FALSE,
    .calc_transp_fuel_use = FALSE,
    .calc_transp_ghg_embodied = FALSE,
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .pldv_avo_pct = 0.5,
    .cong_price = 0.01,
    .parking_price = 20
  ))


  pass_tele <- suppressMessages(mode_passenger_light_duty(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Lake Elmo",
    .scenario = "telework",
    .calc_transp_cost = FALSE,
    .calc_transp_fuel_use = FALSE,
    .calc_transp_ghg_embodied = FALSE,
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .telework_pct = 0.5,
    .parking_price = 20
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
        group_by(ctu, year) %>%
        summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

      testthat::expect_lt(test_ghg$dir_ghg, pass_bau$dir_ghg)
    }
  )
})


testthat::test_that("Minneapolis emissions reduce with interventions", {
  pass <- suppressMessages(mode_passenger_light_duty(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minneapolis",
    .calc_transp_cost = FALSE,
    .calc_transp_fuel_use = FALSE,
    .calc_transp_ghg_embodied = FALSE
  ))

  testthat::expect_length(pass, 2)

  testthat::expect_named(pass,
                         expected = c(
                           "vmt",
                           "dir_ghg"
                           # "emb_ghg",
                           # "fuel_use_gallons_kwh",
                           # "cost"
                         ),
                         ignore.order = TRUE
  )


  pass_bau <- pass$dir_ghg %>%
    filter(year == "2040") %>%
    group_by(ctu, year) %>%
    summarise(dir_ghg = sum(dir_ghg), .groups = "keep")


  pass_transit <- suppressMessages(mode_passenger_light_duty(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minneapolis",
    .scenario = "transit",
    .calc_transp_cost = FALSE,
    .calc_transp_fuel_use = FALSE,
    .calc_transp_ghg_embodied = FALSE,
    .transit_service_pct = .30,
    .transit_avo_pct = 0.5
  ))

  pass_lu <- suppressMessages(mode_passenger_light_duty(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minneapolis",
    .scenario = "land_use",
    .calc_transp_cost = FALSE,
    .calc_transp_fuel_use = FALSE,
    .calc_transp_ghg_embodied = FALSE,
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10
  ))


  pass_road <- suppressMessages(mode_passenger_light_duty(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minneapolis",
    .scenario = "road",
    .calc_transp_cost = FALSE,
    .calc_transp_fuel_use = FALSE,
    .calc_transp_ghg_embodied = FALSE,
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .pldv_avo_pct = 0.5,
    .cong_price = 0.01,
    .parking_price = 20
  ))


  pass_tele <- suppressMessages(mode_passenger_light_duty(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minneapolis",
    .scenario = "telework",
    .calc_transp_cost = FALSE,
    .calc_transp_fuel_use = FALSE,
    .calc_transp_ghg_embodied = FALSE,
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .telework_pct = 0.5
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
        group_by(ctu, year) %>%
        summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

      testthat::expect_lte(test_ghg$dir_ghg, pass_bau$dir_ghg)
    }
  )
})
