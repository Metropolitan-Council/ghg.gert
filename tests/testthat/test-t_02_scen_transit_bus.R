testthat::test_that("St. Paul emissions reduce with interventions",{

  pass <- suppressMessages(
    suppressWarnings(
      scen_transit_bus(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = "St. Paul"
      )))

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
    summarise(dir_ghg = sum(dir_ghg), .groups = "keep") %>%
    left_join(pass$vmt %>%
                filter(year == "2040") %>%
                group_by(ctu, year) %>%
                summarise(vmt = sum(vmt), .groups = "keep"),
              by = c("ctu", "year"))


  pass_transit <- suppressMessages(
    suppressWarnings(
      scen_transit_bus(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = "St. Paul",
        .scenario = "transit",
        .transit_service_pct = .30,
        .transit_avo_pct = 0.5
      )))

  pass_lu <- suppressMessages(suppressWarnings(scen_transit_bus(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "St. Paul",
    .scenario = "land_use",
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10,
    .grid_decarbonization_pct = 0.8
  )))


  pass_road <- suppressMessages(suppressWarnings(scen_transit_bus(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "St. Paul",
    .scenario = "road",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .pldv_avo_pct = 0.5,
    .cong_price = 0.01,
    .parking_price = 20)))


  pass_tele <- suppressMessages(suppressWarnings(
    scen_transit_bus(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = "St. Paul",
      .scenario = "telework",
      .emp_dens_pct_change = 0.10,
      .pop_dens_pct_change = 0.10,
      .vmt_fee = 0.01,
      .telework_pct = 0.5,
      .parking_price = 20)))



  # emissions should decrease only in specific instances
  purrr::map(
    list(
      pass_transit
      # pass_lu,
      # pass_road,
      # pass_tele
    ),
    function(x) {

      test_ghg <- x$dir_ghg %>%
        filter(year == "2040") %>%
        group_by(ctu, year) %>%
        summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

      testthat::expect_lt(test_ghg$dir_ghg, pass_bau$dir_ghg)
    }
  )


  # vmt should increase
  purrr::map(
    list(
      # pass_transit,
      pass_lu,
      pass_road,
      pass_tele
    ),
    function(x) {

      test_ghg <- x$vmt %>%
        filter(year == "2040") %>%
        group_by(ctu, year) %>%
        summarise(vmt = sum(vmt), .groups = "keep")

      testthat::expect_gt(test_ghg$vmt, pass_bau$vmt)
    }
  )


})


testthat::test_that("Minneapolis emissions reduce with interventions",{

  pass <- suppressMessages(
    suppressWarnings(
      scen_transit_bus(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = "Minneapolis"
      )))

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
    summarise(dir_ghg = sum(dir_ghg), .groups = "keep") %>%
    left_join(pass$vmt %>%
                filter(year == "2040") %>%
                group_by(ctu, year) %>%
                summarise(vmt = sum(vmt), .groups = "keep"),
              by = c("ctu", "year"))


  pass_transit <- suppressMessages(
    suppressWarnings(
      scen_transit_bus(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = "Minneapolis",
        .scenario = "transit",
        .transit_service_pct = .30,
        .transit_avo_pct = 0.5
      )))

  pass_lu <- suppressMessages(suppressWarnings(scen_transit_bus(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minneapolis",
    .scenario = "land_use",
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10,
    .grid_decarbonization_pct = 0.8
  )))


  pass_road <- suppressMessages(suppressWarnings(scen_transit_bus(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minneapolis",
    .scenario = "road",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .pldv_avo_pct = 0.5,
    .cong_price = 0.01,
    .parking_price = 20)))


  pass_tele <- suppressMessages(suppressWarnings(
    scen_transit_bus(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = "Minneapolis",
      .scenario = "telework",
      .emp_dens_pct_change = 0.10,
      .pop_dens_pct_change = 0.10,
      .vmt_fee = 0.01,
      .telework_pct = 0.5,
      .parking_price = 20)))



  # emissions should decrease only in specific instances
  purrr::map(
    list(
      pass_transit
      # pass_lu,
      # pass_road,
      # pass_tele
    ),
    function(x) {

      test_ghg <- x$dir_ghg %>%
        filter(year == "2040") %>%
        group_by(ctu, year) %>%
        summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

      testthat::expect_lt(test_ghg$dir_ghg, pass_bau$dir_ghg)
    }
  )


  # vmt should increase
  purrr::map(
    list(
      # pass_transit,
      pass_lu,
      pass_road,
      pass_tele
    ),
    function(x) {

      test_ghg <- x$vmt %>%
        filter(year == "2040") %>%
        group_by(ctu, year) %>%
        summarise(vmt = sum(vmt), .groups = "keep")

      testthat::expect_gt(test_ghg$vmt, pass_bau$vmt)
    }
  )
})


testthat::test_that("Fridley emissions reduce with interventions",{

  pass <- suppressMessages(
    suppressWarnings(
      scen_transit_bus(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = "Fridley"
      )))

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
    summarise(dir_ghg = sum(dir_ghg), .groups = "keep") %>%
    left_join(pass$vmt %>%
                filter(year == "2040") %>%
                group_by(ctu, year) %>%
                summarise(vmt = sum(vmt), .groups = "keep"),
              by = c("ctu", "year"))


  pass_transit <- suppressMessages(
    suppressWarnings(
      scen_transit_bus(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = "Fridley",
        .scenario = "transit",
        .transit_service_pct = .30,
        .transit_avo_pct = 0.5
      )))

  pass_lu <- suppressMessages(suppressWarnings(scen_transit_bus(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Fridley",
    .scenario = "land_use",
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10,
    .grid_decarbonization_pct = 0.8
  )))


  pass_road <- suppressMessages(suppressWarnings(scen_transit_bus(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Fridley",
    .scenario = "road",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .pldv_avo_pct = 0.5,
    .cong_price = 0.01,
    .parking_price = 20)))


  pass_tele <- suppressMessages(suppressWarnings(
    scen_transit_bus(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = "Fridley",
      .scenario = "telework",
      .emp_dens_pct_change = 0.10,
      .pop_dens_pct_change = 0.10,
      .vmt_fee = 0.01,
      .telework_pct = 0.5,
      .parking_price = 20)))



  # emissions should decrease only in specific instances
  purrr::map(
    list(
      pass_transit
      # pass_lu,
      # pass_road,
      # pass_tele
    ),
    function(x) {

      test_ghg <- x$dir_ghg %>%
        filter(year == "2040") %>%
        group_by(ctu, year) %>%
        summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

      testthat::expect_lt(test_ghg$dir_ghg, pass_bau$dir_ghg)
    }
  )


  # vmt should increase
  purrr::map(
    list(
      # pass_transit,
      pass_lu,
      pass_road,
      pass_tele
    ),
    function(x) {

      test_ghg <- x$vmt %>%
        filter(year == "2040") %>%
        group_by(ctu, year) %>%
        summarise(vmt = sum(vmt), .groups = "keep")

      testthat::expect_gt(test_ghg$vmt, pass_bau$vmt)
    }
  )
})

