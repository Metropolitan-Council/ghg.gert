testthat::test_that("Saint Paul emissions constant and walk/bike vmt increase", {
  pass <- suppressMessages(
    suppressWarnings(
      mode_walk_bike(
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
    group_by(geog_name, geog_id, year) %>%
    summarise(dir_ghg = sum(dir_ghg), .groups = "keep") %>%
    left_join(
      pass$vmt %>%
        filter(year == "2040") %>%
        group_by(geog_name, geog_id, year) %>%
        summarise(vmt = sum(vmt), .groups = "keep"),
      by = c("geog_name", "geog_id", "year")
    )


  pass_transit <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Saint Paul",
    .scenario = "transit",
    .transit_service_pct = .30,
    .transit_avo_pct = 0.5
  )))

  pass_lu <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Saint Paul",
    .scenario = "land_use",
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10
  )))


  pass_road <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Saint Paul",
    .scenario = "road",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .pldv_avo_pct = 0.5,
    .cong_price = 0.01,
    .parking_price = 20
  )))


  pass_tele <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Saint Paul",
    .scenario = "telework",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .telework_pct = 0.5,
    .parking_price = 20
  )))

  pass_multi <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Saint Paul",
    .scenario = "telework",
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .parking_price = 20
  )))


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
        filter(year == "2040") %>%
        group_by(geog_name, geog_id, year) %>%
        summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

      # expect emissions to always be 0
      testthat::expect_equal(test_ghg$dir_ghg, pass_bau$dir_ghg)
    }
  )


  purrr::map(
    list(
      pass_transit,
      pass_lu,
      pass_road,
      pass_tele
    ),
    function(x) {
      test_ghg <- x$vmt %>%
        filter(year == "2040") %>%
        group_by(geog_name, geog_id, year) %>%
        summarise(vmt = sum(vmt), .groups = "keep")

      # expect walk vmt to increase or stay constant
      testthat::expect_gte(test_ghg$vmt, pass_bau$vmt)
    }
  )
})


testthat::test_that("Fridley emissions constant and walk/bike vmt increase", {
  pass <- suppressMessages(
    suppressWarnings(
      mode_walk_bike(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = "Fridley"
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
    group_by(geog_name, geog_id, year) %>%
    summarise(dir_ghg = sum(dir_ghg), .groups = "keep") %>%
    left_join(
      pass$vmt %>%
        filter(year == "2040") %>%
        group_by(geog_name, geog_id, year) %>%
        summarise(vmt = sum(vmt), .groups = "keep"),
      by = c("geog_name", "geog_id", "year")
    )


  pass_transit <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Fridley",
    .scenario = "transit",
    .transit_service_pct = .30,
    .transit_avo_pct = 0.5
  )))

  pass_lu <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Fridley",
    .scenario = "land_use",
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10
  )))


  pass_road <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Fridley",
    .scenario = "road",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .pldv_avo_pct = 0.5,
    .cong_price = 0.01,
    .parking_price = 20
  )))


  pass_tele <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Fridley",
    .scenario = "telework",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .telework_pct = 0.5,
    .parking_price = 20
  )))

  pass_multi <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Fridley",
    .scenario = "telework",
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .parking_price = 20
  )))


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
        filter(year == "2040") %>%
        group_by(geog_name, geog_id, year) %>%
        summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

      testthat::expect_equal(test_ghg$dir_ghg, pass_bau$dir_ghg)
    }
  )


  purrr::map(
    list(
      pass_transit,
      pass_lu,
      pass_road,
      pass_tele
    ),
    function(x) {
      test_ghg <- x$vmt %>%
        filter(year == "2040") %>%
        group_by(geog_name, geog_id, year) %>%
        summarise(vmt = sum(vmt), .groups = "keep")

      testthat::expect_gte(test_ghg$vmt, pass_bau$vmt)
    }
  )
})


testthat::test_that("Minnetonka emissions constant and walk/bike vmt increase", {
  pass <- suppressMessages(
    suppressWarnings(
      mode_walk_bike(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = "Minnetonka"
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
    group_by(geog_name, geog_id, year) %>%
    summarise(dir_ghg = sum(dir_ghg), .groups = "keep") %>%
    left_join(
      pass$vmt %>%
        filter(year == "2040") %>%
        group_by(geog_name, geog_id, year) %>%
        summarise(vmt = sum(vmt), .groups = "keep"),
      by = c("geog_name", "geog_id", "year")
    )


  pass_transit <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minnetonka",
    .scenario = "transit",
    .transit_service_pct = .30,
    .transit_avo_pct = 0.5
  )))

  pass_lu <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minnetonka",
    .scenario = "land_use",
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10
  )))


  pass_road <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minnetonka",
    .scenario = "road",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .pldv_avo_pct = 0.5,
    .cong_price = 0.01,
    .parking_price = 20
  )))


  pass_tele <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minnetonka",
    .scenario = "telework",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .telework_pct = 0.5,
    .parking_price = 20
  )))

  pass_multi <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minnetonka",
    .scenario = "telework",
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .parking_price = 20
  )))


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
        filter(year == "2040") %>%
        group_by(geog_name, geog_id, year) %>%
        summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

      testthat::expect_equal(test_ghg$dir_ghg, pass_bau$dir_ghg)
    }
  )


  purrr::map(
    list(
      pass_transit,
      pass_lu,
      pass_road,
      pass_tele
    ),
    function(x) {
      test_ghg <- x$vmt %>%
        filter(year == "2040") %>%
        group_by(geog_name, geog_id, year) %>%
        summarise(vmt = sum(vmt), .groups = "keep")

      testthat::expect_gte(test_ghg$vmt, pass_bau$vmt)
    }
  )
})

testthat::test_that("South Saint Paul emissions constant and walk/bike vmt increase", {
  pass <- suppressMessages(
    suppressWarnings(
      mode_walk_bike(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = "South Saint Paul"
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
    group_by(geog_name, geog_id, year) %>%
    summarise(dir_ghg = sum(dir_ghg), .groups = "keep") %>%
    left_join(
      pass$vmt %>%
        filter(year == "2040") %>%
        group_by(geog_name, geog_id, year),
      by = c("geog_name", "geog_id", "year")
    )


  pass_transit <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "South Saint Paul",
    .scenario = "transit",
    .transit_service_pct = .30,
    .transit_avo_pct = 0.5
  )))

  pass_lu <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "South Saint Paul",
    .scenario = "land_use",
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10
  )))


  pass_road <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "South Saint Paul",
    .scenario = "road",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .pldv_avo_pct = 0.5,
    .cong_price = 0.01,
    .parking_price = 20
  )))


  pass_tele <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "South Saint Paul",
    .scenario = "telework",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .telework_pct = 0.5,
    .parking_price = 20
  )))

  pass_multi <- suppressMessages(suppressWarnings(mode_walk_bike(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "South Saint Paul",
    .scenario = "telework",
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .parking_price = 20
  )))


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
        filter(year == "2040") %>%
        group_by(geog_name, geog_id, mode, year) %>%
        summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

      testthat::expect_equal(test_ghg$dir_ghg, pass_bau$dir_ghg)
    }
  )


  purrr::map(
    list(
      pass_transit,
      pass_lu,
      pass_road,
      pass_tele
    ),
    function(x) {
      test_ghg <- x$vmt %>%
        filter(year == "2040") %>%
        group_by(geog_name, geog_id, mode, year) %>%
        summarise(vmt = sum(vmt), .groups = "keep")

      testthat::expect_gte(test_ghg$vmt[1], pass_bau$vmt[1])
      testthat::expect_gte(test_ghg$vmt[2], pass_bau$vmt[2])
    }
  )
})
