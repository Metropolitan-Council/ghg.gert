transit_rail <- suppressMessages(
  suppressWarnings(
    scen_transit_rail(
      .pass_tb = transportation_data$passenger %>%
        filter(ctu == "St. Paul"),
      .selected_ctu = "St. Paul",
      .calc_transp_cost = TRUE,
      .calc_transp_fuel_use = TRUE,
      .calc_transp_ghg_embodied = TRUE
    )
  )
)
testthat::expect_length(transit_rail, 5)

testthat::test_that("St. Paul emissions reduce with interventions",{

  pass <- suppressMessages(
    suppressWarnings(
      scen_transit_rail(
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
    summarise(dir_ghg = sum(dir_ghg), .groups = "keep")


  pass_transit <- suppressMessages(suppressWarnings(scen_transit_rail(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "St. Paul",
    .scenario = "transit",
    .transit_service_pct = .30,
    .transit_avo_pct = 0.5
  )))

  pass_lu <- suppressMessages(suppressWarnings(scen_transit_rail(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "St. Paul",
    .scenario = "land_use",
    .emp_dens_pct_change = 0.10,
    .pop_dens_pct_change = 0.10,
    .grid_decarbonization_pct = 0.8
  )))


  pass_road <- suppressMessages(suppressWarnings(scen_transit_rail(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "St. Paul",
    .scenario = "road",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .pldv_avo_pct = 0.5,
    .cong_price = 0.01,
    .parking_price = 20)))


  pass_tele <- suppressMessages(suppressWarnings(scen_transit_rail(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "St. Paul",
    .scenario = "telework",
    .emp_dens_pct_change = 0.10,
    .vmt_fee = 0.01,
    .telework_pct = 0.5,
    .parking_price = 20)))



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
})


# testthat::test_that("Fridley emissions reduce with interventions",{
#
#   pass <- suppressMessages(suppressWarnings(scen_transit_rail(
#     .pass_tb = transportation_data$passenger,
#     .selected_ctu = "Fridley"
#   )))
#
#   testthat::expect_length(pass, 2)
#
#   testthat::expect_named(pass,
#                          expected = c(
#                            "vmt",
#                            "dir_ghg"
#                          ),
#                          ignore.order = TRUE
#   )
#
#
#   pass_bau <- pass$dir_ghg %>%
#     filter(year == "2040") %>%
#     group_by(ctu, year) %>%
#     summarise(dir_ghg = sum(dir_ghg), .groups = "keep")
#
#
#   pass_transit <- suppressMessages(suppressWarnings(scen_transit_rail(
#     .pass_tb = transportation_data$passenger,
#     .selected_ctu = "Fridley",
#     .scenario = "transit",
#     .transit_service_pct = .50,
#     .grid_decarbonization_pct = 0.8
#   )))
#
#   pass_lu <- suppressMessages(suppressWarnings(scen_transit_rail(
#     .pass_tb = transportation_data$passenger,
#     .selected_ctu = "Fridley",
#     .scenario = "land_use",
#     .emp_dens_pct_change = 0.10,
#     .pop_dens_pct_change = 0.10,
#     .transit_service_pct = 0.8,
#     .grid_decarbonization_pct = 0.8
#   )))
#
#
#   pass_road <- suppressMessages(suppressWarnings(scen_transit_rail(
#     .pass_tb = transportation_data$passenger,
#     .selected_ctu = "Fridley",
#     .scenario = "road",
#     .emp_dens_pct_change = 0.10,
#     .grid_decarbonization_pct = 0.7,
#     .vmt_fee = 0.01,
#     .pldv_avo_pct = 0.5,
#     .cong_price = 0.01,
#     .parking_price = 20)))
#
#
#   pass_tele <- suppressMessages(suppressWarnings(scen_transit_rail(
#     .pass_tb = transportation_data$passenger,
#     .selected_ctu = "Fridley",
#     .scenario = "telework",
#     .emp_dens_pct_change = 0.10,
#     .grid_decarbonization_pct = 0.8,
#     .vmt_fee = 0.01,
#     .telework_pct = 0.5,
#     .parking_price = 20)))
#
#
#
#   purrr::map(
#     list(
#       # pass_transit
#       # pass_lu
#       # pass_road
#       pass_tele
#     ),
#     function(x) {
#
#       test_ghg <- x$dir_ghg %>%
#         filter(year == "2040") %>%
#         group_by(ctu, year) %>%
#         summarise(dir_ghg = sum(dir_ghg), .groups = "keep")
#
#       testthat::expect_lt(test_ghg$dir_ghg, pass_bau$dir_ghg)
#     }
#   )
# })


testthat::test_that("Minneapolis emissions reduce with interventions",{

  pass <- suppressMessages(suppressWarnings(scen_transit_rail(
    .pass_tb = transportation_data$passenger,
    .selected_ctu = "Minneapolis"
  )))

  testthat::expect_length(pass, 2)

  testthat::expect_named(pass,
                         expected = c(
                           "vmt",
                           "dir_ghg"
                           # "emb_ghg",
                           # "fuel_use",
                           # "cost"
                         ),
                         ignore.order = TRUE
  )


  pass_bau <- pass$dir_ghg %>%
    filter(year == "2040") %>%
    group_by(ctu, year) %>%
    summarise(dir_ghg = sum(dir_ghg), .groups = "keep")


  pass_transit <- suppressMessages(suppressWarnings(
    scen_transit_rail(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = "Minneapolis",
      .scenario = "transit",
      .transit_service_pct = .30,
      .transit_avo_pct = 0.5
    )))

  pass_lu <- suppressMessages(
    suppressWarnings(
      scen_transit_rail(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = "Minneapolis",
        .scenario = "land_use",
        .emp_dens_pct_change = 0.10,
        .pop_dens_pct_change = 0.10,
        .grid_decarbonization_pct = 0.8
      )))


  pass_road <- suppressMessages(
    suppressWarnings(
      scen_transit_rail(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = "Minneapolis",
        .scenario = "road",
        .emp_dens_pct_change = 0.10,
        .vmt_fee = 0.01,
        .pldv_avo_pct = 0.5,
        .cong_price = 0.01,
        .parking_price = 20)))


  pass_tele <- suppressMessages(
    suppressWarnings(
      scen_transit_rail(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = "Minneapolis",
        .scenario = "telework",
        .emp_dens_pct_change = 0.10,
        .vmt_fee = 0.01,
        .telework_pct = 0.5)))



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
})


testthat::test_that("Scandia has no rail PMT",{

  bau <- suppressMessages(suppressWarnings(scen_transit_rail(
    .selected_ctu = "Scandia"
  )))

  transit <- suppressMessages(suppressWarnings(scen_transit_rail(
    .selected_ctu = "Scandia",
    .transit_service_pct = 1,
    .pldv_avo_pct = 0.5
  )))

  vmt_summary <- bind_rows(

    bau$vmt %>%
      group_by(ctu, year) %>%
      dplyr::summarize(vmt = sum(vmt)) %>%
      mutate(scen = "bau"),

    transit$vmt %>%
      group_by(ctu, year) %>%
      dplyr::summarize(vmt = sum(vmt)) %>%
      mutate(scen = "transit")
  ) %>%
    pivot_wider(names_from = "scen",
                values_from = "vmt")


  testthat::expect_equal(vmt_summary %>%
                           mutate(diff = transit - bau) %>%
                           magrittr::extract2("diff") %>%
                           sum(),
                         0)
})

