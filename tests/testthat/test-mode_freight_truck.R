test_freight_truck <- function(x) {
  testthat::test_that(paste0(x, " freight truck emissions decrease with interventions"), {
    # shared BAU from setup.R avoids recomputing the default scenario
    fr <- transportation_bau_by_geog[[x]]$freight$SUT_CUT

    testthat::expect_length(fr, 2)

    testthat::expect_named(fr,
      expected = c(
        "vmt",
        "dir_ghg"
      ),
      ignore.order = TRUE
    )


    fr_bau <- fr$dir_ghg %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::group_by(geog_name, geog_id, year) %>%
      dplyr::summarise(dir_ghg = sum(dir_ghg), .groups = "keep")


    fr_adjusted <- adj_fleet_shares(
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = x,
      .vmt_fee = 0.01,
      .bev_pct_sales = 0.10
    )


    fr_transit <- suppressMessages(suppressWarnings(mode_freight_truck(
      .freight_tb = fr_adjusted$freight,
      .selected_ctu = x,
      .scenario = "transit",
      .transit_service_pct = .30,
      .transit_avo_pct = 0.5
    )))

    fr_lu <- suppressMessages(suppressWarnings(mode_freight_truck(
      .freight_tb = fr_adjusted$freight,
      .selected_ctu = x,
      .scenario = "land_use",
      .emp_dens_pct_change = 0.10,
      .pop_dens_pct_change = 0.10
    )))


    fr_road <- suppressMessages(suppressWarnings(mode_freight_truck(
      .freight_tb = fr_adjusted$freight,
      .selected_ctu = x,
      .scenario = "road",
      .emp_dens_pct_change = 0.10,
      .vmt_fee = 0.01,
      .pldv_avo_pct = 0.5,
      .cong_price = 0.01,
      .freight_parking_price = 20,
      .freight_vmt_fee = 0.02,
      .parking_price = 20
    )))


    fr_tele <- suppressMessages(suppressWarnings(mode_freight_truck(
      .freight_tb = fr_adjusted$freight,
      .selected_ctu = x,
      .scenario = "telework",
      .emp_dens_pct_change = 0.10,
      .vmt_fee = 0.01,
      .freight_parking_price = 20,
      .telework_pct = 0.5,
      .parking_price = 20
    )))


    purrr::map(
      list(
        fr_transit,
        fr_lu,
        fr_road,
        fr_tele
      ),
      function(x) {
        test_ghg <- x$dir_ghg %>%
          filter(year == max(year)) %>%
          group_by(geog_name, year) %>%
          summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

        testthat::expect_lte(test_ghg$dir_ghg, fr_bau$dir_ghg)
      }
    )
  })
}


purrr::map(
  geography_test_list,
  test_freight_truck
)
