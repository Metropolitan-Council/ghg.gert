testthat::test_that("Transit rail length correct", {
  transit_rail <- suppressMessages(
    suppressWarnings(
      mode_transit_rail(
        .pass_tb = transportation_data$passenger %>%
          filter(geog_name == "Saint Paul"),
        .selected_ctu = "Saint Paul",
        .calc_transp_cost = TRUE,
        .calc_transp_fuel_use = TRUE,
        .calc_transp_ghg_embodied = TRUE
      )
    )
  )
  testthat::expect_length(transit_rail, 5)
})

testthat::test_that("Scandia has no rail PMT", {
  bau <- suppressMessages(suppressWarnings(mode_transit_rail(
    .selected_ctu = "Scandia"
  )))

  transit <- suppressMessages(suppressWarnings(mode_transit_rail(
    .selected_ctu = "Scandia",
    .transit_service_pct = 1,
    .pldv_avo_pct = 0.5
  )))

  vmt_summary <- bind_rows(
    bau$vmt %>%
      group_by(geog_name, year) %>%
      dplyr::summarize(vmt = sum(vmt), .groups = "keep") %>%
      mutate(scen = "bau"),
    transit$vmt %>%
      group_by(geog_name, year) %>%
      dplyr::summarize(vmt = sum(vmt), .groups = "keep") %>%
      mutate(scen = "transit")
  ) %>%
    pivot_wider(
      names_from = "scen",
      values_from = "vmt"
    )


  testthat::expect_equal(
    vmt_summary %>%
      mutate(diff = transit - bau) %>%
      magrittr::extract2("diff") %>%
      sum(),
    0
  )
})


test_rail <- function(x) {
  testthat::test_that(paste0(x, " emissions reduce with interventions"), {
    pass <- suppressMessages(
      suppressWarnings(
        mode_transit_rail(
          .pass_tb = transportation_data$passenger,
          .selected_ctu = x
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
      filter(
        year == max(year),
        mode == "RU"
      ) %>%
      group_by(geog_name, year) %>%
      summarise(dir_ghg = sum(dir_ghg), .groups = "keep")


    pass_bau_vmt <- pass$vmt %>%
      filter(
        year == max(year),
        mode == "RU"
      ) %>%
      group_by(geog_name, year) %>%
      summarise(vmt = sum(vmt), .groups = "keep")


    pass_transit <- suppressMessages(suppressWarnings(
      mode_transit_rail(
        .pass_tb = transportation_data$passenger,
        .selected_ctu = x,
        .scenario = "transit",
        .transit_service_pct = .30,
        .transit_avo_pct = 0.5
      )
    ))

    pass_lu <- suppressMessages(suppressWarnings(mode_transit_rail(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .scenario = "land_use",
      .emp_dens_pct_change = 0.10,
      .pop_dens_pct_change = 0.10,
      .transit_service_pct = .30
    )))


    pass_road <- suppressMessages(suppressWarnings(mode_transit_rail(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .scenario = "road",
      .emp_dens_pct_change = 0.10,
      .vmt_fee = 0.01,
      .pldv_avo_pct = 0.5,
      .cong_price = 0.01,
      .parking_price = 20,
      .transit_service_pct = .30
    )))


    pass_tele <- suppressMessages(suppressWarnings(mode_transit_rail(
      .pass_tb = transportation_data$passenger,
      .selected_ctu = x,
      .scenario = "telework",
      .emp_dens_pct_change = 0.10,
      .vmt_fee = 0.01,
      .telework_pct = 0.5,
      .parking_price = 20,
      .transit_service_pct = .30
    )))

    # browser()

    purrr::map(
      list(
        pass_transit,
        pass_lu,
        pass_road,
        pass_tele
      ),
      function(x) {
        # browser()

        test_vmt <- x$vmt %>%
          filter(
            year == max(year),
            mode == "RU"
          ) %>%
          group_by(geog_name, year) %>%
          summarise(vmt = sum(vmt), .groups = "keep")

        test_ghg <- x$dir_ghg %>%
          filter(
            year == max(year),
            mode == "RU"
          ) %>%
          group_by(geog_name, year) %>%
          summarise(dir_ghg = sum(dir_ghg), .groups = "keep")

        # if VMT increases, then emissions increase
        # otherwise emissions decrease
        if (test_vmt$vmt > pass_bau_vmt$vmt) {
          testthat::expect_gte(test_ghg$dir_ghg, pass_bau$dir_ghg)
        } else {
          testthat::expect_lte(test_ghg$dir_ghg, pass_bau$dir_ghg)
        }
      }
    )
  })
}

purrr::map(
  c(
    "Bloomington",
    "Saint Paul",
    "Minneapolis"
  ),
  test_rail
)
