testthat::test_that("calc_housing_leed", {
  expect_true(exists("calc_housing_leed")) # prevent "empty test" notification

  test_leed <- function(leed_table) {
    leed_table %>%
      dplyr::filter(
        inventory_year == 2050,
        new_units > 0,
        efficiency_description == "new_leed",
        efficiency_unit_value == 0
      ) %>%
      nrow() %>%
      testthat::expect_equal(0)
  }


  test_that("Minneapolis LEED works", {
    leed0 <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Minneapolis",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0,
      .enviro_factors = ghg.gert::enviro_factors
    ) %>%
      suppressWarnings()

    # BAU should have no change in effective units
    testthat::expect_equal(sum(leed0 %>%
      filter(efficiency_description == "new_leed") %>%
      pull(efficiency_unit_value)), 0)


    # expect error
    testthat::expect_error(calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Minneapolis",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 1.1,
      .new_mf_homes_leed_gold_pct = 1.1,
      .enviro_factors = ghg.gert::enviro_factors
    ))

    leed6 <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Minneapolis",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.6,
      .new_mf_homes_leed_gold_pct = 0.6,
      .enviro_factors = ghg.gert::enviro_factors
    )

    leed4 <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Minneapolis",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.6,
      .new_mf_homes_leed_gold_pct = 0.4,
      .enviro_factors = ghg.gert::enviro_factors
    )

    leed9 <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Minneapolis",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.8,
      .new_mf_homes_leed_gold_pct = 0.9,
      .enviro_factors = ghg.gert::enviro_factors
    )


    leed1 <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Minneapolis",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 1,
      .new_mf_homes_leed_gold_pct = 1,
      .enviro_factors = ghg.gert::enviro_factors
    )

    purrr::map(
      list(leed6, leed4, leed9, leed1),
      test_leed
    )
  })


  test_that("Blaine LEED works", {
    leed0 <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Blaine",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0,
      .enviro_factors = ghg.gert::enviro_factors
    ) %>%
      suppressWarnings()

    # BAU should have no change in effective units
    testthat::expect_equal(sum(leed0 %>%
      filter(efficiency_description == "new_leed") %>%
      pull(efficiency_unit_value)), 0)


    # expect error
    testthat::expect_error(calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Blaine",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 1.1,
      .new_mf_homes_leed_gold_pct = 1.1,
      .enviro_factors = ghg.gert::enviro_factors
    ))

    leed6 <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Blaine",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.6,
      .new_mf_homes_leed_gold_pct = 0.6,
      .enviro_factors = ghg.gert::enviro_factors
    )

    leed4 <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Blaine",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.6,
      .new_mf_homes_leed_gold_pct = 0.4,
      .enviro_factors = ghg.gert::enviro_factors
    )

    leed9 <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Blaine",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.8,
      .new_mf_homes_leed_gold_pct = 0.9,
      .enviro_factors = ghg.gert::enviro_factors
    )


    leed1 <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Blaine",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 1,
      .new_mf_homes_leed_gold_pct = 1,
      .enviro_factors = ghg.gert::enviro_factors
    )


    purrr::map(
      list(leed6, leed4, leed9, leed1),
      test_leed
    )
  })


  test_that("Willernie LEED works", {
    leed0 <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Willernie",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0,
      .enviro_factors = ghg.gert::enviro_factors
    ) %>%
      suppressWarnings()

    # BAU should have no change in effective units
    testthat::expect_equal(sum(leed0 %>%
      filter(efficiency_description == "new_leed") %>%
      pull(efficiency_unit_value)), 0)


    # expect error
    testthat::expect_error(calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Willernie",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 1.1,
      .new_mf_homes_leed_gold_pct = 1.1,
      .enviro_factors = ghg.gert::enviro_factors
    ))

    leed6 <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Willernie",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.6,
      .new_mf_homes_leed_gold_pct = 0.6,
      .enviro_factors = ghg.gert::enviro_factors
    )

    leed4 <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Willernie",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.6,
      .new_mf_homes_leed_gold_pct = 0.4,
      .enviro_factors = ghg.gert::enviro_factors
    )

    leed9 <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Willernie",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 0.8,
      .new_mf_homes_leed_gold_pct = 0.9,
      .enviro_factors = ghg.gert::enviro_factors
    )


    leed1 <- calc_housing_leed(
      res_tb = building_data$residential,
      .selected_ctu = "Willernie",
      .leed_start_year = 2025,
      .new_sf_homes_leed_gold_pct = 1,
      .new_mf_homes_leed_gold_pct = 1,
      .enviro_factors = ghg.gert::enviro_factors
    )


    purrr::map(
      list(leed6, leed4, leed9, leed1),
      test_leed
    )
  })
})
