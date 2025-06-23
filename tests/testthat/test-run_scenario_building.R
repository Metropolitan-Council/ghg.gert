test_that("run_scenario_building works, Minneapolis", {
  testthat::capture_warnings(
    run_scenario_building(
      .selected_ctu = "Minneapolis"
    )
  ) %>%
    length() %>%
    testthat::expect_equal(4)


  testthat::expect_no_warning(
    run_scenario_building(
      .selected_ctu = "Minneapolis",
      .new_sf_homes_leed_gold_pct = 0.4,
      .new_mf_homes_leed_gold_pct = 0.3,
      .existing_mf_retrofit_pct = 0.5,
      .existing_sf_retrofit_pct = 0.2,
      .smart_grid_energy_reduction_pct = 0.1
    )
  )



  testthat::expect_no_error(
    run_scenario_building(
      .selected_ctu = "Minneapolis",
      .sf_heat_pump_pct = 0.3,
      .mf_heat_pump_pct = 0.2,
    ) %>%
      suppressWarnings()
  )
})


test_that("run_scenario_building works, Lake Elmo", {
  testthat::capture_warnings(
    run_scenario_building(
      .selected_ctu = "Lake Elmo"
    )
  ) %>%
    length() %>%
    testthat::expect_equal(4)


  testthat::expect_no_warning(
    run_scenario_building(
      .selected_ctu = "Lake Elmo",
      .new_sf_homes_leed_gold_pct = 0.4,
      .new_mf_homes_leed_gold_pct = 0.3,
      .existing_mf_retrofit_pct = 0.5,
      .existing_sf_retrofit_pct = 0.2,
      .smart_grid_energy_reduction_pct = 0.1
    )
  )



  testthat::expect_no_error(
    run_scenario_building(
      .selected_ctu = "Lake Elmo",
      .sf_heat_pump_pct = 0.3,
      .mf_heat_pump_pct = 0.2,
    ) %>%
      suppressWarnings()
  )
})


test_that("run_scenario_building works, Inver Grove Heights", {
  testthat::capture_warnings(
    run_scenario_building(
      .selected_ctu = "Inver Grove Heights"
    )
  ) %>%
    length() %>%
    testthat::expect_equal(4)


  testthat::expect_no_warning(
    run_scenario_building(
      .selected_ctu = "Inver Grove Heights",
      .new_sf_homes_leed_gold_pct = 0.4,
      .new_mf_homes_leed_gold_pct = 0.3,
      .existing_mf_retrofit_pct = 0.5,
      .existing_sf_retrofit_pct = 0.2,
      .smart_grid_energy_reduction_pct = 0.1
    )
  )



  testthat::expect_no_error(
    run_scenario_building(
      .selected_ctu = "Inver Grove Heights",
      .sf_heat_pump_pct = 0.3,
      .mf_heat_pump_pct = 0.2,
    ) %>%
      suppressWarnings()
  )
})


test_that("run_scenario_building works, Waterford Twp.", {
  testthat::capture_warnings(
    run_scenario_building(
      .selected_ctu = "Waterford Twp."
    )
  ) %>%
    length() %>%
    testthat::expect_equal(4)


  testthat::expect_no_warning(
    run_scenario_building(
      .selected_ctu = "Waterford Twp.",
      .new_sf_homes_leed_gold_pct = 0.4,
      .new_mf_homes_leed_gold_pct = 0.3,
      .existing_mf_retrofit_pct = 0.5,
      .existing_sf_retrofit_pct = 0.2,
      .smart_grid_energy_reduction_pct = 0.1
    )
  )



  testthat::expect_no_error(
    run_scenario_building(
      .selected_ctu = "Waterford Twp.",
      .sf_heat_pump_pct = 0.3,
      .mf_heat_pump_pct = 0.2,
    ) %>%
      suppressWarnings()
  )
})
