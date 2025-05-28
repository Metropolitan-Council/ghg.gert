testthat::test_that("Saint Paul land use tillage interventions", {
  current <- suppressWarnings(run_scenario_land_use(
    .selected_ctu = "Saint Paul",
    .conservation_tillage_intervention = "current_conservation_tillage"
  ))
  double <- suppressWarnings(run_scenario_land_use(
    .selected_ctu = "Saint Paul",
    .conservation_tillage_intervention = "double_conservation_tillage"
  ))
  all <- suppressWarnings(run_scenario_land_use(
    .selected_ctu = "Saint Paul",
    .conservation_tillage_intervention = "maximum_conservation_tillage"
  ))

  purrr::map(
    list(
      current,
      double,
      all
    ),
    function(x) {
      testthat::expect_equal(nrow(x), 2)
    }
  )

  together <- bind_rows(current, double, all) %>%
    filter(str_detect(var, "stock")) %>%
    select(geog_name, year, var, conservation_tillage_intervention, value) %>%
    pivot_wider(names_from = conservation_tillage_intervention, values_from = value)

  testthat::expect_lt(together$maximum_conservation_tillage, together$current_conservation_tillage)
  testthat::expect_equal(together$maximum_conservation_tillage, together$double_conservation_tillage)
})



testthat::test_that("Blaine land use tillage interventions", {
  current <- suppressWarnings(run_scenario_land_use(
    .selected_ctu = "Blaine",
    .conservation_tillage_intervention = "current_conservation_tillage"
  ))
  double <- suppressWarnings(run_scenario_land_use(
    .selected_ctu = "Blaine",
    .conservation_tillage_intervention = "double_conservation_tillage"
  ))
  all <- suppressWarnings(run_scenario_land_use(
    .selected_ctu = "Blaine",
    .conservation_tillage_intervention = "maximum_conservation_tillage"
  ))

  purrr::map(
    list(
      current,
      double,
      all
    ),
    function(x) {
      testthat::expect_equal(nrow(x), 2)
    }
  )

  together <- bind_rows(current, double, all) %>%
    filter(str_detect(var, "stock")) %>%
    select(geog_name, year, var, conservation_tillage_intervention, value) %>%
    pivot_wider(names_from = conservation_tillage_intervention, values_from = value)

  testthat::expect_lt(together$maximum_conservation_tillage, together$current_conservation_tillage)
  testthat::expect_equal(together$maximum_conservation_tillage, together$double_conservation_tillage)
})


testthat::test_that("Eagan land use tillage interventions", {
  current <- suppressWarnings(run_scenario_land_use(
    .selected_ctu = "Eagan",
    .conservation_tillage_intervention = "current_conservation_tillage"
  ))
  double <- suppressWarnings(run_scenario_land_use(
    .selected_ctu = "Eagan",
    .conservation_tillage_intervention = "double_conservation_tillage"
  ))
  all <- suppressWarnings(run_scenario_land_use(
    .selected_ctu = "Eagan",
    .conservation_tillage_intervention = "maximum_conservation_tillage"
  ))

  purrr::map(
    list(
      current,
      double,
      all
    ),
    function(x) {
      testthat::expect_equal(nrow(x), 2)
    }
  )

  together <- bind_rows(current, double, all) %>%
    filter(str_detect(var, "stock")) %>%
    select(geog_name, year, var, conservation_tillage_intervention, value) %>%
    pivot_wider(names_from = conservation_tillage_intervention, values_from = value)

  testthat::expect_lt(together$maximum_conservation_tillage, together$current_conservation_tillage)
  testthat::expect_equal(together$maximum_conservation_tillage, together$double_conservation_tillage)
})


testthat::test_that("White Bear Lake land use tillage interventions", {
  current <- suppressWarnings(run_scenario_land_use(
    .selected_ctu = "White Bear Lake",
    .conservation_tillage_intervention = "current_conservation_tillage"
  ))
  double <- suppressWarnings(run_scenario_land_use(
    .selected_ctu = "White Bear Lake",
    .conservation_tillage_intervention = "double_conservation_tillage"
  ))
  all <- suppressWarnings(run_scenario_land_use(
    .selected_ctu = "White Bear Lake",
    .conservation_tillage_intervention = "maximum_conservation_tillage"
  ))

  purrr::map(
    list(
      current,
      double,
      all
    ),
    function(x) {
      testthat::expect_equal(nrow(x), 2)
    }
  )

  together <- bind_rows(current, double, all) %>%
    filter(str_detect(var, "stock")) %>%
    select(geog_name, year, var, conservation_tillage_intervention, value) %>%
    pivot_wider(names_from = conservation_tillage_intervention, values_from = value)

  testthat::expect_lt(together$maximum_conservation_tillage, together$current_conservation_tillage)
  testthat::expect_equal(together$maximum_conservation_tillage, together$double_conservation_tillage)
})
