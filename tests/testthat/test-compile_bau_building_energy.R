testthat::test_that("Minneapolis forecasts", {
  mpls_compiled <- compile_bau_building_energy(
    tb = building_energy_data,
    .selected_ctu = "Minneapolis"
  )

  testthat::expect_equal(
    mpls_compiled$residential,
    building_data$residential %>%
      filter(geog_name == "Minneapolis")
  )

  # un-comment when ready
  # testthat::expect_equal(
  #   mpls_compiled$non_residential,
  #   building_data$non_residential %>%
  #     filter(geog_name == "Minneapolis"))
})


testthat::test_that("Lake Elmo forecasts", {
  lk_el_compiled <- compile_bau_building_energy(
    tb = building_energy_data,
    .selected_ctu = "Lake Elmo"
  )

  testthat::expect_equal(
    lk_el_compiled$residential,
    building_data$residential %>%
      filter(geog_name == "Lake Elmo")
  )

  # un-comment when ready
  # testthat::expect_equal(
  #   lk_el_compiled$non_residential,
  #   building_data$non_residential %>%
  #     filter(geog_name == "Lake Elmo"))
})
