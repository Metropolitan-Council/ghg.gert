testthat::test_that("filter geog_name", {
  testthat::expect_error(filter_ctu(tibble(), .selected_ctu = "Minneaopolis"))
})
