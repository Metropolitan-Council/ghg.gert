testthat::test_that("filter ctu", {
  testthat::expect_error(filter_ctu(tibble(), .selected_ctu = "Minneaopolis"))
})
