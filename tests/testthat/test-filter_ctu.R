testthat::test_that("filter ctu", {

  testhat::expect_error(filter_ctu(tibble(), .selected_ctu = "Minneaopolis"))
})
