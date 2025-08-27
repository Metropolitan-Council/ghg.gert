testthat::test_that("Input checker correct", {
  testthat::expect_error(check_inputs("transit_avo_pct", -10))
  testthat::expect_error(check_inputs("electric_scenario", "foo"))
  testthat::expect_error(check_inputs("bev_pct_sales", 100))
  testthat::expect_error(check_inputs("aeo_scenario", "foo"))
  testthat::expect_error(check_inputs("mode", "foo"))
  testthat::expect_error(check_inputs("mode", "BRT"))
  testthat::expect_error(check_inputs("mode", "FRAIL"))
})
