
testthat::expect_error(check_inputs("transit_avo_pct", -10))
testthat::expect_error(check_inputs("electric_scenario", "cats"))
testthat::expect_error(check_inputs("bev_pct_sales", 100))
