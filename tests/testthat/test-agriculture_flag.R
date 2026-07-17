testthat::test_that("All counties have agriculture flag set to TRUE", {
  county_flags <- agriculture_flag %>%
    dplyr::filter(geog_level == "COUNTY")

  testthat::expect_true(all(county_flags$has_ag))
  testthat::expect_equal(sum(!county_flags$has_ag), 0)
})

testthat::test_that("Major cities like Minneapolis have agriculture flag set to FALSE", {
  minneapolis <- agriculture_flag %>%
    dplyr::filter(geog_name == "Minneapolis")

  testthat::expect_equal(nrow(minneapolis), 1)
  testthat::expect_false(minneapolis$has_ag)
})

testthat::test_that("Agriculture flag dataset has expected structure", {
  testthat::expect_s3_class(agriculture_flag, "data.frame")

  expected_cols <- c(
    "geog_name",
    "geog_short_name",
    "geog_id",
    "geog_id_type",
    "geog_level",
    "has_ag"
  )

  testthat::expect_true(all(expected_cols %in% names(agriculture_flag)))
})

testthat::test_that("Agriculture flag has no missing values in key columns", {
  testthat::expect_false(any(is.na(agriculture_flag$geog_id)))
  testthat::expect_false(any(is.na(agriculture_flag$geog_level)))
  testthat::expect_false(any(is.na(agriculture_flag$has_ag)))
})

testthat::test_that("Agriculture flag has_ag column is logical type", {
  testthat::expect_type(agriculture_flag$has_ag, "logical")
})

testthat::test_that("Agriculture flag includes all geographic levels", {
  expected_levels <- c("COUNTY", "CITY", "TOWNSHIP")

  present_levels <- unique(agriculture_flag$geog_level)

  testthat::expect_true(all(expected_levels %in% present_levels))
})
