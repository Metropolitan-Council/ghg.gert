testthat::test_that("CBTP adjustment returns 1 (no effect) before start_year", {
  cbtp_adjust <- vmt_trip_reduction(
    .pass_tb = st_paul_passenger,
    # .households_community = 100000,
    .cbtp_prop_targeted = 0.5,
    .cbtp_start_year = "2025",
    .enviro_factors = enviro_factors
  )

  # Years before 2025 should have no effect (cbtp_adj = 1)
  pre_2025 <- cbtp_adjust %>%
    dplyr::filter(year %in% c("2015", "2018", "2020"))

  testthat::expect_equal(
    pre_2025$cbtp_adj,
    c(1, 1, 1)
  )
})


testthat::test_that("CBTP adjustment correct reduction (uncapped)", {
  # Test with prop_targeted = 0.5
  # Expected: -(0.5 × 0.19 × 0.12) = -0.0114
  # cbtp_adj = 1 - 0.0114 = 0.9886
  cbtp_adjust <- vmt_trip_reduction(
    .pass_tb = st_paul_passenger,
    # .households_community = 100000,
    .cbtp_prop_targeted = 0.5,
    .cbtp_start_year = "2025",
    .enviro_factors = enviro_factors
  )

  # Years at or after 2025 should have the reduction
  post_2025 <- cbtp_adjust %>%
    dplyr::filter(year >= "2025")

  testthat::expect_equal(
    unique(post_2025$cbtp_adj),
    0.9886,
    tolerance = 0.00001
  )
})


testthat::test_that("CBTP adjustment capped at 2.3% maximum reduction", {
  # Test with prop_targeted = 1.0 (full proportion)
  # Expected uncapped: -(1.0 × 0.19 × 0.12) = -0.0228
  # But capped at -0.023, so cbtp_adj = 1 - 0.023 = 0.977
  cbtp_adjust <- vmt_trip_reduction(
    .pass_tb = st_paul_passenger,
    # .households_community = 100000,
    .cbtp_prop_targeted = 1.0,
    .cbtp_start_year = "2025",
    .enviro_factors = enviro_factors
  )

  # Verify the capped reduction
  post_2025 <- cbtp_adjust %>%
    dplyr::filter(year >= "2025")

  testthat::expect_equal(
    unique(post_2025$cbtp_adj),
    0.977,
    tolerance = 0.001
  )
})


testthat::test_that("Higher prop_targeted produces larger reduction", {
  # Create two calls with different prop_targeted values
  cbtp_low <- vmt_trip_reduction(
    .pass_tb = st_paul_passenger,
    # .households_community = 100000,
    .cbtp_prop_targeted = 0.25,
    .cbtp_start_year = "2025",
    .enviro_factors = enviro_factors
  ) %>%
    dplyr::filter(year >= "2025") %>%
    dplyr::pull(cbtp_adj) %>%
    unique()

  cbtp_high <- vmt_trip_reduction(
    .pass_tb = st_paul_passenger,
    # .households_community = 100000,
    .cbtp_prop_targeted = 0.75,
    .cbtp_start_year = "2025",
    .enviro_factors = enviro_factors
  ) %>%
    dplyr::filter(year >= "2025") %>%
    dplyr::pull(cbtp_adj) %>%
    unique()

  # Higher prop_targeted should produce lower cbtp_adj (more reduction)
  testthat::expect_lt(cbtp_high, cbtp_low)

  # Verify the difference corresponds to correct calculations
  # Low: -(0.25 × 0.19 × 0.12) = -0.0057 → 0.9943
  # High: -(0.75 × 0.19 × 0.12) = -0.0171 → 0.9829
  testthat::expect_equal(cbtp_low, 0.9943, tolerance = 0.00001)
  testthat::expect_equal(cbtp_high, 0.9829, tolerance = 0.00001)
})


testthat::test_that("CBTP returns correct tibble structure", {
  cbtp_adjust <- vmt_trip_reduction(
    .pass_tb = st_paul_passenger,
    # .households_community = 100000,
    .cbtp_prop_targeted = 0.5,
    .cbtp_start_year = "2025",
    .enviro_factors = enviro_factors
  )

  # Verify tibble structure
  testthat::expect_s3_class(cbtp_adjust, "tbl_df")
  # testthat::expect_named(cbtp_adjust, c("year", "cbtp_adj"))
  testthat::expect_equal(
    nrow(cbtp_adjust),
    nrow(st_paul_passenger %>% dplyr::select(year) %>% dplyr::distinct())
  )
})


testthat::test_that("CBTP produces all expected years in output", {
  cbtp_adjust <- vmt_trip_reduction(
    .pass_tb = st_paul_passenger,
    # .households_community = 100000,
    .cbtp_prop_targeted = 0.5,
    .cbtp_start_year = "2025",
    .enviro_factors = enviro_factors
  )

  expected_years <- c("2015", "2018", "2020", "2025", "2030", "2035", "2040", "2045", "2050")

  testthat::expect_equal(
    sort(cbtp_adjust$year),
    sort(expected_years)
  )
})


testthat::test_that("CBTP with zero prop_targeted returns no effect", {
  cbtp_adjust <- vmt_trip_reduction(
    .pass_tb = st_paul_passenger,
    # .households_community = 100000,
    .cbtp_prop_targeted = 0,
    .cbtp_start_year = "2025",
    .enviro_factors = enviro_factors
  )

  # All years should have cbtp_adj = 1 (no reduction)
  testthat::expect_equal(
    cbtp_adjust$cbtp_adj,
    rep(1, nrow(cbtp_adjust))
  )
})
