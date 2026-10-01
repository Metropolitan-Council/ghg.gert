test_trip_reduction <- function(x) {
  testthat::test_that(paste0(x, " CBTP adjustment returns 1 before start_year"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x)

    cbtp_adjust <- vmt_trip_reduction(
      .pass_tb = pass_tb_filtered,
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

  testthat::test_that(paste0(x, " CBTP adjustment correct reduction (uncapped)"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x)

    # Test with prop_targeted = 0.5
    # Expected: -(0.5 × 0.19 × 0.12) = -0.0114
    # cbtp_adj = 1 - 0.0114 = 0.9886
    # full_effect_year = start_year gives an instant, unramped effect
    cbtp_adjust <- vmt_trip_reduction(
      .pass_tb = pass_tb_filtered,
      .cbtp_prop_targeted = 0.5,
      .cbtp_start_year = "2025",
      .cbtp_full_effect_year = "2025",
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

  testthat::test_that(paste0(x, " CBTP adjustment capped at 2.3% maximum reduction"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x)

    # Test with prop_targeted = 1.0 (full proportion)
    # Expected uncapped: -(1.0 × 0.19 × 0.12) = -0.0228
    # But capped at -0.023, so cbtp_adj = 1 - 0.023 = 0.977
    cbtp_adjust <- vmt_trip_reduction(
      .pass_tb = pass_tb_filtered,
      .cbtp_prop_targeted = 1.0,
      .cbtp_start_year = "2025",
      .cbtp_full_effect_year = "2025",
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

  testthat::test_that(paste0(x, " Higher prop_targeted produces larger reduction"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x)

    # Create two calls with different prop_targeted values
    cbtp_low <- vmt_trip_reduction(
      .pass_tb = pass_tb_filtered,
      .cbtp_prop_targeted = 0.25,
      .cbtp_start_year = "2025",
      .cbtp_full_effect_year = "2025",
      .enviro_factors = enviro_factors
    ) %>%
      dplyr::filter(year >= "2025") %>%
      dplyr::pull(cbtp_adj) %>%
      unique()

    cbtp_high <- vmt_trip_reduction(
      .pass_tb = pass_tb_filtered,
      .cbtp_prop_targeted = 0.75,
      .cbtp_start_year = "2025",
      .cbtp_full_effect_year = "2025",
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

  testthat::test_that(paste0(x, " CBTP returns correct tibble structure"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x)

    cbtp_adjust <- vmt_trip_reduction(
      .pass_tb = pass_tb_filtered,
      .cbtp_prop_targeted = 0.5,
      .cbtp_start_year = "2025",
      .enviro_factors = enviro_factors
    )

    # Verify tibble structure
    testthat::expect_s3_class(cbtp_adjust, "tbl_df")
    testthat::expect_equal(
      nrow(cbtp_adjust),
      nrow(pass_tb_filtered %>% dplyr::select(year) %>% dplyr::distinct())
    )
  })

  testthat::test_that(paste0(x, " CBTP produces all expected years in output"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x)

    cbtp_adjust <- vmt_trip_reduction(
      .pass_tb = pass_tb_filtered,
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

  testthat::test_that(paste0(x, " CBTP with zero prop_targeted returns no effect"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x)

    cbtp_adjust <- vmt_trip_reduction(
      .pass_tb = pass_tb_filtered,
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

  testthat::test_that(paste0(x, " CBTP reaches full reduction in full_effect_year"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x)

    cbtp_adjust <- vmt_trip_reduction(
      .pass_tb = pass_tb_filtered,
      .cbtp_prop_targeted = 0.5,
      .cbtp_start_year = "2025",
      .cbtp_full_effect_year = "2045",
      .enviro_factors = enviro_factors
    )

    pre_ramp <- cbtp_adjust %>%
      dplyr::filter(year %in% c("2015", "2018", "2020")) %>%
      dplyr::pull(cbtp_adj)

    testthat::expect_equal(pre_ramp, rep(1, 3))

    at_start <- cbtp_adjust %>%
      dplyr::filter(year == "2025") %>%
      dplyr::pull(cbtp_adj)

    testthat::expect_lt(at_start, 1)

    during_ramp <- cbtp_adjust %>%
      dplyr::filter(year >= "2025", year <= "2045") %>%
      dplyr::arrange(year) %>%
      dplyr::pull(cbtp_adj)

    testthat::expect_true(all(diff(during_ramp) < 0))

    post_ramp <- cbtp_adjust %>%
      dplyr::filter(year >= "2045") %>%
      dplyr::pull(cbtp_adj)

    # expect first post-ramp year is less than at_start
    testthat::expect_lt(post_ramp[1], at_start)
    # expect last year is less than 1
    testthat::expect_lt(post_ramp[-1], 1)
  })

  testthat::test_that(paste0(x, " CBTP single-year ramp equals instant full reduction"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x)

    # start_year == full_effect_year should be instant step (no ramp)
    cbtp_adjust <- vmt_trip_reduction(
      .pass_tb = pass_tb_filtered,
      .cbtp_prop_targeted = 0.5,
      .cbtp_start_year = "2030",
      .cbtp_full_effect_year = "2030",
      .enviro_factors = enviro_factors
    )

    post_2030 <- cbtp_adjust %>%
      dplyr::filter(year >= "2030") %>%
      dplyr::pull(cbtp_adj)

    testthat::expect_lt(post_2030[1], 1)
    testthat::expect_equal(min(post_2030), 0.9886, tolerance = 0.00001)
  })

  testthat::test_that(paste0(x, " CBTP earlier full_effect_year shortens ramp"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x)

    longer_ramp_start <- vmt_trip_reduction(
      .pass_tb = pass_tb_filtered,
      .cbtp_prop_targeted = 0.5,
      .cbtp_start_year = "2025",
      .cbtp_full_effect_year = "2045",
      .enviro_factors = enviro_factors
    ) %>%
      dplyr::filter(year == "2025") %>%
      dplyr::pull(cbtp_adj)

    cbtp_adjust <- vmt_trip_reduction(
      .pass_tb = pass_tb_filtered,
      .cbtp_prop_targeted = 0.5,
      .cbtp_start_year = "2025",
      .cbtp_full_effect_year = "2027",
      .enviro_factors = enviro_factors
    )

    at_start <- cbtp_adjust %>%
      dplyr::filter(year == "2025") %>%
      dplyr::pull(cbtp_adj)

    post_ramp <- cbtp_adjust %>%
      dplyr::filter(year >= "2030") %>%
      dplyr::pull(cbtp_adj)

    testthat::expect_lt(at_start, longer_ramp_start)
    testthat::expect_lt(min(post_ramp), 1)
  })
}

purrr::map(
  geography_test_list,
  test_trip_reduction
)
