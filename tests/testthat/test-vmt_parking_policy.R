test_parking_policy <- function(x) {
  testthat::test_that(paste0(x, " parking policy reduces VMT"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x)

    # Baseline - no parking price increase
    parking_bau <- vmt_parking_policy(
      tb = pass_tb_filtered,
      .parking_cost = parking_cost,
      .mode = "PLDV",
      .parking_price = 0,
      .enviro_factors = enviro_factors
    )

    testthat::expect_equal(nrow(parking_bau), length(unique(pass_tb_filtered$year)))

    testthat::expect_named(parking_bau,
      expected = c(
        "year",
        "geog_id",
        "geog_name",
        "park_price_adj"
      ),
      ignore.order = TRUE
    )


    testthat::expect_error(vmt_parking_policy(
      tb = pass_tb_filtered,
      .mode = "SUT",
      .freight_parking_price = 2,
      .enviro_factors = enviro_factors
    ))

    testthat::expect_error(vmt_parking_policy(
      tb = pass_tb_filtered,
      .mode = "BIKE",
      .parking_price = 1,
      .freight_parking_price = 2,
      .enviro_factors = enviro_factors
    ))

    parking_bau_final <- parking_bau %>%
      filter(year == max(year)) %>%
      pull(park_price_adj)


    parking_20 <- vmt_parking_policy(
      tb = pass_tb_filtered,
      .parking_cost = parking_cost,
      .mode = "PLDV",
      .parking_price = 20,
      .enviro_factors = enviro_factors
    )

    parking_44 <- vmt_parking_policy(
      tb = pass_tb_filtered,
      .parking_cost = parking_cost,
      .mode = "PLDV",
      .parking_price = 44,
      .enviro_factors = enviro_factors
    )

    parking_66 <- vmt_parking_policy(
      tb = pass_tb_filtered,
      .parking_cost = parking_cost,
      .mode = "PLDV",
      .parking_price = 66,
      .enviro_factors = enviro_factors
    )

    purrr::map(
      list(
        parking_20,
        parking_44,
        parking_66
      ),
      function(x) {
        test_adj <- x %>%
          filter(year == max(year)) %>%
          pull(park_price_adj)

        testthat::expect_lt(test_adj, parking_bau_final)
      }
    )
  })
}

purrr::map(
  geography_test_list,
  test_parking_policy
)

# Test maximum reduction limit
test_max_reduction_floor <- function(x) {
  testthat::test_that(paste0(x, " parking respects MAX_PARKING_REDUCTION_PCT floor"), {
    pass_tb_filtered <- transportation_data$passenger %>%
      filter(geog_name == x)

    freight_tb_filtered <- transportation_data$freight %>%
      filter(geog_name == x)

    # Get the maximum allowed reduction
    max_reduction_floor <- 1 + enviro_factors$MAX_PARKING_REDUCTION_PCT

    # Test PLDV with extremely high parking price to trigger limiter
    parking_extreme_pldv <- vmt_parking_policy(
      tb = pass_tb_filtered,
      .parking_cost = parking_cost,
      .mode = "PLDV",
      .parking_price = 1000, # Extreme price to trigger limiter
      .enviro_factors = enviro_factors
    )

    pldv_min_adj <- parking_extreme_pldv %>%
      pull(park_price_adj) %>%
      min()

    # Verify adjustment doesn't go below the floor
    expect_gte(pldv_min_adj, max_reduction_floor)

    # Only cities with non-zero parking costs will hit the exact floor
    # Free parking cities (PARK=0) will be at 1 + park_elast = 0.93 > 0.70
    current_parking <- parking_cost %>%
      filter(geog_name == x, mode == "PLDV") %>%
      pull(value)

    if (length(current_parking) > 0 && current_parking[1] > 0) {
      expect_equal(pldv_min_adj, max_reduction_floor)
    }

    # Test SUT (freight) with extremely high parking price
    parking_extreme_sut <- vmt_parking_policy(
      tb = freight_tb_filtered,
      .parking_cost = parking_cost,
      .mode = "SUT",
      .freight_parking_price = 1000, # Extreme price to trigger limiter
      .enviro_factors = enviro_factors
    )

    sut_min_adj <- parking_extreme_sut %>%
      pull(park_price_adj) %>%
      min()

    # Verify adjustment doesn't go below the floor
    expect_gte(sut_min_adj, max_reduction_floor)

    # Check if freight has non-zero parking cost
    freight_parking <- parking_cost %>%
      filter(geog_name == x, mode == "SUT") %>%
      pull(value)

    if (length(freight_parking) > 0 && freight_parking[1] > 0) {
      expect_equal(sut_min_adj, max_reduction_floor)
    }

    # Test that transit modes have ceiling (inverse of vehicle floor)
    parking_transit <- vmt_parking_policy(
      tb = pass_tb_filtered,
      .parking_cost = parking_cost,
      .mode = "BU",
      .parking_price = 1000, # Extreme price to trigger ceiling
      .enviro_factors = enviro_factors
    )

    transit_max_adj <- parking_transit %>%
      pull(park_price_adj) %>%
      max()

    # Calculate maximum allowed increase (inverse of maximum reduction)
    max_increase_ceiling <- 1 - enviro_factors$MAX_PARKING_REDUCTION_PCT

    # Verify adjustment doesn't go above the ceiling
    expect_lte(transit_max_adj, max_increase_ceiling)

    # Check if current parking cost allows testing ceiling
    current_parking <- parking_cost %>%
      filter(geog_name == x, mode == "PLDV") %>%
      pull(value)

    if (length(current_parking) > 0 && current_parking[1] > 0) {
      expect_equal(transit_max_adj, max_increase_ceiling)
    }
  })
}

purrr::map(
  geography_test_list,
  test_max_reduction_floor
)
