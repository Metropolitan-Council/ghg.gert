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
