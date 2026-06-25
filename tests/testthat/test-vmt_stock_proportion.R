test_stock_proportion <- function(ctu) {
  testthat::test_that(paste0(ctu, " Stock proportions adjust correctly"), {
    # browser()


    manual_percentages <- transportation_data$passenger %>%
      filter_ctu(ctu) %>%
      filter(
        stringr::str_detect(var, "Stock"),
        mode == "PLDV",
        var != "TotStock"
      ) %>%
      group_by(mode, geog_name, geog_id, year) %>%
      pivot_wider(
        names_from = var,
        values_from = value
      ) %>%
      janitor::adorn_percentages()


    testthat::expect_equal(
      vmt_stock_proportion(
        .tb = transportation_data$passenger %>% filter(geog_name == ctu | geog_name == "All"),
        .mode = "PLDV",
        .stock = "BEVStock"
      ) %>% pull(mode_stock_adj),
      manual_percentages$BEVStock,
      tolerance = 0.001
    )


    testthat::expect_equal(
      vmt_stock_proportion(
        .tb = transportation_data$passenger %>% filter(geog_name == ctu | geog_name == "All"),
        .mode = "PLDV",
        .stock = "SIStock"
      ) %>% pull(mode_stock_adj),
      manual_percentages$SIStock,
      tolerance = 0.001
    )


    testthat::expect_equal(
      vmt_stock_proportion(
        .tb = transportation_data$passenger %>% filter(geog_name == ctu | geog_name == "All"),
        .mode = "PLDV",
        .stock = "HEVStock"
      ) %>% pull(mode_stock_adj),
      manual_percentages$HEVStock,
      tolerance = 0.001
    )

    # Test BU (bus) mode - skip for CTUs without bus service (produces NaN)
    bu_result <- vmt_stock_proportion(
      .tb = transportation_data$passenger %>% filter(geog_name == ctu | geog_name == "All"),
      .mode = "BU",
      .stock = "BCIStock"
    ) %>% select(-geog_id)

    # Only test if no NaN values (CTU has complete bus service data)
    if (!any(is.nan(bu_result$mode_stock_adj))) {
      testthat::expect_equal(
        bu_result,
        tibble::tribble(
          ~geog_name, ~year, ~mode, ~mode_stock_adj,
          ctu, "2015", "BU", 1,
          ctu, "2018", "BU", 1,
          ctu, "2020", "BU", 1,
          ctu, "2025", "BU", 1,
          ctu, "2030", "BU", 1,
          ctu, "2035", "BU", 1,
          ctu, "2040", "BU", 1,
          ctu, "2045", "BU", 1,
          ctu, "2050", "BU", 1
        ),
        tolerance = 0.015 # Increased tolerance for county aggregation effects
      )
    }

    # Test RU (rail) mode - skip for CTUs without rail service (produces NaN)
    ru_result <- vmt_stock_proportion(
      .tb = transportation_data$passenger %>% filter(geog_name == ctu | geog_name == "All"),
      .mode = "RU",
      .stock = "EVStock"
    ) %>% select(-geog_id)

    # Only test if no NaN values (CTU has complete rail service data)
    if (!any(is.nan(ru_result$mode_stock_adj))) {
      testthat::expect_equal(
        ru_result,
        tibble::tribble(
          ~geog_name, ~year, ~mode, ~mode_stock_adj,
          ctu, "2015", "RU", 1,
          ctu, "2018", "RU", 1,
          ctu, "2020", "RU", 1,
          ctu, "2025", "RU", 1,
          ctu, "2030", "RU", 1,
          ctu, "2035", "RU", 1,
          ctu, "2040", "RU", 1,
          ctu, "2045", "RU", 1,
          ctu, "2050", "RU", 1
        ),
        tolerance = 0.015 # Increased tolerance for county aggregation effects
      )
    }

    testthat::expect_error(
      vmt_stock_proportion(
        .tb = transportation_data$passenger %>% filter(geog_name == ctu | geog_name == "All"),
        .mode = "BU",
        .stock = "SIStock"
      )
    )
  })
}

purrr::map(
  geography_test_list,
  test_stock_proportion
)
