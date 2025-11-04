

test_stock_proportion <- function(ctu){
  testthat::test_that("Stock proportions adjust correctly", {
    # browser()


    manual_percentages <- transportation_data$passenger %>%
      filter_ctu(ctu) %>%
      filter(stringr::str_detect(var, "Stock"),
             mode == "PLDV",
             var != "TotStock") %>%
      group_by(mode, geog_name, geog_id, year) %>%
      pivot_wider(names_from = var,
                  values_from = value) %>%
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


    testthat::expect_equal(
      vmt_stock_proportion(
        .tb = transportation_data$passenger %>% filter(geog_name == ctu | geog_name == "All"),
        .mode = "BU",
        .stock = "BCIStock"
      ) %>% select(-geog_id),
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
      )
    )


    testthat::expect_equal(
      vmt_stock_proportion(
        .tb = transportation_data$passenger %>% filter(geog_name == ctu | geog_name == "All"),
        .mode = "RU",
        .stock = "EVStock"
      ) %>% select(-geog_id),
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
      )
    )

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
  list("Saint Paul",
       "Minneapolis",
       "Bloomington",
       "Lake Elmo",
       "Burnsville",
       "Apple Valley",
       "Edina",
       # Smaller CTUs don't have good rail estimates
       # "Centerville",
       # "Hanover",
       # "Birchwood Village",
       # "New Trier",
       "Twin Cities Region"),
  test_stock_proportion
)


