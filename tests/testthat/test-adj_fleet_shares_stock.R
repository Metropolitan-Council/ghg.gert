test_stock_changes <- function(ctu) {
  test_that(paste0("fleet shares adjust to stock inputs, ", ctu), {
    # browser()
    testthat::expect_error(
      adj_fleet_shares_stock(
        .bev_pct_stock = .20,
        .hev_pct_stock = .40,
        .pass_tb = transportation_data$passenger,
        .freight_tb = transportation_data$freight,
        .selected_ctu = ctu,
        .vmt_fee = 100,
        .enviro_factors = enviro_factors
      )
    )

    testthat::expect_warning(
      adj_fleet_shares_stock(
        .bev_pct_stock = 0.35,
        .hev_pct_stock = 0.60,
        .pass_tb = transportation_data$passenger,
        .freight_tb = transportation_data$freight,
        .selected_ctu = ctu,
        .enviro_factors = enviro_factors
      )
    )


    t_hev_bev <- adj_fleet_shares_stock(
      .bev_pct_stock = .20,
      .hev_pct_stock = .40,
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = ctu,
      .vmt_fee = 0,
      .payd_fee = 0,
      .gas_tax = 0,
      .enviro_factors = enviro_factors
    )


    test_total_table <- left_join(
      transportation_data$passenger %>%
        filter(
          mode == "PLDV",
          str_detect(var, "Tot")
        ),
      t_hev_bev$pass %>%
        filter(
          mode == "PLDV",
          str_detect(var, "Tot")
        ),
      by = c("mode", "var", "geog_name", "year", "aeo_mode", "type"),
      suffix = c(".orig", ".adj")
    ) %>%
      mutate(diff = round(value.orig - value.adj)) %>%
      filter(diff != 0)

    testthat::expect_equal(nrow(test_total_table), 0)

    adj_fleet_shares_stock(
      .bev_pct_stock = 1,
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight %>%
        filter(geog_name == ctu),
      .selected_ctu = ctu,
      .vmt_fee = 0,
      .payd_fee = 0,
      .gas_tax = 0,
      .enviro_factors = enviro_factors
    ) %>%
      testthat::expect_warning() %>%
      testthat::expect_warning()


    freight_test_total_table <- left_join(
      transportation_data$freight %>%
        filter(
          mode %in% c("SUT", "CUT"),
          str_detect(var, "Tot")
        ),
      t_hev_bev$freight %>%
        filter(
          mode %in% c("SUT", "CUT"),
          str_detect(var, "Tot")
        ),
      by = c("mode", "var", "geog_name", "year", "aeo_mode", "type"),
      suffix = c(".orig", ".adj")
    ) %>%
      mutate(diff = round(value.orig - value.adj)) %>%
      filter(diff != 0)

    testthat::expect_equal(nrow(freight_test_total_table), 0)

    ref_fleet <- transportation_data$passenger %>%
      filter_ctu(ctu)

    t_fleet <- adj_fleet_shares_stock(
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = ctu,
      .bev_pct_stock = 0.4,
      .enviro_factors = enviro_factors
    )

    t_fleet2 <- adj_fleet_shares_stock(
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = ctu,
      .bev_pct_stock = 0.45,
      .enviro_factors = enviro_factors
    )


    t_fleet3 <- adj_fleet_shares_stock(
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = ctu,
      .bev_pct_stock = 0.6,
      .enviro_factors = enviro_factors
    )


    compare_fleet <- function(comp_fleet) {
      expect_equal(
        ref_fleet %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "PMT"
          ) %>%
          pull(value),
        comp_fleet$pass %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "PMT"
          ) %>%
          pull(value)
      )

      # bev decrease
      expect_lt(
        ref_fleet %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "BEVStock"
          ) %>%
          pull(value),
        comp_fleet$pass %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "BEVStock"
          ) %>%
          pull(value)
      )

      # si decrease
      expect_gt(
        ref_fleet %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "SIStock"
          ) %>%
          pull(value),
        comp_fleet$pass %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "SIStock"
          ) %>%
          pull(value)
      )

      # ci decrease
      expect_gt(
        ref_fleet %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "CIStock"
          ) %>%
          pull(value),
        comp_fleet$pass %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "CIStock"
          ) %>%
          pull(value)
      )
    }

    purrr::map(
      list(
        t_fleet,
        t_fleet2,
        t_fleet3
      ),
      compare_fleet
    )
  })
}

purrr::map(
  list(
    "Saint Paul",
    "Centerville",
    "Birchwood Village",
    "New Trier",
    "Twin Cities Region",
    "Hennepin County",
    "Washington County"
  ),
  test_stock_changes
)
