test_fleet_shares <- function(x) {
  testthat::test_that(paste0("Expected changes, ", x), {
    # Stock adjustments only adjust the proportion of the total vehicles
    # The total number of vehicles should NOT change

    testthat::expect_error(
      adj_fleet_shares(
        .bev_pct_sales = .20,
        .hev_pct_sales = .40,
        .pass_tb = transportation_data$passenger,
        .freight_tb = transportation_data$freight,
        .selected_ctu = x,
        .vmt_fee = 100,
        .enviro_factors = enviro_factors
      )
    )


    testthat::expect_error(
      adj_fleet_shares(
        .bev_pct_sales = .20,
        .hev_pct_sales = .40,
        .pass_tb = transportation_data$passenger,
        .freight_tb = transportation_data$freight,
        .selected_ctu = x,
        .vmt_fee = 0.1,
        .payd_fee = 0.1,
        .enviro_factors = enviro_factors
      )
    )


    testthat::expect_warning(
      adj_fleet_shares(
        .bev_pct_sales = 0.35,
        .hev_pct_sales = 0.60,
        .pass_tb = transportation_data$passenger,
        .freight_tb = transportation_data$freight,
        .selected_ctu = x,
        .enviro_factors = enviro_factors
      )
    )


    t_hev_bev <- adj_fleet_shares(
      .bev_pct_sales = .20,
      .hev_pct_sales = .40,
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = x,
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
      mutate(diff = abs(value.orig - value.adj) > 1) %>%
      filter(diff)

    testthat::expect_equal(nrow(test_total_table), 0)


    testthat::expect_warning(
      suppressMessages(
        adj_fleet_shares(
          .bev_pct_sales = 1,
          .pass_tb = transportation_data$passenger,
          .freight_tb = transportation_data$freight %>%
            filter(geog_name == x),
          .selected_ctu = x,
          .vmt_fee = 0,
          .payd_fee = 0,
          .gas_tax = 0,
          .enviro_factors = enviro_factors
        )
      )
    )
    # t_hev_bev$freight


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
  })
}


purrr::map(
  geography_test_list,
  test_fleet_shares
)


test_fleet_shares_pricing <- function(x) {
  test_that(paste0("fleet shares adjust to pricing inputs, ", x), {
    ref_fleet <- transportation_data$passenger %>%
      filter_ctu(x)

    t_fleet <- adj_fleet_shares(
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = x,
      .vmt_fee = 0.01,
      .enviro_factors = enviro_factors
    )

    t_fleet2 <- adj_fleet_shares(
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = x,
      .vmt_fee = 0.05,
      .enviro_factors = enviro_factors
    )


    t_fleet3 <- adj_fleet_shares(
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = x,
      .vmt_fee = 0.02,
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
      expect_gt(
        ref_fleet %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "BEVExist"
          ) %>%
          pull(value),
        comp_fleet$pass %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "BEVExist"
          ) %>%
          pull(value)
      )

      # si decrease
      expect_gt(
        ref_fleet %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "SIExist"
          ) %>%
          pull(value),
        comp_fleet$pass %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "SIExist"
          ) %>%
          pull(value)
      )

      # ci decrease
      expect_gt(
        ref_fleet %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "CIExist"
          ) %>%
          pull(value),
        comp_fleet$pass %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "CIExist"
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
  geography_test_list,
  test_fleet_shares_pricing
)

test_fleet_shares_pricing_bev <- function(x) {
  test_that(paste0("Fleet shares adjust to pricing and BEV inputs, ", x), {
    ref_fleet <- transportation_data$passenger %>%
      filter_ctu(x)

    t_fleet <- adj_fleet_shares(
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = x,
      .vmt_fee = 0.01,
      .bev_pct_sales = 0.10,
      .enviro_factors = enviro_factors
    )

    t_fleet2 <- adj_fleet_shares(
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = x,
      .vmt_fee = 0.05,
      .bev_pct_sales = 0.01,
      .enviro_factors = enviro_factors
    )


    t_fleet3 <- adj_fleet_shares(
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = x,
      .vmt_fee = 0.02,
      .bev_pct_sales = 0.10,
      .enviro_factors = enviro_factors
    )

    t_fleet4 <- adj_fleet_shares(
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = x,
      .vmt_fee = 0.2,
      .bev_pct_sales = 0.2,
      .enviro_factors = enviro_factors
    )


    testthat::expect_error(
      adj_fleet_shares(
        .pass_tb = transportation_data$passenger,
        .freight_tb = transportation_data$freight,
        .selected_ctu = x,
        .vmt_fee = 2,
        .bev_pct_sales = 0.6,
        .enviro_factors = enviro_factors
      )
    )


    t_fleet5 <- adj_fleet_shares(
      .pass_tb = transportation_data$passenger,
      .freight_tb = transportation_data$freight,
      .selected_ctu = x,
      .vmt_fee = 1,
      .bev_pct_sales = 0.6,
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
      expect_gt(
        ref_fleet %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "BEVExist"
          ) %>%
          pull(value),
        comp_fleet$pass %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "BEVExist"
          ) %>%
          pull(value)
      )

      # si decrease
      expect_gt(
        ref_fleet %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "SIExist"
          ) %>%
          pull(value),
        comp_fleet$pass %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "SIExist"
          ) %>%
          pull(value)
      )

      # ci decrease
      expect_gt(
        ref_fleet %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "CIExist"
          ) %>%
          pull(value),
        comp_fleet$pass %>%
          filter(
            year == "2050",
            mode == "PLDV",
            var == "CIExist"
          ) %>%
          pull(value)
      )
    }

    purrr::map(
      list(
        t_fleet,
        t_fleet2,
        t_fleet3,
        t_fleet4,
        t_fleet5
      ),
      compare_fleet
    )
  })
}

purrr::map(
  geography_test_list,
  test_fleet_shares_pricing_bev
)
