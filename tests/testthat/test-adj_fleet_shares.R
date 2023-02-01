testthat::test_that("Expected number of rows", {
  # Stock adjustments only adjust the proportion of the total vehicles
  # The total number of vehicles should NOT change


  t_hev_bev_phev <- adj_fleet_shares(
    .bev_pct_sales = .20,
    .phev_pct_sales = .15,
    .hev_pct_sales = .40,
    .pass_tb = st_paul_passenger,
    .freight_tb = st_paul_freight,
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .enviro_factors = enviro_factors
  )


  test_total_table <- left_join(
    st_paul_passenger %>%
      filter(
        mode == "PLDV",
        str_detect(var, "Tot")
      ),
    t_hev_bev_phev$pass %>%
      filter(
        mode == "PLDV",
        str_detect(var, "Tot")
      ),
    c("mode", "var", "ctu", "year", "aeo_mode", "type"),
    suffix = c(".orig", ".adj")
  ) %>%
    mutate(diff = round(value.orig - value.adj)) %>%
    filter(diff != 0)

  testthat::expect_equal(nrow(test_total_table), 0)



  testthat::expect_warning(
    suppressMessages(
      adj_fleet_shares(
        .bev_pct_sales = 1,
        .pass_tb = st_paul_passenger,
        .freight_tb = st_paul_freight %>%
          filter(ctu == "St. Paul"),
        .vmt_fee = 0,
        .payd_fee = 0,
        .gas_tax = 0,
        .enviro_factors = enviro_factors
      )
    )
  )
  # t_hev_bev_phev$freight


  freight_test_total_table <- left_join(
    st_paul_freight %>%
      filter(
        mode %in% c("SUT", "CUT"),
        str_detect(var, "Tot")
      ),
    t_hev_bev_phev$freight %>%
      filter(
        mode %in% c("SUT", "CUT"),
        str_detect(var, "Tot")
      ),
    c("mode", "var", "ctu", "year", "aeo_mode", "type"),
    suffix = c(".orig", ".adj")
  ) %>%
    mutate(diff = round(value.orig - value.adj)) %>%
    filter(diff != 0)

  testthat::expect_equal(nrow(freight_test_total_table), 0)


})
