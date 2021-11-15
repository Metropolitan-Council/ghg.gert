testthat::test_that("Expected number of rows", {
  t_with_drs <- adj_fleet_shares(
    .bev_pct_sales = 0,
    .phev_pct_sales = 0,
    .hev_pct_sales = 0,
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .av_pct = 0,
    .drs_pct = .10,
    .enviro_factors = enviro_factors
  )


  testthat::expect_equal(nrow(t_with_drs$pass), nrow(transportation_data$passenger))
  testthat::expect_equal(nrow(t_with_drs$freight), nrow(transportation_data$freight))

  t_with_av <- adj_fleet_shares(
    .bev_pct_sales = 0,
    .phev_pct_sales = 0,
    .hev_pct_sales = 0,
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .av_pct = .05,
    .drs_pct = 0,
    .enviro_factors = enviro_factors
  )

  testthat::expect_equal(nrow(t_with_av$freight), nrow(transportation_data$freight))
})

t_with_av <- adj_fleet_shares(
  .bev_pct_sales = 0,
  .phev_pct_sales = 0,
  .hev_pct_sales = 0,
  .pass_tb = transportation_data$passenger %>%
    filter(ctu == "St. Paul"),
  .freight_tb = transportation_data$freight %>%
    filter(ctu == "St. Paul"),
  .vmt_fee = 0,
  .payd_fee = 0,
  .gas_tax = 0,
  .av_pct = 0.05,
  .drs_pct = 0,
  .enviro_factors = enviro_factors
)

t_with_av$pass %>%
  filter(var == "AVStock")

pass_av <- t_with_av$pass %>%
  filter(
    var == "TotStock",
    mode == "PLDV"
  )


testthat::expect_equal(
  tolerance = 0.001,
  pass_av$value[1:7],
  c(
    154401.15,
    161234.142,
    165789.47,
    166827.35,
    170255.75,
    170439.518,
    169406.28
  )
)


# Stock adjustments only adjust the proportion of the total vehicles
# The total number of vehicles should NOT change


t_hev_bev_phev <- adj_fleet_shares(
  .bev_pct_sales = .20,
  .phev_pct_sales = .15,
  .hev_pct_sales = .40,
  .pass_tb = transportation_data$passenger %>%
    filter(ctu == "St. Paul"),
  .freight_tb = transportation_data$freight %>%
    filter(ctu == "St. Paul"),
  .vmt_fee = 0,
  .payd_fee = 0,
  .gas_tax = 0,
  .drs_pct = 0,
  .av_pct = 0,
  .enviro_factors = enviro_factors
)


test_total_table <- left_join(
  transportation_data$passenger %>%
    filter(
      ctu == "St. Paul",
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
  adj_fleet_shares(
    .bev_pct_sales = 1,
    .pass_tb = transportation_data$passenger %>%
      filter(ctu == "St. Paul"),
    .freight_tb = transportation_data$freight %>%
      filter(ctu == "St. Paul"),
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .drs_pct = 0,
    .av_pct = 0,
    .enviro_factors = enviro_factors
  )
)

t_hev_bev_phev$freight


freight_test_total_table <- left_join(
  transportation_data$freight %>%
    filter(
      ctu == "St. Paul",
      mode %in% c("SUT", "CUT"),
      str_detect(var, "Tot")
    ),
  t_hev_bev_phev$freight %>%
    filter(
      ctu == "St. Paul",
      mode %in% c("SUT", "CUT"),
      str_detect(var, "Tot")
    ),
  c("mode", "var", "ctu", "year", "aeo_mode", "type"),
  suffix = c(".orig", ".adj")
) %>%
  mutate(diff = round(value.orig - value.adj)) %>%
  filter(diff != 0)

testthat::expect_equal(nrow(freight_test_total_table), 0)


# if the non-alt fuel sales make up 100% of all sales, then there should
# be no CI or SI sales in the final year

no_si_ci <- adj_fleet_shares(
  .bev_pct_sales = .60,
  .phev_pct_sales = .20,
  .hev_pct_sales = .20,
  .pass_tb = transportation_data$passenger %>%
    filter(ctu == "St. Paul"),
  .freight_tb = transportation_data$freight %>%
    filter(ctu == "St. Paul"),
  .enviro_factors = enviro_factors
)

no_si_ci$pass %>%
  filter(
    mode == "PLDV",
    var == "CISales"
  )
