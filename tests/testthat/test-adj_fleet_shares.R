testthat::test_that("Expected number of rows", {
  t_hev_bev_phev <- adj_fleet_shares(
    .bev_pct_sales = .20,
    .phev_pct_sales = .15,
    .hev_pct_sales = .40,
    .pass_tb = transportation_data$passenger,
    .freight_tb = transportation_data$freight,
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .drs_pct_trip = 0,
    .av_pct = 0,
    .enviro_factors = enviro_factors
  )

  testthat::expect_equal(nrow(t_hev_bev_phev$pass), nrow(transportation_data$passenger))
  testthat::expect_equal(nrow(t_hev_bev_phev$freight), nrow(transportation_data$freight))


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
    .drs_pct_trip = .10,
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
    .drs_pct_trip = 0,
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
  .drs_pct_trip = 0,
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
  .drs_pct_trip = 0,
  .av_pct = 0,
  .enviro_factors = enviro_factors
)


t_hev_bev_phev$pass %>%
  filter(
    mode == "PLDV",
    var == "SISales"
  )
