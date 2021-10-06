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
  .av_pct = .5,
  .drs_pct_trip = 0,
  .enviro_factors = enviro_factors
)

testthat::expect_equal(nrow(t_with_av$pass), nrow(transportation_data$passenger))
testthat::expect_equal(nrow(t_with_av$freight), nrow(transportation_data$freight))
