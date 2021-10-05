t_hev_bev_phev <- adj_fleet_shares(
  .bev_pct_sales = 20,
  .phev_pct_sales = 15,
  .hev_pct_sales = 40,
  .pass_tb = transportation_data$passenger,
  .freight_tb = transportation_data$freight,
  .vmt_fee = 0,
  .payd_fee = 0,
  .gas_tax = 0,
  .drs_pct_trip = 0,
  .av_pct = 0,
  .enviro_factors = enviro_factors
)

testthat::expect_equal(nrow(transportation_data$passenger), nrow(t_hev_bev_phev$pass))


t_with_drs <- adj_fleet_shares(
  .bev_pct_sales = 20,
  .phev_pct_sales = 15,
  .hev_pct_sales = 40,
  .pass_tb = transportation_data$passenger,
  .freight_tb = transportation_data$freight,
  .vmt_fee = 0,
  .payd_fee = 0,
  .gas_tax = 0,
  .av_pct = 0,
  .drs_pct_trip = 10,
  .enviro_factors = enviro_factors
)


testthat::expect_equal(nrow(transportation_data$passenger), nrow(t_with_drs$pass))
