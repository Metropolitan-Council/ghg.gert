

testthat::expect_error(
  calc_av_sales(
    tb = transportation_data$passenger,
    .av_pct = 0.05
  )
)

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


sales_w_orig_values <- calc_av_sales(
  tb = t_with_av$pass %>%
    dplyr::mutate(value = case_when(
      var == "AVStock" & year == "2025" ~ 166936.7,
      var == "AVStock" & year == "2030" ~ 170939.9,
      var == "AVStock" & year == "2035" ~ 173314.5,
      var == "AVStock" & year == "2040" ~ 175867.3,
      TRUE ~ value
    )),
  .av_pct = 0.05
)


testthat::expect_equal(
  tolerance = 0.001,
  sales_w_orig_values$value[1:7],
  c(
    0.0000,
    0.0000,
    0.0000,
    109.3856,
    238.7471,
    984.5836,
    2216.196
  )
)
