st_paul_passenger <- transportation_data$passenger %>%
  filter(ctu == "St. Paul")

fcm_test <- calc_fuel_cost_mile(
  st_paul_passenger,
  .mode = "PLDV",
  .aeo_scenario = "REF",
  .miles_per_gallon =  "SIMPG",
  .fuel_cost_gallon = 239.8 / 100,
  .av_pct = 0
)


testthat::expect_error(
  vmt_road_policy(
  .pass_tb = st_paul_passenger,
  .tb_vmt = tibble(
    year = unique(st_paul_passenger$year),
    ctu = "St. Paul",
    mode = "PLDV"
  ),
  .mode = "PLDV",
  .tb_fuel_cost_mile = fcm_test,
  .vmt_fee = 0.05,
  .payd_fee = 0.05,
  .stock = "SIStock",
  .enviro_factors = enviro_factors)
)


si_road_policy <- vmt_road_policy(
  .pass_tb = st_paul_passenger,
  .tb_vmt = tibble(
    year = unique(st_paul_passenger$year),
    ctu = "St. Paul",
    mode = "PLDV"
  ),
  .mode = "PLDV",
  .tb_fuel_cost_mile = fcm_test,
  .vmt_fee = 0.05,
  .cong_price = 0.10,
  .gas_tax = 0.94,
  .payd_fee = 0,
  .stock = "SIStock",
  .enviro_factors = enviro_factors
) %>%
  mutate(across(3:8, round, digits = 6))


test_si_road <- tibble::tribble(
  ~year,       ~ctu, ~fuel_time_cost_mile, ~payd_ins_adj, ~vmt_fee_adj, ~cong_adjust, ~cross_vmt, ~gas_adj,
  "2015", "St. Paul",             0.218029,             0,            1,            1,          0,        1,
  "2018", "St. Paul",             0.217227,             0,            1,            1,          0,        1,
  "2020", "St. Paul",               0.2167,             0,            1,            1,          0,        1,
  "2025", "St. Paul",             0.215392,             0,     0.921074,     0.994953,       0.13, 0.534782,
  "2030", "St. Paul",             0.214102,             0,     0.920599,     0.994923,       0.13, 0.531981,
  "2035", "St. Paul",             0.212832,             0,     0.920125,     0.994893,       0.13, 0.529187,
  "2040", "St. Paul",              0.21158,             0,     0.919652,     0.994862,       0.13, 0.526402,
  "2045", "St. Paul",             0.210347,             0,     0.919181,     0.994832,       0.13, 0.523626,
  "2050", "St. Paul",             0.209132,             0,     0.918712,     0.994802,       0.13, 0.520858
)


testthat::expect_equal(
  si_road_policy$fuel_time_cost_mile,
  test_si_road$fuel_time_cost_mile
)

testthat::expect_equal(
  si_road_policy$payd_ins_adj,
  test_si_road$payd_ins_adj
)

testthat::expect_equal(
  si_road_policy$vmt_fee_adj,
  test_si_road$vmt_fee_adj
)

testthat::expect_equal(
  si_road_policy$cong_adjust,
  test_si_road$cong_adjust
)

testthat::expect_equal(
  si_road_policy$gas_adj,
  test_si_road$gas_adj
)




fce <- calc_fuel_cost_mile(
  st_paul_passenger,
  .mode = "PLDV",
  .aeo_scenario = "REF",
  "BEVElec",
  enviro_factors$ELEC_FUEL_COST_KWH
)



bev_road_policy <- vmt_road_policy(
  .pass_tb = st_paul_passenger,
  .tb_vmt = tibble(
    year = unique(st_paul_passenger$year),
    ctu = "St. Paul",
    mode = "PLDV"
  ),
  .mode = "PLDV",
  .tb_fuel_cost_mile = fce,
  .vmt_fee = 0.05,
  .cong_price = 0.10,
  .payd_fee = 0.0,
  .gas_tax = 0.94,
  .stock = "BEVSock"
) %>%
  mutate(across(3:7, round, digits = 6))


testthat::expect_equal(bev_road_policy$gas_adj, c(1, 1, 1, 1, 1, 1, 1, 1, 1))
