
# t_out <- out_sum_long %>%
#   filter(mode == "PLDV",
#          class== "SI") %>%
#   arrange(year)
#
# t_out$VMT %>% dput
#
# out_sum_long %>%
#   filter(mode == "PLDV",
#          class== "CI")

fcm_test <- tibble::tribble(
  ~year, ~mode, ~var, ~ctu, ~fuel_cost_mile,
  "2015", "PLDV", "SIMPG", "All", 8.98888151338707,
  "2018", "PLDV", "SIMPG", "All", 8.9087031854957,
  "2020", "PLDV", "SIMPG", "All", 8.85604090006863,
  "2025", "PLDV", "SIMPG", "All", 8.72516345020066,
  "2030", "PLDV", "SIMPG", "All", 8.59622014714893,
  "2035", "PLDV", "SIMPG", "All", 8.46918241267415,
  "2040", "PLDV", "SIMPG", "All", 8.34402207896999,
  "2045", "PLDV", "SIMPG", "All", 8.22071140965584,
  "2050", "PLDV", "SIMPG", "All", 8.0992230643295
)



si_vmt <- calc_vmt_forecast(
  .scenario = "BAU",
  tb = transportation_data$passenger,
  .mode = "PLDV",
  .stock = "SIStock",
  .variable = "PMT",
  .tb_fuel_cost_mile = fcm_test,
  .aeo_scenario = "REF",
  .transit_avo = 0,
  .transit_rider_pct = 0,
  .vmt_fee = 0,
  .payd_fee = 0,
  .gas_tax = 0,
  .cong_price = 0,
  .parking_price = 0,
  .drs_pct = 0,
  .av_pct = 0,
  .freight_vmt_fee = 0,
  .pop_dens_pct_change = 0,
  .emp_dens_pct_change = 0,
  .land_use_pct_change = 0,
  .intersection_design_pct_change = 0,
  .job_access_pct_change = 0,
  .transit_dist_pct_change = 0,
  .comb_5d_impact_pct_change = 0,
  .telework_pct = 0,
  ch_phev = 0
) %>%
  dplyr::filter(ctu == "St. Paul") %>%
  dplyr::arrange(year)

testthat::expect_equal(
  si_vmt$vmt[1:7],
  c(
    0,
    2212217.09054726,
    0,
    2085360.41006151,
    2045594.73396012,
    2017027.72754979,
    1888351.83369188
  )
)


# test walk ------

walk_vmt <- calc_vmt_forecast(
  .scenario = "BAU",
  tb = transportation_data$passenger,
  .mode = "WALK",
  .stock = "",
  .variable = "PMT",
  .tb_fuel_cost_mile = fcm_test,
  .aeo_scenario = "REF",
  .transit_avo = 0,
  .transit_rider_pct = 0,
  .vmt_fee = 0,
  .payd_fee = 0,
  .gas_tax = 0,
  .cong_price = 0,
  .parking_price = 0,
  .drs_pct = 0,
  .av_pct = 0,
  .freight_vmt_fee = 0,
  .pop_dens_pct_change = 0,
  .emp_dens_pct_change = 0,
  .land_use_pct_change = 0,
  .intersection_design_pct_change = 0,
  .job_access_pct_change = 0,
  .transit_dist_pct_change = 0,
  .comb_5d_impact_pct_change = 0,
  .telework_pct = 0,
  ch_phev = 0
) %>%
  filter(ctu == "St. Paul")


testthat::expect_equal(
  walk_vmt$vmt,
  # BAU values from original run
  c(
    0.5978465135, 0.6308370199,
    0.6528306909, 0.6789191471, 0.7050076034,
    0.7323857847, 0.759763966, 0.7871421473,
    0.8145203287
  )
)
