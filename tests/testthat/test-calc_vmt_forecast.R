
# Business as usual scenario testing -----

st_paul_passenger <- transportation_data$passenger %>%
  filter(ctu == "St. Paul")

# passenger si ------
si_fcm_test <- calc_fuel_cost_mile(
  st_paul_passenger,
  .mode = "PLDV",
  .aeo_scenario = "REF",
  .miles_per_gallon =  "SIMPG",
  .fuel_cost_gallon = 239.8,
  .av_pct = 0
)


si_vmt <- calc_vmt_forecast(
  .scenario = "BAU",
  tb = st_paul_passenger,
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
  dplyr::arrange(year) %>%
  dplyr::mutate(
    vmt = vmt / 10^5,
    class = "SI"
  )

testthat::test_that("BAU, Passenger gasoline correct", {
  testthat::expect_equal(
    si_vmt$vmt,
    c(c(
      0, 22.1221709054726, 0, 20.8536041006151, 20.4559473396012,
      20.1702772754979, 18.8835183369188, 18.3082802849089, 17.7590876643119
    ))
  )
})


# test walk ------

walk_vmt <- calc_vmt_forecast(
  .scenario = "BAU",
  tb = st_paul_passenger,
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
  dplyr::mutate(vmt = vmt / 10^5)



testthat::test_that("BAU walk VMT correct", {
  testthat::expect_equal(
    walk_vmt$vmt,
    # BAU values from original run
    c(
      0.5978465135, 0.6308370199,
      0.6528306909, 0.6789191471,
      0.7050076034, 0.7323857847,
      0.759763966, 0.7871421473,
      0.8145203287
    )
  )
})


# test passenger ci -----

fcm_test <- calc_fuel_cost_mile(
  st_paul_passenger,
  .mode = "PLDV",
  .aeo_scenario = "REF",
  .miles_per_gallon =  "SIMPG",
  .fuel_cost_gallon = 239.8,
  .av_pct = 0
)


ci_vmt <- calc_vmt_forecast(
  .scenario = "BAU",
  tb = st_paul_passenger,
  .mode = "PLDV",
  .stock = "CIStock",
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
  dplyr::arrange(year) %>%
  dplyr::mutate(vmt = vmt / 10^5)




testthat::test_that("Passenger, CI, BAU VMT correct", {
  testthat::expect_equal(
    ci_vmt$vmt,
    c(
      0, 0.321548660412793,
      0, 0.327578572046905,
      0.324350232511664,
      0.319227114245039,
      0.315217992720498,
      0.314529126022104,
      0.313899242344753
    )
  )
})

# rail ! -----

ru_vmt <- calc_vmt_forecast(
  .scenario = "BAU",
  tb = st_paul_passenger,
  .mode = "RU",
  .stock = "EVStock",
  .variable = "PMT",
  .tb_fuel_cost_mile = si_fcm_test,
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
  dplyr::arrange(year) %>%
  dplyr::mutate(vmt = vmt / 10^5)
