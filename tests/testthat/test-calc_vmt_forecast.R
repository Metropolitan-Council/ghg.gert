
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
  .land_use_diversity_pct_change = 0,
  .intersection_design_pct_change = 0,
  .job_access_pct_change = 0,
  .transit_dist_pct_change = 0,
  .comb_5d_impact_pct_change = 0,
  .telework_pct = 0,
  .phev_electric = FALSE
) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(
    vmt = vmt / 10^5,
    class = "SI"
  )

testthat::test_that("BAU, Passenger gasoline correct", {
  testthat::expect_equal(
    si_vmt$vmt,
    c(
      22.1801380413249, 22.1221709054726, 22.0886226208252, 20.8536041006151,
      20.4559473396012, 20.1702772754979, 18.8835183369188, 18.3082802849089,
      17.7590876643119
    )
  )
})


# walk ------

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
  .land_use_diversity_pct_change = 0,
  .intersection_design_pct_change = 0,
  .job_access_pct_change = 0,
  .transit_dist_pct_change = 0,
  .comb_5d_impact_pct_change = 0,
  .telework_pct = 0,
  .phev_electric = FALSE
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


# passenger ci -----

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
  .land_use_diversity_pct_change = 0,
  .intersection_design_pct_change = 0,
  .job_access_pct_change = 0,
  .transit_dist_pct_change = 0,
  .comb_5d_impact_pct_change = 0,
  .telework_pct = 0,
  .phev_electric = FALSE
) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(vmt = vmt / 10^5)




testthat::test_that("Passenger, CI, BAU VMT correct", {
  testthat::expect_equal(
    ci_vmt$vmt,
    c(
      0.319054791655997, 0.321548660412793, 0.323142027478914, 0.327578572046905,
      0.324350232511664, 0.319227114245039, 0.315217992720498, 0.314529126022104,
      0.313899242344753
    )
  )
})

# rail -----

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
  .land_use_diversity_pct_change = 0,
  .intersection_design_pct_change = 0,
  .job_access_pct_change = 0,
  .transit_dist_pct_change = 0,
  .comb_5d_impact_pct_change = 0,
  .telework_pct = 0,
  .phev_electric = FALSE
) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(vmt = vmt / 10^5)



# bus ci ------
si_fcm_test <- calc_fuel_cost_mile(
  st_paul_passenger,
  .mode = "PLDV",
  .aeo_scenario = "REF",
  .miles_per_gallon =  "CIMPG",
  .fuel_cost_gallon = 239.8,
  .av_pct = 0
)


bus_ci_vmt <- calc_vmt_forecast(
  .scenario = "BAU",
  tb = st_paul_passenger,
  .mode = "BU",
  .stock = "BCIStock",
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
  .land_use_diversity_pct_change = 0,
  .intersection_design_pct_change = 0,
  .job_access_pct_change = 0,
  .transit_dist_pct_change = 0,
  .comb_5d_impact_pct_change = 0,
  .telework_pct = 0,
  .phev_electric = FALSE
) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(
    vmt = vmt / 10^5,
    class = "CI"
  )

testthat::test_that("BAU, Bus diesel correct", {
  testthat::expect_equal(
    bus_ci_vmt$vmt,
    c(
      0.0432390104740045, 0.0456038078208909, 0.0471835689770316,
      0.0394933326108395, 0.0105744903611263, 0, 0, 0, 0
    )
  )
})


# plug in hybrid ------



phev_vmt <- calc_vmt_forecast(
  .scenario = "BAU",
  tb = st_paul_passenger,
  .mode = "PLDV",
  .stock = "PHEVStock",
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
  .land_use_diversity_pct_change = 0,
  .intersection_design_pct_change = 0,
  .job_access_pct_change = 0,
  .transit_dist_pct_change = 0,
  .comb_5d_impact_pct_change = 0,
  .telework_pct = 0,
  .phev_electric = FALSE
) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(
    vmt = vmt / 10^5
  )

testthat::expect_equal(
  phev_vmt$vmt,
  c(
    0.00253434752991233, 0.0287663568143962, 0.0451311636754967,
    0.488532186047875, 0.69975653043403, 0.776707638386635, 1.08225627894277,
    1.27017350051881, 1.45027225105197
  )
)


# dynamic ride share error ------

testthat::expect_error(calc_vmt_forecast(
  .scenario = "MIT",
  tb = st_paul_passenger,
  .mode = "DRS",
  .stock = "BEVStock",
  .variable = "PMT",
  .tb_fuel_cost_mile = fcm_test,
  .aeo_scenario = "REF"
))
