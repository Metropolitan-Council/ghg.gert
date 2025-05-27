# Business as usual scenario testing -----
# testing values are divided by 1000 for comparison with the old Excel workbook
# passenger si ------


testthat::test_that("BAU, Passenger gasoline correct", {

  si_vmt <- calc_vmt_forecast(
    .scenario = "BAU",
    .selected_ctu = "all",
    tb = st_paul_passenger,
    .mode = "PLDV",
    .stock = "SIStock",
    .variable = "PMT",
    .tb_fuel_cost_mile = si_fcm_test,
    .aeo_scenario = "REF",
    .transit_avo_pct = 0,
    .transit_service_pct = 0,
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .cong_price = 0,
    .parking_price = 0,
    .freight_parking_price = 0,
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

  testthat::expect_equal(
    si_vmt$vmt / 1000,
    c(
      22.1801997330227, 22.1222324359409, 22.0886840579825, 20.8536621026997,
      20.4560042356456, 20.1703333769814, 18.883570859419, 18.3083312074456,
      17.7591370593277
    )
  )
})


testthat::test_that("BAU walk VMT correct", {


  walk_vmt <- calc_vmt_forecast(
    .scenario = "BAU",
    .selected_ctu = "all",
    tb = st_paul_passenger,
    .mode = "WALK",
    .stock = "",
    .variable = "PMT",
    .tb_fuel_cost_mile = si_fcm_test,
    .aeo_scenario = "REF",
    .transit_avo_pct = 0,
    .transit_service_pct = 0,
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .cong_price = 0,
    .parking_price = 0,
    .freight_parking_price = 0,
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




  testthat::expect_equal(
    walk_vmt$vmt / 1000,
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
testthat::test_that("Passenger, CI, BAU VMT correct", {

  ci_vmt <- calc_vmt_forecast(
    .scenario = "BAU",
    .selected_ctu = "all",
    tb = st_paul_passenger,
    .mode = "PLDV",
    .stock = "CIStock",
    .variable = "PMT",
    .tb_fuel_cost_mile = si_fcm_test,
    .aeo_scenario = "REF",
    .transit_avo_pct = 0,
    .transit_service_pct = 0,
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .cong_price = 0,
    .parking_price = 0,
    .freight_parking_price = 0,
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



  testthat::expect_equal(
    ci_vmt$vmt / 1000,
    c(
      0.319055679073007, 0.321549554766233, 0.323142926264136, 0.327579483171905,
      0.324351134657379, 0.319228002141346, 0.315218869465859, 0.314530000851455,
      0.31390011542215
    )
  )
})
# rail -----
testthat::test_that("Urban rail passenger vmt correct", {

  ru_vmt <- testthat::expect_no_error(calc_vmt_forecast(
    .scenario = "BAU",
    .selected_ctu = "all",
    tb = st_paul_passenger,
    .mode = "RU",
    .stock = "EVStock",
    .variable = "PMT",
    .tb_fuel_cost_mile = si_fcm_test,
    .aeo_scenario = "REF",
    .transit_avo_pct = 0,
    .transit_service_pct = 0,
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .cong_price = 0,
    .parking_price = 0,
    .freight_parking_price = 0,
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
    dplyr::mutate(vmt = vmt / 10^5))


})
# bus ci ------


testthat::test_that("BAU, Bus diesel correct", {

  bus_ci_vmt <- calc_vmt_forecast(
    .scenario = "BAU",
    .selected_ctu = "all",
    tb = st_paul_passenger,
    .mode = "BU",
    .stock = "BCIStock",
    .variable = "PMT",
    .tb_fuel_cost_mile = ci_fcm_test,
    .aeo_scenario = "REF",
    .transit_avo_pct = 0,
    .transit_service_pct = 0,
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .cong_price = 0,
    .parking_price = 0,
    .freight_parking_price = 0,
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


  testthat::expect_equal(
    bus_ci_vmt$vmt / 1000,
    c(
      0.0595668970974576, 0.0641398193961864, 0.0671884342584746,
      0.0708046407415254, 0.0744208472245763, 0.0782158267372881,
      0.08201080625, 0.0858057857521186, 0.0896007652648305
    )
  )
})


# plug in hybrid ------


testthat::test_that("PHEV passenger vmt correct", {

  phev_vmt_elec <- calc_vmt_forecast(
    .scenario = "BAU",
    .selected_ctu = "all",
    tb = st_paul_passenger,
    .mode = "PLDV",
    .stock = "PHEVStock",
    .variable = "PMT",
    .tb_fuel_cost_mile = si_fcm_test,
    .aeo_scenario = "REF",
    .transit_avo_pct = 0,
    .transit_service_pct = 0,
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .cong_price = 0,
    .parking_price = 0,
    .freight_parking_price = 0,
    .freight_vmt_fee = 0,
    .pop_dens_pct_change = 0,
    .emp_dens_pct_change = 0,
    .land_use_diversity_pct_change = 0,
    .intersection_design_pct_change = 0,
    .job_access_pct_change = 0,
    .transit_dist_pct_change = 0,
    .comb_5d_impact_pct_change = 0,
    .telework_pct = 0,
    .phev_electric = TRUE
  ) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(
      vmt = vmt / 10^5
    )

  phev_vmt_gas <- calc_vmt_forecast(
    .scenario = "BAU",
    .selected_ctu = "all",
    tb = st_paul_passenger,
    .mode = "PLDV",
    .stock = "PHEVStock",
    .variable = "PMT",
    .tb_fuel_cost_mile = si_fcm_test,
    .aeo_scenario = "REF",
    .transit_avo_pct = 0,
    .transit_service_pct = 0,
    .vmt_fee = 0,
    .payd_fee = 0,
    .gas_tax = 0,
    .cong_price = 0,
    .parking_price = 0,
    .freight_parking_price = 0,
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
    phev_vmt_gas$vmt / 1000 +
      phev_vmt_elec$vmt / 1000,
    c(
      0.00253435457893076, 0.0287664368249618, 0.0451312892030342,
      0.488533544848253, 0.699758476732484, 0.776709798716421, 1.08225928912365,
      1.27017703337147, 1.45027628483017
    )
  )

})
# dynamic ride share error ------
testthat::test_that("Dynamic ride share error", {

  testthat::expect_error(calc_vmt_forecast(
    .scenario = "MIT",
    .selected_ctu = "all",
    tb = st_paul_passenger,
    .mode = "DRS",
    .stock = "BEVStock",
    .variable = "PMT",
    .tb_fuel_cost_mile = si_fcm_test,
    .aeo_scenario = "REF"
  ))
})


# bus_ci_vmt <- calc_vmt_forecast(
#   .scenario = "bus_ci",
#   .selected_ctu = "all",
#   tb = st_paul_passenger,
#   .mode = "BU",
#   .stock = "BCIStock",
#   .variable = "PMT",
#   .tb_fuel_cost_mile = si_fcm_test,
#   .aeo_scenario = "REF",
#   .transit_avo_pct = 0,
#   .transit_service_pct = .10
# )
