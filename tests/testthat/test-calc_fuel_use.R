testthat::test_that("Gasoline fuel correct", {

  si_vmt <- calc_vmt_forecast(
    .scenario = "BAU",
    .selected_ctu = "Saint Paul",
    tb = st_paul_passenger,
    .mode = "PLDV",
    .stock = "SIStock",
    .variable = "PMT",
    .tb_fuel_cost_mile = si_fcm_test,
    .aeo_scenario = "REF"
  ) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(class = "SI")

  si_fuel_use <- calc_fuel_use(
    tb_vmt = si_vmt,
    tb = st_paul_passenger,
    .mode = "PLDV",
    .aeo_scenario = "REF",
    .miles_per_gallon = "SIMPG"
  )


  testthat::expect_gt(dplyr::first(si_fuel_use$fuel_use_gallons_kwh),
                      dplyr::last(si_fuel_use$fuel_use_gallons_kwh))

  testthat::expect_equal(
    # VMT is decreasing
    # fuel efficiency is increasing,
    # so lower gallons over time
    si_fuel_use$fuel_use_gallons_kwh,
    c(100913978.794603, 98332827.9556061, 96121304.0820201, 90533083.7173244,
      78959203.3353295, 66035006.7391961, 51891579.1425269, 44401177.1372552,
      39659177.6987387),
    tolerance = 0.01
  )
})

testthat::test_that("Diesel fuel correct", {
  ci_vmt <- calc_vmt_forecast(
    .scenario = "BAU",
    .selected_ctu = "all",
    tb = st_paul_passenger,
    .mode = "PLDV",
    .stock = "CIStock",
    .variable = "PMT",
    .tb_fuel_cost_mile = ci_fcm_test,
    .aeo_scenario = "REF"
  ) %>%
    dplyr::arrange(year) %>%
    mutate(class = "CI")


  ci_fuel_use <- calc_fuel_use(
    tb_vmt = ci_vmt,
    tb = st_paul_passenger,
    .mode = "PLDV",
    # .fuel_type = "CI",
    .aeo_scenario = "REF",
    .miles_per_gallon = "CIMPG"
  )


  testthat::expect_equal(
    ci_fuel_use$fuel_use_gallons_kwh,
    c(
      1007631.64849143, 1012470.37371948, 1015462.85808423, 1021632.78612439,
      1041862.90621712, 1028925.88831946, 1016713.58191866, 1018380.81644028,
      1018620.12045657
    ),
    tolerance = 0.01
  )
})

testthat::test_that("Hybrid fuel correct", {
  fcm_test_hev <- calc_fuel_cost_mile(
    st_paul_passenger,
    .mode = "PLDV",
    .aeo_scenario = "REF",
    .miles_per_gallon = "HEVMPG",
    .fuel_cost_gallon = enviro_factors$SI_FUEL_COST_GAL
  )



  hev_vmt <- calc_vmt_forecast(
    .scenario = "BAU",
    .selected_ctu = "all",
    tb = st_paul_passenger,
    .mode = "PLDV",
    .stock = "HEVStock",
    .variable = "PMT",
    .tb_fuel_cost_mile = fcm_test_hev,
    .aeo_scenario = "REF",
  ) %>%
    dplyr::arrange(year) %>%
    mutate(class = "HEV")


  hev_fuel_use <- calc_fuel_use(
    tb_vmt = hev_vmt,
    tb = st_paul_passenger,
    .mode = "PLDV",
    # .fuel_type = "SI",
    .aeo_scenario = "REF",
    .miles_per_gallon = "HEVMPG"
  )

  testthat::expect_equal(
    hev_fuel_use$fuel_use_gallons_kwh,
    c(
      77230.8973501721, 166956.872634598, 204447.61085105, 388585.252783796,
      825012.655224193, 1393645.17530972, 2044383.9828009, 2391574.71008681,
      2737695.72982986
    ),
    tolerance = 0.01
  )
})



testthat::test_that("BEV fuel correct", {
  fcm_test_bev <- calc_fuel_cost_mile(
    st_paul_passenger,
    .mode = "PLDV",
    .aeo_scenario = "REF",
    .miles_per_gallon = "BEVElec",
    .fuel_cost_gallon = enviro_factors$ELEC_FUEL_COST_KWH
  )



  bev_vmt <- calc_vmt_forecast(
    .scenario = "BAU",
    .selected_ctu = "all",
    tb = st_paul_passenger,
    .mode = "PLDV",
    .stock = "BEVStock",
    .variable = "PMT",
    .tb_fuel_cost_mile = fcm_test_bev,
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
    mutate(class = "BEV")


  bev_fuel_use <- calc_fuel_use(
    tb_vmt = bev_vmt,
    tb = st_paul_passenger,
    .mode = "PLDV",
    .aeo_scenario = "REF",
    .miles_per_gallon = "BEVElec"
  )

  # BEV increases over time
  testthat::expect_equal(
    bev_fuel_use$fuel_use_gallons_kwh,
    c(
      78357.9963079239, 1161784.67599212, 1837675.98201365, 16111989.3330469,
      14268190.7619578, 12710591.9531019, 24547070.0801707, 28816858.3206686,
      32941504.6987717
    ),
    tolerance = 0.01
  )
})


testthat::test_that("Gasoline fuel correct with strategy", {

  si_vmt <- calc_vmt_forecast(
    .scenario = "BAU",
    .selected_ctu = "Saint Paul",
    tb = transportation_data$passenger,
    .mode = "PLDV",
    .stock = "SIStock",
    .variable = "PMT",
    .tb_fuel_cost_mile = si_fcm_test,
    .vmt_fee = 0.01,
    .aeo_scenario = "REF",
    .gas_tax = 0.03,
    .parking_price = 5,
  ) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(class = "SI")

  si_fuel_use <- calc_fuel_use(
    tb_vmt = si_vmt,
    .mode = "PLDV",
    .aeo_scenario = "REF",
    .miles_per_gallon = "SIMPG"
  )


  testthat::expect_gt(dplyr::first(si_fuel_use$fuel_use_gallons_kwh),
                      dplyr::last(si_fuel_use$fuel_use_gallons_kwh))

  testthat::expect_equal(
    si_fuel_use$fuel_use_gallons_kwh,
    c(100913978.794603, 98332827.9556061, 96121304.0820201, 76213069.6862021,
      66464759.7555275, 55578791.6508259, 43668101.3971846, 37359953.47729,
      33366836.8533895),
    tolerance = 0.01
  )
})


testthat::test_that("Gasoline fuel correct with strategy, Eagan", {

  si_vmt <- calc_vmt_forecast(
    .scenario = "BAU",
    .selected_ctu = "Eagan",
    tb = transportation_data$passenger,
    .mode = "PLDV",
    .stock = "SIStock",
    .variable = "PMT",
    .tb_fuel_cost_mile = si_fcm_test,
    .vmt_fee = 0.01,
    .aeo_scenario = "REF",
    .gas_tax = 0.03,
    .parking_price = 5,
  ) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(class = "SI")

  si_fuel_use <- calc_fuel_use(
    tb_vmt = si_vmt,
    .mode = "PLDV",
    .aeo_scenario = "REF",
    .miles_per_gallon = "SIMPG"
  )


  testthat::expect_gt(dplyr::first(si_fuel_use$fuel_use_gallons_kwh),
                      dplyr::last(si_fuel_use$fuel_use_gallons_kwh))

  testthat::expect_equal(
    si_fuel_use$fuel_use_gallons_kwh,
    c(26971063.920759, 26801175.3970082, 26534061.8548255, 11487251.412213,
      10161759.830737, 8611599.095806, 6909397.84615116, 6007143.7220859,
      5449517.9969384),
    tolerance = 0.01
  )
})
