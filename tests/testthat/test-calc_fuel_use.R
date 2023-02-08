# Gasoline ------

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
  mutate(class = "SI")

si_fuel_use <- calc_fuel_use(
  tb_vmt = si_vmt,
  tb = st_paul_passenger,
  .mode = "PLDV",
  # .fuel_type = "SI",
  .aeo_scenario = "REF",
  .miles_per_gallon = "SIMPG",
  .is_av = FALSE
)


# still need to confirm these values
testthat::expect_equal(
  si_fuel_use$fuel_use,
  c(
    59171006.0357739, 59547519.1594438, 59810766.7901064, 57313641.874183,
    57064028.5037486, 57111129.0775869, 54269758.6628763, 53405804.9154055,
    52580862.3042421
  )
)

# Diesel ------


ci_vmt <- calc_vmt_forecast(
  .scenario = "BAU",
  .selected_ctu = "all",
  tb = st_paul_passenger,
  .mode = "PLDV",
  .stock = "CIStock",
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
  mutate(class = "CI")


ci_fuel_use <- calc_fuel_use(
  tb_vmt = ci_vmt,
  tb = st_paul_passenger,
  .mode = "PLDV",
  # .fuel_type = "CI",
  .aeo_scenario = "REF",
  .miles_per_gallon = "CIMPG",
  .is_av = FALSE
)


testthat::expect_equal(
  ci_fuel_use$fuel_use,
  c(
    1010255.34977134, 1021206.33703609, 1028312.85224504, 1047643.41041398,
    1042505.09068065, 1031169.01560497, 1023309.83123388, 1026178.97564794,
    1029244.26335665
  )
)
# Hybrid -------



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
  .tb_fuel_cost_mile = fcm_test_dies,
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
  mutate(class = "HEV")


hev_fuel_use <- calc_fuel_use(
  tb_vmt = hev_vmt,
  tb = st_paul_passenger,
  .mode = "PLDV",
  # .fuel_type = "SI",
  .aeo_scenario = "REF",
  .miles_per_gallon = "HEVMPG",
  .is_av = FALSE
)

testthat::expect_equal(
  hev_fuel_use$fuel_use,
  c(
    169192.298596365, 470527.58136809, 720618.340731252, 1658849.35921583,
    3438342.76061775, 5771549.38269618, 8421600.99774135, 9778740.96783401,
    11049825.0818169
  )
)
