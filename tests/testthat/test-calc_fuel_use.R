
# Gasoline ------

fcm_test <- calc_fuel_cost_mile(
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
  .transit_avo_pct = 0,
  .transit_rider_pct = 0,
  .vmt_fee = 0,
  .payd_fee = 0,
  .gas_tax = 0,
  .cong_price = 0,
  .parking_price = 0,
  .freight_parking_price = 0,
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
  mutate(class = "SI")

si_fuel_use <- calc_fuel_use(
  tb_vmt = si_vmt,
  tb = st_paul_passenger,
  .mode = "PLDV",
  .fuel_type = "SI",
  .aeo_scenario = "REF",
  .miles_per_gallon = "SIMPG",
  .is_av = FALSE
)


# still need to confirm these values
testthat::expect_equal(
  si_fuel_use$fuel_use,
  c(
    59170844.497043, 59547349.0661272, 59810605.7124559, 57313473.7460134,
    57063873.2846237, 57110973.1138398, 54269603.4878194, 53405665.1978367,
    52580712.8421712
  )
)

# Diesel ------
