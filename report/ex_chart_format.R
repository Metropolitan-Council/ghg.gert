chartformat(
  tb = building_energy_bau_data$residential, select_ctu = params$ctu_selection,
  res_tb_bau = building_energy_bau_data$residential,
  run_residential = TRUE,
  run_non_residential = FALSE,
  # affordable floor area
  .single_family_floor_area_growth_pct = 0.05,
  .new_homes_affected_pct = 1,
  # other variables
  .electrified_buildings_pct = 0,
  .new_homes_to_multifamily_pct = 0,
  .existing_high_efficiency_buildings_pct = 0,
  .home_behavior_change_pct = 0,
  .new_homes_leed_gold_pct = 0,
  .existing_home_retrofit_pct = 0,
  .existing_home_ultra_retrofit_pct = 0,
  .additional_electrified_residential_buildings_pct = 0,
  .grid_decarbonization_pct = 0,
  .enviro_factors = enviro_factors
)

args_aff_floor <- list(
  res_tb = building_energy_bau_data$residential,
  res_tb_bau = building_energy_bau_data$residential,
  run_residential = TRUE,
  run_non_residential = FALSE,
  .single_family_floor_area_growth_pct = ".x",
  .new_homes_affected_pct = 1,
  # other variables
  .electrified_buildings_pct = 0,
  .new_homes_to_multifamily_pct = 0,
  .existing_high_efficiency_buildings_pct = 0,
  .home_behavior_change_pct = 0,
  .new_homes_leed_gold_pct = 0,
  .existing_home_retrofit_pct = 0,
  .existing_home_ultra_retrofit_pct = 0,
  .additional_electrified_residential_buildings_pct = 0,
  .grid_decarbonization_pct = 0,
  .enviro_factors = enviro_factors
)

chartformatdata(
  from = 0.01, to = 0.05, by = .01, select_ctu = params$ctu_selection,
  args = args_aff_floor
)
