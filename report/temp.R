test <- calc_ghg_non_residential(
  non_res_tb = building_energy_bau_data$non_residential,
  non_res_tb_bau = building_energy_bau_data$non_residential,
  .selected_ctu = "Minneapolis",
  .industrial_smart_grid_pct = 1,
  .commercial_smart_grid_pct = 1,
  .grid_decarbonization_pct = 1,
  .smart_grid_energy_reduction_pct = 1,
  .enviro_factors = enviro_factors,
  .existing_high_efficiency_buildings_pct = 0.8
)


test <- calc_electrify_commercial_heating(
  non_res_tb = ghg.sp::calc_ghg_non_residential(
    non_res_tb = building_energy_bau_data$non_residential,
    non_res_tb_bau = building_energy_bau_data$non_residential,
    .selected_ctu = "Minneapolis",
    .industrial_smart_grid_pct = 1,
    .commercial_smart_grid_pct = 1,
    .grid_decarbonization_pct = 1,
    .smart_grid_energy_reduction_pct = 1,
    .enviro_factors = enviro_factors,
    .existing_high_efficiency_buildings_pct = 0.8
  ),
  .selected_ctu = "Minneapolis",
  .grid_decarbonization_pct = 0.8,
  .electrified_buildings_pct = 0.40,
  .non_res_natural_gas_for_water_heating_pct = 0.20,
  .non_res_natural_gas_for_space_heating_pct = 0.69,
  .enviro_factors = enviro_factors
)

calc_non_res_renewable_ng(
  non_res_tb = calc_ghg_non_residential(
    non_res_tb = building_data$non_residential,
    non_res_tb_bau = building_data$non_residential,
    .selected_ctu = "Minneapolis",
    .industrial_smart_grid_pct = 1,
    .commercial_smart_grid_pct = 1,
    .grid_decarbonization_pct = 0.8,
    .smart_grid_energy_reduction_pct = 1,
    .enviro_factors = enviro_factors,
    .existing_high_efficiency_buildings_pct = 0.8
  ),
  .enviro_factors = enviro_factors,
  .selected_ctu = "Minneapolis"
)

?run_scenario_land_use()
