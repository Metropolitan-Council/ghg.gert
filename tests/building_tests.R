library(ghg.sp)

# business as usual
ghg.sp::get_demographic_baseline(tb = building_energy_data)
ghg.sp::calc_demographic_forecast(tb = building_energy_data)
ghg.sp::get_residential_energy_baseline(tb = building_energy_data)
ghg.sp::calc_residential_energy_forecast(tb = building_energy_data)
ghg.sp::get_by_ctu_non_residential_xcel_energy_baseline(tb = building_energy_data)
ghg.sp::get_by_county_non_residential_energy_baseline(tb = building_energy_data)
ghg.sp::get_statewide_non_residential_energy(tb = building_energy_data)
ghg.sp::get_non_residential_energy_baseline(tb = building_energy_data)
ghg.sp::calc_non_residential_energy_forecast(tb = building_energy_data)
ghg.sp::compile_bau_building_energy(tb = building_energy_data)

# strategies
## residential
ghg.sp::adj_unit_counts(res_tb = compile_bau_building_energy()$residential,
                        .new_homes_to_multifamily_pct = 0.50)
ghg.sp::calc_affordable_floor_area(
  res_tb  = compile_bau_building_energy()$residential,
  .single_family_floor_area_growth_pct = 0.05
)
ghg.sp::calc_floor_area_leed(
  res_tb = compile_bau_building_energy()$residential,
  .new_homes_leed_gold_pct = 0.5,
  .enviro_factors = enviro_factors
)
ghg.sp::calc_floor_area_retrofit(
  res_tb = compile_bau_building_energy()$residential,
  .existing_home_retrofit_pct = 0.80,
  .existing_home_ultra_retrofit_pct = 0.20,
  .enviro_factors = enviro_factors
)
ghg.sp::calc_floor_area_retrofit(
  res_tb = compile_bau_building_energy()$residential,
  .existing_home_retrofit_pct = 0.80,
  .existing_home_ultra_retrofit_pct = 0.20,
  .enviro_factors = enviro_factors
)
ghg.sp::calc_floor_area_behavior_change(
  res_tb = compile_bau_building_energy()$residential,
  .home_behavior_change_pct = 1.00,
  .enviro_factors = enviro_factors
)
ghg.sp::calc_ghg_residential(
  res_tb = compile_bau_building_energy()$residential,
  res_tb_bau = compile_bau_building_energy()$residential,
  .grid_decarbonization_pct = 1,
  .enviro_factors = enviro_factors
)

# non_residential

