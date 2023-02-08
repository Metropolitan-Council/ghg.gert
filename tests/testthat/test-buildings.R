# business as usual
# instead of running all these function we use the .rdata file: "building_energy_bau_data"

get_demographic_baseline(tb = building_energy_data)

calc_demographic_forecast(tb = building_energy_data)

get_residential_energy_baseline(tb = building_energy_data)

calc_residential_energy_forecast(tb = building_energy_data)

get_by_ctu_non_residential_xcel_energy_baseline(tb = building_energy_data)

get_by_county_non_residential_energy_baseline(tb = building_energy_data)

get_statewide_non_residential_energy(tb = building_energy_data)

get_non_residential_energy_baseline(tb = building_energy_data)

calc_non_residential_energy_forecast(tb = building_energy_data)

compile_bau_building_energy(tb = building_energy_data)

# strategies
## residential
adj_unit_counts(
  res_tb = building_energy_bau_data$residential,
  .new_homes_to_multifamily_pct = 0.50
)

calc_affordable_floor_area(
  res_tb = building_energy_bau_data$residential,
  .selected_ctu = "all",
  .single_family_floor_area_growth_pct = 0.05
)

calc_floor_area_leed(
  res_tb = building_energy_bau_data$residential,
  .selected_ctu = "all",
  .new_homes_leed_gold_pct = 0.5,
  .enviro_factors = enviro_factors
)

calc_floor_area_retrofit(
  res_tb = building_energy_bau_data$residential,
  .selected_ctu = "all",
  .existing_home_retrofit_pct = 0.80,
  .existing_home_ultra_retrofit_pct = 0.20,
  .enviro_factors = enviro_factors
)

calc_floor_area_retrofit(
  res_tb = building_energy_bau_data$residential,
  .selected_ctu = "all",
  .existing_home_retrofit_pct = 0.80,
  .existing_home_ultra_retrofit_pct = 0.20,
  .enviro_factors = enviro_factors
)

calc_floor_area_behavior_change(
  res_tb = building_energy_bau_data$residential,
  .selected_ctu = "all",
  .home_behavior_change_pct = 1.00,
  .enviro_factors = enviro_factors
)

calc_ghg_residential(
  res_tb = building_energy_bau_data$residential,
  res_tb_bau = building_energy_bau_data$residential,
  .selected_ctu = "all",
  .grid_decarbonization_pct = 1,
  .enviro_factors = enviro_factors
)

calc_electrify_residential_heating(
  res_tb = calc_ghg_residential(
    res_tb = building_energy_bau_data$residential,
    res_tb_bau = building_energy_bau_data$residential,
    .selected_ctu = "all",
    .grid_decarbonization_pct = 0.80,
    .enviro_factors = enviro_factors
  ),
  .selected_ctu = "all",
  .additional_electrified_residential_buildings_pct = 0.45,
  .res_natural_gas_for_space_heating_pct = 0.71,
  .res_natural_gas_for_water_heating_pct = 0.24,
  .grid_decarbonization_pct = 0.80,
  .enviro_factors = enviro_factors
)

calc_residential_renewable_ng(
  res_tb = calc_ghg_residential(
    res_tb = building_energy_bau_data$residential,
    res_tb_bau = building_energy_bau_data$residential,
    .selected_ctu = "all",
    .grid_decarbonization_pct = 1,
    .enviro_factors = enviro_factors
  ),
  .selected_ctu = "all",
  .enviro_factors = enviro_factors
)

scen_building_residential(
  res_tb = building_energy_bau_data$residential,
  res_tb_bau = building_energy_bau_data$residential,
  .selected_ctu = "all",
  .renewable_ng_res = FALSE,
  .new_homes_to_multifamily_pct = 0.50,
  .single_family_floor_area_growth_pct = 0.05,
  .new_homes_affected_pct = 0.50,
  .new_homes_leed_gold_pct = 0.50,
  .existing_home_retrofit_pct = 0.80,
  .existing_home_ultra_retrofit_pct = 0.20,
  .home_behavior_change_pct = 1.00,
  .grid_decarbonization_pct = 1,
  .additional_electrified_residential_buildings_pct = 0.45,
  .res_natural_gas_for_space_heating_pct = 0.71,
  .res_natural_gas_for_water_heating_pct = 0.24,
  .enviro_factors = enviro_factors
)


# non_residential

calc_existing_comm_building_efficiency(
  non_res_tb = building_energy_bau_data$non_residential,
  .existing_high_efficiency_buildings_pct = 0.80,
  .selected_ctu = "all"
)
# tidyr::pivot_wider(
#   names_from = c("var", "year"),
#   values_from = "value",
#   names_sep = "."
# )

check <- calc_ghg_non_residential(
  non_res_tb = calc_existing_comm_building_efficiency(
    non_res_tb = building_energy_bau_data$non_residential,
    .existing_high_efficiency_buildings_pct = 0.80,
    .selected_ctu = "all"
  ),
  non_res_tb_bau = building_energy_bau_data$non_residential,
  .selected_ctu = "all",
  .industrial_smart_grid_pct = 1,
  .commercial_smart_grid_pct = 1,
  .grid_decarbonization_pct = 0.8,
  .smart_grid_energy_reduction_pct = 1,
  .enviro_factors = enviro_factors,
  .existing_high_efficiency_buildings_pct = 0.8
) %>%
  tidyr::pivot_wider(
    names_from = c("var", "year"),
    values_from = "value",
    names_sep = "."
  )



calc_electrify_commercial_heating(
  non_res_tb = calc_ghg_non_residential(
    non_res_tb = building_energy_bau_data$non_residential,
    non_res_tb_bau = building_energy_bau_data$non_residential,
    .selected_ctu = "all",
    .industrial_smart_grid_pct = 1,
    .commercial_smart_grid_pct = 1,
    .grid_decarbonization_pct = 1,
    .smart_grid_energy_reduction_pct = 1,
    .enviro_factors = enviro_factors,
    .existing_high_efficiency_buildings_pct = 0.8
  ),
  .selected_ctu = "all",
  .grid_decarbonization_pct = 0.8,
  .electrified_buildings_pct = 0.40,
  .non_res_natural_gas_for_water_heating_pct = 0.20,
  .non_res_natural_gas_for_space_heating_pct = 0.69,
  .enviro_factors = enviro_factors
)

calc_non_res_renewable_ng(
  .renewable_ng_nonres = TRUE,
  non_res_tb = calc_ghg_non_residential(
    non_res_tb = building_energy_bau_data$non_residential,
    non_res_tb_bau = building_energy_bau_data$non_residential,
    .selected_ctu = "all",
    .industrial_smart_grid_pct = 1,
    .commercial_smart_grid_pct = 1,
    .grid_decarbonization_pct = 0.8,
    .smart_grid_energy_reduction_pct = 1,
    .enviro_factors = enviro_factors,
    .existing_high_efficiency_buildings_pct = 0.8
  ),
  .selected_ctu = "all",
  .enviro_factors = enviro_factors
)

scen_building_non_residential(
  .renewable_ng_nonres = TRUE,
  non_res_tb = building_energy_bau_data$non_residential,
  non_res_tb_bau = building_energy_bau_data$non_residential,
  .selected_ctu = "all",
  .electrified_buildings_pct = 0.40,
  .non_res_natural_gas_for_water_heating_pct = 0.20,
  .non_res_natural_gas_for_space_heating_pct = 0.69,
  .commercial_smart_grid_pct = 1.00,
  .industrial_smart_grid_pct = 1.00,
  .smart_grid_energy_reduction_pct = 1.00,
  .grid_decarbonization_pct = 0.80,
  .existing_high_efficiency_buildings_pct = 0.80,
  .enviro_factors = enviro_factors
)
