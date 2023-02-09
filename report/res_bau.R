res_bau_2018 <- run_scenario_building(
  res_tb = building_energy_bau_data$residential,
  res_tb_bau = building_energy_bau_data$residential,
  run_residential = TRUE,
  run_non_residential = FALSE,
  .selected_ctu = params$ctu_selection,
  # affordable floor area
  .single_family_floor_area_growth_pct = 0,
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
) %>%
  filter(
    var %in% c(
      "residential_mwh",
      "residential_electricity_emissions_kg_co",
      "residential_therms",
      "residential_natural_gas_emissions_kg_co"
    ),
    year == 2018,
    scen == "bau"
  ) %>%
  ungroup()

res_bau_2040 <- run_scenario_building(
  res_tb = building_energy_bau_data$residential,
  res_tb_bau = building_energy_bau_data$residential,
  run_residential = TRUE,
  run_non_residential = FALSE,
  .selected_ctu = params$ctu_selection,
  # affordable floor area
  .single_family_floor_area_growth_pct = 0.15,
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
)  %>%
  filter(
    var %in% c(
      "residential_mwh",
      "residential_electricity_emissions_kg_co",
      "residential_therms",
      "residential_natural_gas_emissions_kg_co"
    ),
    year == 2040,
    scen == "scen"
  ) %>%
  ungroup()