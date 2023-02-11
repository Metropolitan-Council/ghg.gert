# Residential Baseline 2018

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
  ungroup() %>%
  add_variable_names(., "2018 Baseline")


# Residential BAU (2040)

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
  ungroup() %>%
  add_variable_names(., "2040 Business-as-Usual")


# Non-Residential Baseline (2018)

non_res_bau_2018 <- run_scenario_building(
  run_residential = FALSE,
  run_non_residential = TRUE,
  .selected_ctu = params$ctu_selection,
  non_res_tb = building_data$non_residential,
  non_res_tb_bau = building_data$non_residential,
  .renewable_ng_nonres = FALSE,
  .electrified_buildings_pct = 0,
  .non_res_natural_gas_for_water_heating_pct = 0.20,
  .non_res_natural_gas_for_space_heating_pct = 0.69,
  .commercial_smart_grid_pct = 1,
  .industrial_smart_grid_pct = 1,
  .smart_grid_energy_reduction_pct = 1,
  .grid_decarbonization_pct = 0,
  .existing_high_efficiency_buildings_pct = 0,
  .enviro_factors = enviro_factors
) %>%
  ungroup() %>%
  dplyr::filter(
    year == 2018,
    scen == "bau",
    var %in% c(
      "commercial_mwh",
      "industrial_mwh",
      "commercial_therms",
      "industrial_therms",
      "total_industrial_commercial_emissions"
    )
  ) %>%
  add_variable_names(., "2018 Baseline")

# Non-Residential BAU (2040)

non_res_bau_2040 <- run_scenario_building(
  run_residential = FALSE,
  run_non_residential = TRUE,
  .selected_ctu = params$ctu_selection,
  non_res_tb = building_data$non_residential,
  non_res_tb_bau = building_data$non_residential,
  .renewable_ng_nonres = FALSE,
  .electrified_buildings_pct = 0,
  .non_res_natural_gas_for_water_heating_pct = 0.20,
  .non_res_natural_gas_for_space_heating_pct = 0.69,
  .commercial_smart_grid_pct = 1,
  .industrial_smart_grid_pct = 1,
  .smart_grid_energy_reduction_pct = 1,
  .grid_decarbonization_pct = 0,
  .existing_high_efficiency_buildings_pct = 0,
  .enviro_factors = enviro_factors
) %>%
  ungroup() %>%
  dplyr::filter(
    year == 2040,
    scen == "bau",
    var %in% c(
      "commercial_mwh",
      "industrial_mwh",
      "commercial_therms",
      "industrial_therms",
      "total_industrial_commercial_emissions"
    )
  ) %>%
  add_variable_names(., "2040 Busines-As-Usual")

# Land Use Baseline (2018)

luse_bau_2018 <- run_scenario_land_use(
  tb = land_use_data,
  .selected_ctu = params$ctu_selection,
  .urban_form_scenario = "bau",
  .conservation_tillage_intervention = "current_conservation_tillage",
  .tree_planting_intervention = "match_la_million_trees_goal",
  .tree_planting_per_capita = 0.26,
  .tree_planting_per_hectare = 247,
  .parking_lot_reduction_percentage = 0,
  detail = FALSE
) %>%
  filter(year == 2016)

# Land Use Busines-as-Usual (2040)

luse_bau_2040 <- run_scenario_land_use(
  tb = land_use_data,
  .selected_ctu = "Minneapolis",
  .urban_form_scenario = "bau",
  .conservation_tillage_intervention = "current_conservation_tillage",
  .tree_planting_intervention = "match_la_million_trees_goal",
  .tree_planting_per_capita = 0.26,
  .tree_planting_per_hectare = 247,
  .parking_lot_reduction_percentage = 0,
  detail = FALSE
)  %>%
  filter(year == 2040)


calc_conservation_tillage(
  tb = land_use_data,
  .selected_ctu = "Minneapolis",
  .urban_form_scenario = "bau",
  .conservation_tillage_intervention = "current_conservation_tillage",
  .tree_planting_intervention = "tree_planting_on_all_pervious",
  .tree_planting_per_capita = 0.26,
  .tree_planting_per_hectare = 247,
  .parking_lot_reduction_percentage = 0.8,
  detail = FALSE
)

calc_parking_lot_land_cover(
  tb = land_use_data,
  .selected_ctu = "Minneapolis",
  .urban_form_scenario = "bau",
  .parking_lot_reduction_percentage = 0.8,
  .tree_planting_intervention = "tree_planting_on_all_pervious",
  .tree_planting_per_capita = 0.26,
  .tree_planting_per_hectare = 247,
  detail = FALSE
)
