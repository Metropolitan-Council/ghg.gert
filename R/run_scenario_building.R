#' @title Run building energy scenarios
#'
#' @inheritParams scen_building_residential
#' @inheritParams scen_building_non_residential
#'
#' @family building_energy_module
#'
#' @return
#' @export
#'
run_scenario_building <-
  function(res_tb = building_data$residential,
           non_res_tb = building_data$non_residential,
           .enviro_factors = enviro_factors,


           #non-residential
           .electrified_buildings_pct = 0.40,
           .natural_gas_for_water_heating_pct = 0.20,
           .natural_gas_for_space_heating_pct =  0.69,
           .boiler_to_heat_pump_efficiency_ratio =  1.59362,
           .commercial_smart_grid_pct = 1.00,
           .industrial_smart_grid_pct = 1.00,

           #residential
           .new_homes_to_multifamily_pct = 0,
           .single_family_floor_area_growth_pct = 0.05,
           .existing_high_efficiency_buildings_pct = 0.80,
           .home_behavior_change_pct = 1.00,
           .single_family_floor_area_growth_pct = 0.05,
           .new_homes_affected_pct = 0.30,
           .new_homes_leed_gold_pct = 0.50,
           .existing_home_retrofit_pct = 0.80,
           .existing_home_ultra_retrofit_pct = 0.20,

           #grid
           .smart_grid_energy_reduction_pct = 1,
           .grid_decarbonization_pct = 1) {

    scen_building_residential(
      .new_homes_to_multifamily_pct = .new_homes_to_multifamily_pct,
      .pct_growth_single_family_floor_area = .pct_growth_single_family_floor_area,
      .existing_high_efficiency_buildings_pct = .existing_high_efficiency_buildings_pct,
      .home_behavior_change_pct = .home_behavior_change_pct,
      .single_family_floor_area_growth_pct = .single_family_floor_area_growth_pct,
      .new_homes_affected_pct = .new_homes_affected_pct,
      .new_homes_leed_gold_pct = .new_homes_leed_gold_pct,
      .existing_home_retrofit_pct = .existing_home_retrofit_pct,
      .existing_home_ultra_retrofit_pct = .existing_home_ultra_retrofit_pct
    )

    scen_building_non_residential(
      .electrified_buildings_pct = .electrified_buildings_pct,
      .natural_gas_for_water_heating_pct = .natural_gas_for_water_heating_pct,
      .natural_gas_for_space_heating_pct =  .natural_gas_for_space_heating_pct,
      .boiler_to_heat_pump_efficiency_ratio =  .boiler_to_heat_pump_efficiency_ratio,
      .commercial_smart_grid_pct = .commercial_smart_grid_pct,
      .industrial_smart_grid_pct = .industrial_smart_grid_pct,
    )
  }
