#' @title Run Building Energy Scenarios
#' @family building_energy_module
#'
#' @description `run_scenario_building` produces the outputs of the building energy module of the
#' Metropolitan Council Greenhouse Gas Scenario Planning Tool.
#'
#' @inheritParams scen_building_residential
#' @inheritParams scen_building_non_residential
#'
#'
#' @return
#' @export
#'
run_scenario_building <-
  function(res_tb = building_data$residential,
           non_res_tb = building_data$non_residential,
           res_tb_bau = building_data$residential,
           non_res_tb_bau = building_data$non_residential,
           .enviro_factors = enviro_factors,

           #non-residential
             #electrification
           .electrified_buildings_pct = 0.40,
           .non_res_natural_gas_for_water_heating_pct = 0.20,
           .non_res_natural_gas_for_space_heating_pct =  0.69,
           .boiler_to_heat_pump_efficiency_ratio =  1.59362,
             #smartgrid
           .commercial_smart_grid_pct = 1.00,
           .industrial_smart_grid_pct = 1.00,
           .smart_grid_energy_reduction_pct = 1,
           #residential
             #floor_area
           .new_homes_to_multifamily_pct = 0.50,
           .existing_high_efficiency_buildings_pct = 0.80,
           .home_behavior_change_pct = 1.00,
           .single_family_floor_area_growth_pct = 0.05,
           .new_homes_affected_pct = 0.30,
           .new_homes_leed_gold_pct = 0.50,
           .existing_home_retrofit_pct = 0.80,
           .existing_home_ultra_retrofit_pct = 0.20,
             #electrification
           .res_natural_gas_for_space_heating_pct = 0.71,
           .res_natural_gas_for_water_heating_pct = 0.24,
           .additional_electrified_residential_buildings_pct = 0.45,
           #grid
           .grid_decarbonization_pct = 1) {

    res <-
      scen_building_residential(
      tb = res_tb,
      .new_homes_to_multifamily_pct = .new_homes_to_multifamily_pct,
      .single_family_floor_area_growth_pct = .single_family_floor_area_growth_pct,
      .home_behavior_change_pct = .home_behavior_change_pct,
      .new_homes_affected_pct = .new_homes_affected_pct,
      .new_homes_leed_gold_pct = .new_homes_leed_gold_pct,
      .existing_home_retrofit_pct = .existing_home_retrofit_pct,
      .existing_home_ultra_retrofit_pct = .existing_home_ultra_retrofit_pct,
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .enviro_factors = .enviro_factors
    )

    non_res <-
      scen_building_non_residential(
      #.existing_high_efficiency_buildings_pct = .existing_high_efficiency_buildings_pct,
      .electrified_buildings_pct = .electrified_buildings_pct,
      .natural_gas_for_water_heating_pct = .natural_gas_for_water_heating_pct,
      .natural_gas_for_space_heating_pct =  .natural_gas_for_space_heating_pct,
      .boiler_to_heat_pump_efficiency_ratio =  .boiler_to_heat_pump_efficiency_ratio,
      .commercial_smart_grid_pct = .commercial_smart_grid_pct,
      .industrial_smart_grid_pct = .industrial_smart_grid_pct,
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .smart_grid_energy_reduction_pct = .smart_grid_energy_reduction_pct
    )

    return(res)

  }
