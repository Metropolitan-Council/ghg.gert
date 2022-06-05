#' @title Run Building Energy Scenarios
#' @family Buildings
#'
#' @description `run_scenario_transportation_building` produces the outputs of the building energy module of the
#' Metropolitan Council Greenhouse Gas Scenario Planning Tool by city/township for the specified scenario
#'
#' @inheritParams scen_building_residential
#' @inheritParams scen_building_non_residential
#' @param grid_decarbonization_pct **Numeric**. A number between `0` and `1`.
#'
#' @return **Tibble**.
#'       Returns a table with columns `ctu_name`, `var`, `scen`, `year`, and `value`.
#'       The table is the output of the building energy module, any modification to
#'       the inputs of the building energy module must be specified as an argument
#'       to the function `run_scenario_transportation_building()`
#'
#' @export
#'
#' @examples
#' \donotrun{
#' run_scenario_transportation_building <-
#' function(res_tb = building_data$residential,
#'          non_res_tb = building_data$non_residential,
#'          res_tb_bau = building_data$residential,
#'          non_res_tb_bau = building_data$non_residential,
#'          .enviro_factors = enviro_factors,
#'          .electrified_buildings_pct = 0.40,
#'          .non_res_natural_gas_for_water_heating_pct = 0.20,
#'          .non_res_natural_gas_for_space_heating_pct =  0.69,
#'          .boiler_to_heat_pump_efficiency_ratio =  1.59362,
#'          .commercial_smart_grid_pct = 1.00,
#'          .industrial_smart_grid_pct = 1.00,
#'          .smart_grid_energy_reduction_pct = 1.00,
#'          .new_homes_to_multifamily_pct = 0.50,
#'          .existing_high_efficiency_buildings_pct = 0.80,
#'          .home_behavior_change_pct = 1.00,
#'          .single_family_floor_area_growth_pct = 0.05,
#'          .new_homes_affected_pct = 0.30,
#'          .new_homes_leed_gold_pct = 0.50,
#'          .existing_home_retrofit_pct = 0.80,
#'          .existing_home_ultra_retrofit_pct = 0.20,
#'          .res_natural_gas_for_space_heating_pct = 0.71,
#'          .res_natural_gas_for_water_heating_pct = 0.24,
#'          .additional_electrified_residential_buildings_pct = 0.45,
#'          .grid_decarbonization_pct = 1)
#' }
#'
run_scenario_transportation_building <-
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
           .smart_grid_energy_reduction_pct = 1.00,
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
        res_tb_bau = res_tb_bau,
        .new_homes_to_multifamily_pct = .new_homes_to_multifamily_pct,
        .single_family_floor_area_growth_pct = .single_family_floor_area_growth_pct,
        .home_behavior_change_pct = .home_behavior_change_pct,
        .new_homes_affected_pct = .new_homes_affected_pct,
        .new_homes_leed_gold_pct = .new_homes_leed_gold_pct,
        .existing_home_retrofit_pct = .existing_home_retrofit_pct,
        .existing_home_ultra_retrofit_pct = .existing_home_ultra_retrofit_pct,
        .additional_electrified_residential_buildings_pct = .additional_electrified_residential_buildings_pct,
        .res_natural_gas_for_space_heating_pct = .res_natural_gas_for_space_heating_pct,
        .res_natural_gas_for_water_heating_pct = .res_natural_gas_for_water_heating_pct,
        .boiler_to_heat_pump_efficiency_ratio = .boiler_to_heat_pump_efficiency_ratio,
        .grid_decarbonization_pct = .grid_decarbonization_pct,
        .enviro_factors = .enviro_factors
      )

    non_res <-
      scen_building_non_residential(
        non_res_tb = non_res_tb,
        non_res_tb_bau = non_res_tb_bau,
        .existing_high_efficiency_buildings_pct = .existing_high_efficiency_buildings_pct,
        .electrified_buildings_pct = .electrified_buildings_pct,
        .non_res_natural_gas_for_water_heating_pct = .non_res_natural_gas_for_water_heating_pct,
        .non_res_natural_gas_for_space_heating_pct =  .non_res_natural_gas_for_space_heating_pct,
        .boiler_to_heat_pump_efficiency_ratio =  .boiler_to_heat_pump_efficiency_ratio,
        .commercial_smart_grid_pct = .commercial_smart_grid_pct,
        .industrial_smart_grid_pct = .industrial_smart_grid_pct,
        .grid_decarbonization_pct = .grid_decarbonization_pct,
        .smart_grid_energy_reduction_pct = .smart_grid_energy_reduction_pct,
        .enviro_factors = .enviro_factors
      )

    building_module_ouput <-
      bind_rows(res, non_res)


    return(building_module_ouput)

  }
