#' @title Calculate Strategy Electrify Commercial Heating
#' @family building_energy_module
#'
#' @description `calc_electrify_commercial_heating` calculates the effect of electrifying
#' commercial buildings on greenhouse gas emissions.
#'
#' @param .electrified_buildings_pct Numeric. A number between `0` and `1`.
#'      The fraction of additional commercial buildings that will be electrified.
#'      Default is `0.40`
#' @param .natural_gas_for_water_heating_pct Numeric. A number between `0` and `1`.
#'      The percent of natural gas that is commonly used for heating water in commercial buildings.
#'      Default is `0.20`
#' @param .natural_gas_for_space_heating_pct Numeric. A number between `0` and `1`.
#'      The percent of natural gas that is commonly used for space heating in commercial buildings.
#'      Default is `0.69`
#' @param .boiler_to_heat_pump_efficiency_ratio Numeric.
#' The ratio of boiler to heat pump efficiency
#'      Default is `1.59362`
#' @param .enviro_factors
#'
#' @return
#' @export
#'
#' @examples
#' \donotrun{
#' ghg.sp::calc_electrify_commercial_heating(
#'     non_res_tb = calc_ghg_non_residential(
#'           non_res_tb = building_data$non_residential,
#'           non_res_tb_bau = building_data$non_residential,
#'           .industrial_smart_grid_pct = 1,
#'           .commercial_smart_grid_pct = 1,
#'           .grid_decarbonization_pct = 0.8,
#'           .smart_grid_energy_reduction_pct = 1,
#'           .enviro_factors = enviro_factors,
#'           .existing_high_efficiency_buildings_pct = 0.8
#'     ),
#'     .grid_decarbonization_pct = 0.8,
#'     .electrified_buildings_pct = 0.40,
#'     .non_res_natural_gas_for_water_heating_pct = 0.20,
#'     .non_res_natural_gas_for_space_heating_pct = 0.69,
#'     .boiler_to_heat_pump_efficiency_ratio = 1.59362,
#'     .enviro_factors = enviro_factors
#')
#'}
calc_electrify_commercial_heating <-
  function(non_res_tb,
           .electrified_buildings_pct,
           .non_res_natural_gas_for_water_heating_pct,
           .non_res_natural_gas_for_space_heating_pct,
           .boiler_to_heat_pump_efficiency_ratio,
           .grid_decarbonization_pct,
           .enviro_factors) {
    new_non_res_tb <-
      non_res_tb %>%
      dplyr::mutate(
        reduced_therms =
          commercial_therms.bau.2040 * .electrified_buildings_pct,
        gas_savings_pct =
          1 - ((commercial_therms.bau.2040 - commercial_therms.scen.2040)
               / commercial_therms.scen.2040
          ),
        commercial_natural_gas_emissions_kg_co.scen.2040 =

          (
            reduced_therms *
              gas_savings_pct *
              .non_res_natural_gas_for_space_heating_pct *
              enviro_factors$KG_CO2E_PER_THERM_FORECAST
          )
        - (
          reduced_therms *
            gas_savings_pct *
            .non_res_natural_gas_for_space_heating_pct *
            .boiler_to_heat_pump_efficiency_ratio *

            (
              enviro_factors$KG_CO2E_PER_MHW_FORECAST
              * (1 - .grid_decarbonization_pct)
              * .enviro_factors$THERM_TO_MWH
            )
        )
        + (
          reduced_therms
          * gas_savings_pct
          * .non_res_natural_gas_for_water_heating_pct
          * enviro_factors$KG_CO2E_PER_THERM_FORECAST

        )
        - ((
          reduced_therms
          * gas_savings_pct
          * .non_res_natural_gas_for_water_heating_pct
          * enviro_factors$KG_CO2E_PER_THERM_FORECAST

        ) /
          (
            1 *
              (
                enviro_factors$KG_CO2E_PER_MHW_FORECAST *
                  (1 - .grid_decarbonization_pct)
              )
            * .enviro_factors$THERM_TO_MWH
          )
        )
      )

    return(new_non_res_tb)

  }
