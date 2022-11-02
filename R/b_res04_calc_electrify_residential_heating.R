#' @title Calculate strategy electrify residential heating
#' @family residential
#' @family buildings
#' @description Calculates the impact of electrifying
#' heat in the residential sector by city/township for the specified scenario.
#'
#' @param .additional_electrified_residential_buildings_pct numeric,  a value between `0` and `1`.
#' The percentage of buildings that would
#' be electrified under the specified scenario.
#' @param .res_natural_gas_for_water_heating_pct numeric,  a value between `0` and `1`.
#' The percentage of natural gas that is commonly used for space heating in residential
#' buildings.
#' @param .res_natural_gas_for_water_heating_pct numeric,  a value between `0` and `1`.
#' The percentage of natural gas that is commonly used for water heating in residential
#' buildings.
#'
#' @inheritParams run_scenario_building
#'
#' @return [tibble::tibble()].
#' Data table with output of electrify residential heating.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_electrify_residential_heating(
#'   res_tb = calc_ghg_residential(
#'     res_tb = building_data$residential,
#'     res_tb_bau = building_data$residential,
#'     .grid_decarbonization_pct = 0.80,
#'     .enviro_factors = enviro_factors
#'   ),
#'   .additional_electrified_residential_buildings_pct = 0.45,
#'   .res_natural_gas_for_space_heating_pct = 0.71,
#'   .res_natural_gas_for_water_heating_pct = 0.24,
#'   .grid_decarbonization_pct = 0.80,
#'   .enviro_factors = enviro_factors
#' )
#' }
calc_electrify_residential_heating <- function(res_tb,
                                               res_tb_bau,
                                               .additional_electrified_residential_buildings_pct,
                                               .res_natural_gas_for_space_heating_pct,
                                               .res_natural_gas_for_water_heating_pct,
                                               .grid_decarbonization_pct,
                                               .enviro_factors) {
  new_res_tb <- res_tb %>%
    mutate(
      gas_savings_pct =
        1 - ((residential_therms.bau.2040 - residential_therms.scen.2040)
        / residential_therms.scen.2040
        ),
      reduced_therms = residential_therms.scen.2040
      * .additional_electrified_residential_buildings_pct,
      residential_natural_gas_emissions_kg_co.scen.2040 =
        (
          reduced_therms
          * gas_savings_pct
            * .res_natural_gas_for_space_heating_pct
            * .enviro_factors$KG_CO2E_PER_THERM_FORECAST
        )
        -
          (
            reduced_therms
            * gas_savings_pct
              * .res_natural_gas_for_space_heating_pct
              * enviro_factors$BOILER_TO_HEAT_PUMP_EFFICIENCY_RATIO
          )
          * .enviro_factors$KG_CO2E_PER_MHW_FORECAST
            * (1 - .grid_decarbonization_pct)
            * .enviro_factors$THERM_TO_MWH

          +
          (
            (
              reduced_therms
              * gas_savings_pct
                * .res_natural_gas_for_water_heating_pct
                * .enviro_factors$KG_CO2E_PER_THERM_FORECAST
            )
            -
              (
                reduced_therms
                * gas_savings_pct
                  * .res_natural_gas_for_water_heating_pct
                  * .enviro_factors$KG_CO2E_PER_THERM_FORECAST
              )
              / 1
                * .enviro_factors$KG_CO2E_PER_MHW_FORECAST
                * (1 - .grid_decarbonization_pct)
                * .enviro_factors$THERM_TO_MWH
          )
    )

  return(new_res_tb)
}
