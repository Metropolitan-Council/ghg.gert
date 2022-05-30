#' @title Calculate Strategy Electrify Residential Heating
#' @family building_energy_module
#'
#'
#' @param res_tb
#' @param .additional_electrified_residential_buildings_pct
#' @param .natural_gas_for_space_heating_pct
#' @param .natural_gas_for_water_heating_pct
#' @param .boiler_to_heat_pump_efficiency_ratio
#'
#' @return
#' @export
#'
#' @examples
#' \donotrun{
#' library(ghg.sp)
#'
#' ghg.sp::calc_electrify_residential_heating(
#'      tb = calc_ghg_residential(
#'           res_tb = building_data$residential,
#'           .grid_decarbonization_pct = 0.80,
#'           .enviro_factors = enviro_factors),
#'      .additional_electrified_residential_buildings_pct = 0.45,
#'      .natural_gas_for_space_heating_pct = 0.71,
#'      .natural_gas_for_water_heating_pct = 0.24,
#'      .boiler_to_heat_pump_efficiency_ratio = 1.59362,
#'      .grid_decarbonization_pct = 0.80,
#'      .enviro_factors = enviro_factors
#' )
#' }
calc_electrify_residential_heating <- function(tb,
                                               .additional_electrified_residential_buildings_pct,
                                               .natural_gas_for_space_heating_pct,
                                               .natural_gas_for_water_heating_pct,
                                               .boiler_to_heat_pump_efficiency_ratio,
                                               .grid_decarbonization_pct,
                                               .enviro_factors) {



  new_res_tb <- tb %>%
    mutate(
      gas_savings_pct =
        1 - ((residential_therms.bau.2040 - residential_therms.scen.2040)
             / residential_therms.scen.2040
        ),
      reduced_therms =  residential_therms.scen.2040
      * .additional_electrified_residential_buildings_pct,

      residential_natural_gas_emissions_kg_co.scen.2040 =
        (
          reduced_therms
          * gas_savings_pct
          * .natural_gas_for_space_heating_pct
          * .enviro_factors$KG_CO2E_PER_THERM_FORECAST
        )
      -
        (
          reduced_therms
          * gas_savings_pct
          * .natural_gas_for_space_heating_pct
          * .boiler_to_heat_pump_efficiency_ratio
        )
      * .enviro_factors$KG_CO2E_PER_MHW_FORECAST
      * (1 - .grid_decarbonization_pct)
      * .enviro_factors$THERM_TO_MWH

      +
        (
          (
            reduced_therms
            * gas_savings_pct
            * .natural_gas_for_water_heating_pct
            * .enviro_factors$KG_CO2E_PER_THERM_FORECAST
          )
          -
            (
              reduced_therms
              * gas_savings_pct
              * .natural_gas_for_water_heating_pct
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
