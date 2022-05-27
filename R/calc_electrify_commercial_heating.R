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
#'
#'
#' }
calc_electrify_commercial_heating <-
  function(tb,
           .electrified_buildings_pct = 0.40,
           .natural_gas_for_water_heating_pct = 0.20,
           .natural_gas_for_space_heating_pct = 0.69,
           .boiler_to_heat_pump_efficiency_ratio = 1.59362,
           .enviro_factors = enviro_factors) {
    new_non_res_tb <- tb %>%
      dplyr::mutate(
        reduced_therms =
          commercial_therms.bau.2040 * .electrified_buildings_pct,
        gas_savings_pct = 1 - commercial_therms.scen.2040 / commercial_therms.bau.2040,
        commercial_natural_gas_emissions_kg_co.scen.2040 =

          (
            reduced_therms *
              gas_savings_pct *
              .natural_gas_for_space_heating_pct *
              kg_per_therm.scen.2040
          )
        - (
          reduced_therms *
            gas_savings_pct *
            .natural_gas_for_space_heating_pct *
            .boiler_to_heat_pump_efficiency_ratio *
            .enviro_factors$THERM_TO_MWH *
            kg_per_mwh.scen.2040 *
            0.2
        )
        + (
          reduced_therms
          * .natural_gas_for_water_heating_pct
          * kg_per_therm.scen.2040 *
            gas_savings_pct
        )
        - ((
          reduced_therms *
            .natural_gas_for_water_heating_pct *
            kg_per_therm.scen.2040 *
            gas_savings_pct
        ) /
          (1 *
             kg_per_mwh.scen.2040 *
             .enviro_factors$THERM_TO_MWH *
             0.2)
        )
      )

    return(new_non_res_tb)

  }
