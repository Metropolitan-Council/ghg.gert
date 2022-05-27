#' @title Calculate Renewable Natural Gas Impact on Non-Residential Building Emissions
#' @family building_energy_module
#' @description text...
#' @return
#' @export
#'
#' @note `calc_non_res_renewable_ng` is called within `scen_building_non_residential()`
#'
#' @examples
#' \donotrun{
#' ghg.sp::calc_non_res_renewable_ng(
#'      tb =
#'      .enviro_factors = enviro_factors
#' )
#' }
#'
calc_non_res_renewable_ng <-
  function(tb,
           .enviro_factors = .enviro_factors) {
    tb %>%
      dplyr::mutate(
        reduced_therms =
          (industrial_therms.scen.2040 +
             commercial_therms.scen.2040) -
          (commercial_therms.bau.2040 +
             industrial_therms.bau.2040)
      ) %>%
      dplyr::mutate(
        commercial_natural_gas_emissions_kg_co.scen.2040 =
          ((commercial_therms.bau.2040 + industrial_therms.bau.2040) -
             (reduced_therms - 78 * population.bau.2040)
          ) *
          .enviro_factors$KG_CO2E_PER_THERM_FORECAST
      )
  }
