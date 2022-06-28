#' @title Calculate Renewable Natural Gas Impact on Non-Residential Building Emissions
#' @family Commercial/Industrial
#' @family Buildings
#'
#' @description Calculates the impact of transitioning to
#' renewable natural gas on greenhouse gas emissions by city/township for the
#' specified scenario.
#'
#' @return [tibble::tibble()].
#' @export
#'
#' @note `calc_non_res_renewable_ng()` is called within `scen_building_non_residential()`
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_non_res_renewable_ng(
#'      non_res_tb = calc_ghg_non_residential(
#'           non_res_tb = building_data$non_residential,
#'           non_res_tb_bau = building_data$non_residential,
#'           .industrial_smart_grid_pct = 1,
#'           .commercial_smart_grid_pct = 1,
#'           .grid_decarbonization_pct = 0.8,
#'           .smart_grid_energy_reduction_pct = 1,
#'           .enviro_factors = enviro_factors,
#'           .existing_high_efficiency_buildings_pct = 0.8
#'     )
#'      .enviro_factors = enviro_factors
#' )
#' }
#'
calc_non_res_renewable_ng <-
  function(non_res_tb,
           .enviro_factors = .enviro_factors) {
    new_non_res_tb <-
      non_res_tb %>%
      dplyr::mutate(
        reduced_therms =
          (commercial_therms.bau.2040 +
             industrial_therms.bau.2040) -
          (industrial_therms.scen.2040 +
             commercial_therms.scen.2040)

      ) %>%
      dplyr::mutate(
        commercial_natural_gas_emissions_kg_co.scen.2040 =
          ((commercial_therms.bau.2040 + industrial_therms.bau.2040) -
             (reduced_therms - (78 * population.bau.2040))
          ) *
          .enviro_factors$KG_CO2E_PER_THERM_FORECAST
      )

    return(new_non_res_tb)
  }
