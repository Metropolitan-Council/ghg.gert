#' @title Calculate strategy electrify commercial heating
#' @family commercial-industrial
#' @family buildings
#'
#' @description Calculates the effect of electrifying
#'     commercial buildings on greenhouse gas emissions by city/township
#'     for the specified scenario.
#'      For more details, see `vignette("building_energy_module_outputs_non_residential")`
#'
#' @inheritParams run_scenario_building
#' @param .electrified_buildings_pct numeric,  a value between `0` and `1`.
#'      The fraction of additional commercial buildings that will be electrified.
#'      Default is `0.0`.
#' @param .enviro_factors
#'
#' @return [tibble::tibble()]
#'     A table with columns `year`, `ctu_name`, `population`,
#'    `residential_mwh`,
#'    `residential_electricity_emissions_kg_co`,
#'    `residential_therms`, and
#'    `residential_natural_gas_emissions_kg_co` with the modified values
#'    to reflect building electrification.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#' calc_electrify_commercial_heating(
#'   non_res_tb = calc_ghg_non_residential(
#'     non_res_tb = building_energy_bau_data$non_residential,
#'     non_res_tb_bau = building_energy_bau_data$non_residential,
#'     .selected_ctu = "all",
#'     .grid_decarbonization_pct = 1,
#'     .smart_grid_energy_reduction_pct = 1,
#'     .enviro_factors = enviro_factors,
#'     .existing_high_efficiency_buildings_pct = 0.8
#'   ),
#'   .selected_ctu = "all",
#'   .grid_decarbonization_pct = 0.8,
#'   .electrified_buildings_pct = 0.40,
#'   .enviro_factors = enviro_factors
#' )
#' }
calc_electrify_commercial_heating <- function(non_res_tb,
                                              .selected_ctu,
                                              .electrified_buildings_pct,
                                              .grid_decarbonization_pct,
                                              .enviro_factors = ghg.sp::enviro_factors) {
  non_res_tb <- filter_ctu(non_res_tb, .selected_ctu = .selected_ctu)

  new_non_res_tb <- non_res_tb %>%
    tidyr::pivot_wider(
      names_from = c(var, scen, year),
      values_from = value,
      names_sep = "."
    ) %>%
    dplyr::mutate(
      # reduced therms
      reduced_therms.scen.2040 =
        commercial_therms.scen.2040 *
          .electrified_buildings_pct,
      commercial_therms.scen.2040 =
        commercial_therms.scen.2040 -
          reduced_therms.scen.2040,

      # scenario therms
      commercial_natural_gas_emissions_kg_co.scen.2040 =
        commercial_therms.scen.2040 *
          .enviro_factors$KG_CO2E_PER_THERM_FORECAST,
      commercial_mwh.scen.2040 =
        commercial_mwh.scen.2040 +
          ((
            reduced_therms.scen.2040 *
              .enviro_factors$NON_RES_NATURAL_GAS_FOR_SPACE_HEATING_PCT *
              .enviro_factors$BOILER_TO_HEAT_PUMP_EFFICIENCY_RATIO
          ) * .enviro_factors$THERM_TO_MWH +
            (
              reduced_therms.scen.2040 *
                .enviro_factors$NON_RES_NATURAL_GAS_FOR_WATER_HEATING_PCT *
                .enviro_factors$BOILER_TO_HEAT_PUMP_EFFICIENCY_RATIO
            ) * .enviro_factors$THERM_TO_MWH
          ),
      commercial_electricity_emissions_kg_co.scen.2040 =
        commercial_mwh.scen.2040 *
          .enviro_factors$KG_CO2E_PER_MHW_FORECAST * (1 - .grid_decarbonization_pct)
    ) %>%
    tidyr::pivot_longer(
      names_to = "var",
      values_to = "value",
      cols = -c(ctu_name)
    ) %>%
    tidyr::separate(
      col = var,
      into = c("var", "scen", "year"),
      sep = "\\."
    ) %>%
    dplyr::ungroup()

  return(new_non_res_tb)
}
