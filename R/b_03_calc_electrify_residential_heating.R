#' @title Calculate strategy electrify residential heating
#' @family residential
#' @family buildings
#' @description Calculates the impact of electrifying
#' heat in the residential sector by city/township for the specified scenario.
#'
#' @param .additional_electrified_residential_buildings_pct numeric,  a value between `0` and `1`.
#'   Default is `0`.
#' The percentage of buildings that would
#' be electrified under the specified scenario.
#'
#' @inheritParams run_scenario_building
#' @inheritParams calc_parking_lot_land_cover
#' @inheritParams run_all_modules
#' @inheritParams run_scenario_land_use
#' @inheritParams filter_ctu
#'
#' @return [tibble::tibble()].
#' Data table with output of electrify residential heating.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.ccap)
#'
#' calc_electrify_residential_heating(
#'   res_tb = calc_ghg_residential(
#'     res_tb = building_data$residential,
#'     res_tb_bau = building_data$residential,
#'     .selected_ctu = "all",
#'     .grid_decarbonization_pct = 0.80,
#'     .enviro_factors = enviro_factors
#'   ),
#'   .selected_ctu = "all",
#'   .additional_electrified_residential_buildings_pct = 0.45,
#'   .grid_decarbonization_pct = 0.80,
#'   .enviro_factors = enviro_factors
#' )
#' }
calc_electrify_residential_heating <- function(res_tb,
                                               .selected_ctu,
                                               .additional_electrified_residential_buildings_pct,
                                               .grid_decarbonization_pct,
                                               .enviro_factors = enviro_factors) {
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)

  new_res_tb <- res_tb %>%
    tidyr::pivot_wider(
      id_cols = c(geog_name),
      names_from = c(var, scen, year),
      values_from = value,
      names_sep = "."
    ) %>%
    dplyr::mutate(
      # reduced therms
      reduced_therms.scen.2040 =
        residential_therms.scen.2040 *
          .additional_electrified_residential_buildings_pct,

      # scenario therms 2040
      residential_therms.scen.2040 =
        residential_therms.scen.2040 -
          reduced_therms.scen.2040,

      # residential natural gas emissions
      residential_natural_gas_emissions_kg_co.scen.2040 =

        residential_therms.scen.2040 *
          .enviro_factors$KG_CO2E_PER_THERM_FORECAST,

      # residential MWH scenario 2040
      residential_mwh.scen.2040 =
        residential_mwh.scen.2040 +
          (
            (
              reduced_therms.scen.2040 *
                .enviro_factors$RES_NATUAL_GAS_FOR_SPACE_HEATING_PCT *
                .enviro_factors$BOILER_TO_HEAT_PUMP_EFFICIENCY_RATIO
            ) * .enviro_factors$THERM_TO_MWH +
              (
                reduced_therms.scen.2040 *
                  .enviro_factors$RES_NATURAL_GAS_FOR_WATER_HEATING_PCT *
                  .enviro_factors$BOILER_TO_HEAT_PUMP_EFFICIENCY_RATIO
              ) * .enviro_factors$THERM_TO_MWH
          ),
      residential_electricity_emissions_kg_co.scen.2040 =
        residential_mwh.scen.2040 *
          .enviro_factors$KG_CO2E_PER_MHW_FORECAST * (1 - .grid_decarbonization_pct)
    ) %>%
    tidyr::pivot_longer(
      cols = c(2:last_col()),
      names_to = "var",
      values_to = "value"
    ) %>%
    tidyr::separate(
      col = var,
      into = c("var", "scen", "year"),
      sep = "\\."
    ) %>%
    dplyr::ungroup()


  return(new_res_tb)
}
