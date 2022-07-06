#' @title Calculate Non-Residential Building Emissions
#' @family commercial-industrial
#' @family buildings
#' @family emissions
#'
#' @description calculates total energy demand and emissions from
#' workers for industrial and commercial sectors by city/township for the specified scenario.
#'
#' @param non_res_tb table with non-residential data.
#'      Default is `building_data$non_residential`
#' @param .industrial_smart_grid_pct numeric, a value between `0` and `1`.
#'      The percentage of industrial buildings that would be on the smart grid for the
#'      pecified scenario. Default is `1`.
#' @param .commercial_smart_grid_pct numeric, a value between `0` and `1`.
#'      The percentage of commercial buildings that would be on the smart grid for the
#'      specified scenario.
#'      Default is `1`.
#' @param .grid_decarbonization_pct numeric, a value between `0` and `1`. Default is `1`.
#' @param .smart_grid_energy_reduction_pct numeric, a value between `0` and `1`. Default is `1`.
#' @inheritParams run_scenario_transportation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_ghg_non_residential(
#'   non_res_tb = building_data$non_residential,
#'   non_res_tb_bau = building_data$non_residential,
#'   .industrial_smart_grid_pct = 1,
#'   .commercial_smart_grid_pct = 1,
#'   .grid_decarbonization_pct = 1,
#'   .smart_grid_energy_reduction_pct = 1,
#'   .enviro_factors = enviro_factors,
#'   .existing_high_efficiency_buildings_pct = 0.8
#' )
#' }
calc_ghg_non_residential <- function(non_res_tb,
                                     non_res_tb_bau,
                                     .commercial_smart_grid_pct,
                                     .industrial_smart_grid_pct,
                                     .smart_grid_energy_reduction_pct,
                                     .grid_decarbonization_pct,
                                     .existing_high_efficiency_buildings_pct,
                                     .enviro_factors) {
  emis <-
    function(tb,
             grid_decarb,
             commercial_smart_grid_pct,
             industrial_smart_grid_pct,
             smart_grid_decarb) {
      tb %>%
        dplyr::filter(
          var %in% c(
            "population",
            "commercial_jobs",
            "industrial_jobs",
            "commercial_therm_per_worker",
            "industrial_therm_per_worker",
            "commercial_mwh_per_worker",
            "industrial_mwh_per_worker"
          )
        ) %>%
        dplyr::group_by(ctu_name, year, var) %>%
        tidyr::pivot_wider(names_from = "var", values_from = value) %>%
        dplyr::mutate(
          kg_per_mwh = dplyr::case_when(
            year < 2040 ~ .enviro_factors$KG_CO2E_PER_MHW_BASELINE,
            TRUE ~ .enviro_factors$KG_CO2E_PER_MHW_FORECAST
          ),
          kg_per_therm = dplyr::case_when(
            year < 2040 ~ .enviro_factors$KG_CO2E_PER_THERM_BASELINE,
            TRUE ~ .enviro_factors$KG_CO2E_PER_THERM_FORECAST
          )
        ) %>%
        dplyr::mutate(
          # mw hours
          commercial_mwh = (commercial_jobs * commercial_smart_grid_pct) * commercial_mwh_per_worker,
          industrial_mwh = (industrial_jobs * industrial_smart_grid_pct) * industrial_mwh_per_worker,

          # therms
          commercial_therms = commercial_jobs * commercial_therm_per_worker,
          industrial_therms = industrial_jobs * industrial_therm_per_worker,


          # electric emissions
          commercial_electricity_emissions_kg_co =
            commercial_mwh * (kg_per_mwh * (1 - grid_decarb) * (1 - smart_grid_decarb)),
          industrial_electricity_emissions_kg_co =
            industrial_mwh * (kg_per_mwh * (1 - grid_decarb) * (1 - smart_grid_decarb)),
          # therm emissions

          commercial_natural_gas_emissions_kg_co =
            commercial_therms * kg_per_therm,
          industrial_natural_gas_emissions_kg_co =
            industrial_therms * kg_per_therm,
          total_industrial_commercial_emissions = sum(
            c(
              commercial_electricity_emissions_kg_co,
              industrial_electricity_emissions_kg_co,
              commercial_natural_gas_emissions_kg_co,
              industrial_natural_gas_emissions_kg_co
            ),
            na.rm = T
          )
        ) %>%
        dplyr::select(
          ctu_name,
          year,
          population,
          commercial_jobs,
          industrial_jobs,
          kg_per_mwh,
          kg_per_therm,
          commercial_mwh_per_worker,
          industrial_mwh_per_worker,
          commercial_mwh,
          industrial_mwh,
          commercial_therms,
          industrial_therms,
          commercial_electricity_emissions_kg_co,
          industrial_electricity_emissions_kg_co,
          commercial_natural_gas_emissions_kg_co,
          industrial_natural_gas_emissions_kg_co,
          total_industrial_commercial_emissions
        )
    }

  emis_bau <-
    emis(
      tb = non_res_tb_bau,
      grid_decarb = 0,
      commercial_smart_grid_pct = 1,
      industrial_smart_grid_pct = 1,
      smart_grid_decarb = 1
    )
  emis_strategy <-
    emis(
      tb = calc_existing_comm_building_efficiency(non_res_tb,
                                                  .existing_high_efficiency_buildings_pct = .existing_high_efficiency_buildings_pct
      ),
      grid_decarb = .grid_decarbonization_pct,
      commercial_smart_grid_pct = .commercial_smart_grid_pct,
      industrial_smart_grid_pct = .industrial_smart_grid_pct,
      smart_grid_decarb = .smart_grid_energy_reduction_pct
    )
  emis_final <-
    dplyr::right_join(
      emis_bau,
      emis_strategy,
      by = c("ctu_name", "year"),
      suffix = c(".bau", ".scen")
    ) %>%
    tidyr::pivot_longer(
      names_to = "var",
      values_to = "value",
      cols = -c(ctu_name, year)
    ) %>%
    tidyr::pivot_wider(
      names_from = c(var, year),
      values_from = value,
      names_sep = "."
    )

  return(emis_final)
}
