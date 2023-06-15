#' @title Calculate non-residential building emissions
#' @family commercial-industrial
#' @family buildings
#' @family emissions
#'
#' @description This function calculates the total energy demand and greenhouse gas (GHG) emissions
#'    for non-residential buildings in the commercial and industrial sectors, by city or township,
#'    under a specified scenario. The calculation considers parameters such as smart grid
#'    adoption rates, grid decarbonization rates, and energy reduction percentages. For a more
#'    detailed explanation, refer to `vignette("building_energy_module_outputs_non_residential")`.
#'
#' @param non_res_tb table with non-residential data.
#'      Default is `building_data$non_residential`
#' @param .grid_decarbonization_pct numeric, a value between `0` and `1`. Default is `0.4`.
#' @param .smart_grid_energy_reduction_pct numeric, a value between `0` and `1`. Default is `0`.
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
#'   .selected_ctu = "all",
#'   .grid_decarbonization_pct = 1,
#'   .smart_grid_energy_reduction_pct = 1,
#'   .enviro_factors = enviro_factors,
#'   .existing_high_efficiency_buildings_pct = 0.8
#' )
#' }
calc_ghg_non_residential <- function(non_res_tb,
                                     non_res_tb_bau,
                                     .selected_ctu,
                                     .smart_grid_energy_reduction_pct,
                                     .grid_decarbonization_pct,
                                     .existing_high_efficiency_buildings_pct,
                                     .enviro_factors = ghg.sp::enviro_factors) {
  non_res_tb <- filter_ctu(non_res_tb, .selected_ctu = .selected_ctu)
  non_res_tb_bau <-
    filter_ctu(non_res_tb_bau, .selected_ctu = .selected_ctu)

  emis <- function(tb,
                   grid_decarb,
                   smart_grid_decarb,
                   .enviro_factors = ghg.sp::enviro_factors) {
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
        commercial_mwh = .enviro_factors$COMMERCIAL_SMART_GRID_PCT *
          (commercial_jobs * commercial_mwh_per_worker),
        industrial_mwh = .enviro_factors$INDUSTRIAL_SMART_GRID_PCT *
          (industrial_jobs * industrial_mwh_per_worker),

        # therms
        commercial_therms = commercial_jobs * commercial_therm_per_worker,
        industrial_therms = industrial_jobs * industrial_therm_per_worker,


        # electric emissions
        commercial_electricity_emissions_kg_co =
          commercial_mwh * (kg_per_mwh * (1 -
           dplyr::if_else(year < 2040, .enviro_factors$GRID_DECARBONIZATION_BASELINE,
                             dplyr::if_else((grid_decarb + smart_grid_decarb > 1), 1,
                                                   grid_decarb + smart_grid_decarb
                                                   )
           )
          )),
        industrial_electricity_emissions_kg_co =
          industrial_mwh * (kg_per_mwh * (1 -
             dplyr::if_else(year < 2040, .enviro_factors$GRID_DECARBONIZATION_BASELINE,
                             dplyr::if_else((grid_decarb + smart_grid_decarb > 1), 1,
                                                   grid_decarb + smart_grid_decarb
                                                   )
             )
          )),
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
      grid_decarb = .enviro_factors$GRID_DECARBONIZATION_BASELINE,
      smart_grid_decarb = 0,
      .enviro_factors = .enviro_factors
    )

  emis_strategy <-
    emis(
      tb = calc_existing_comm_building_efficiency(
        non_res_tb,
        .selected_ctu = .selected_ctu,
        .existing_high_efficiency_buildings_pct = .existing_high_efficiency_buildings_pct,
        .enviro_factors = .enviro_factors
      ),
      grid_decarb = .grid_decarbonization_pct,
      smart_grid_decarb = .smart_grid_energy_reduction_pct,
      .enviro_factors = .enviro_factors
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
    tidyr::separate(
      col = var,
      into = c("var", "scen"),
      sep = "\\."
    ) %>%
    dplyr::ungroup()

  return(emis_final)
}
