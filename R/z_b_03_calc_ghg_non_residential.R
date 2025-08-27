#' @title DEPRECATED Calculate non-residential building emissions
#' @family commercial-industrial
#' @family buildings
#' @family emissions
#' @family deprecated
#'
#' @description This function calculates the total energy demand and greenhouse gas (GHG) emissions
#'    for non-residential buildings in the commercial and industrial sectors, by city or township,
#'    under a specified scenario. The calculation considers parameters such as smart grid
#'    adoption rates, grid decarbonization rates, and energy reduction percentages. For a more
#'    detailed explanation, refer to `vignette("building_energy_module_outputs_non_residential")`.
#'
#' @param non_res_tb table with non-residential data.
#'      Default is `building_data$non_residential`
#' @param .grid_decarbonization_pct numeric, a value between `0` and `1`. Default is `0.6`.
#' @param .smart_grid_energy_reduction_pct numeric, a value between `0` and `1`. Default is `0`.
#'
#' @inheritParams run_module_transportation
#' @inheritParams calc_parking_lot_land_cover
#' @inheritParams run_all_modules
#' @inheritParams run_scenario_land_use
#' @inheritParams filter_ctu
#' @inheritParams scen_building_non_residential
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.ccap)
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
                                     .enviro_factors = ghg.ccap::enviro_factors) {
  non_res_tb <- filter_ctu(non_res_tb, .selected_ctu = .selected_ctu)
  non_res_tb_bau <-
    filter_ctu(non_res_tb_bau, .selected_ctu = .selected_ctu)

  emis <- function(tb,
                   grid_decarb,
                   .enviro_factors = ghg.ccap::enviro_factors) {
    emis_tb <- tb %>%
      dplyr::filter(
        var %in% c(
          "population",
          "commercial_jobs",
          "industrial_jobs",
          "commercial_therm_per_worker",
          "industrial_therm_per_worker",
          "commercial_mwh_per_worker",
          "industrial_mwh_per_worker",
          "commercial_mwh",
          "commercial_therms",
          "industrial_mwh",
          "industrial_therms"
        )
      ) %>%
      dplyr::group_by(geog_name, geog_id, year, var) %>%
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
        commercial_mwh = .enviro_factors$COMMERCIAL_SMART_GRID_PCT * (1 - .enviro_factors$SMART_GRID_IMPACT) *
          dplyr::if_else(is.finite(commercial_mwh_per_worker * commercial_jobs), commercial_mwh_per_worker * commercial_jobs, commercial_mwh),
        industrial_mwh = .enviro_factors$INDUSTRIAL_SMART_GRID_PCT * (1 - .enviro_factors$SMART_GRID_IMPACT) *
          dplyr::if_else(is.finite(industrial_mwh_per_worker * industrial_jobs), industrial_mwh_per_worker * industrial_jobs, industrial_mwh),

        # therms
        commercial_therms = dplyr::if_else(
          is.finite(commercial_therm_per_worker * commercial_jobs),
          commercial_therm_per_worker * commercial_jobs, commercial_therms
        ),
        industrial_therms = dplyr::if_else(
          is.finite(industrial_therm_per_worker * industrial_jobs),
          industrial_therm_per_worker * industrial_jobs, industrial_therms
        ),


        # electric emissions
        commercial_electricity_emissions_kg_co =
          commercial_mwh * (kg_per_mwh *
            (1 - dplyr::if_else(year < 2040,
              .enviro_factors$GRID_DECARBONIZATION_BASELINE,
              dplyr::if_else((grid_decarb > 1), 1,
                grid_decarb
              )
            )
            )
          ),
        industrial_electricity_emissions_kg_co =
          industrial_mwh * (kg_per_mwh * (1 -
            dplyr::if_else(year < 2040, .enviro_factors$GRID_DECARBONIZATION_BASELINE,
              dplyr::if_else((grid_decarb > 1), 1,
                grid_decarb
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
          na.rm = TRUE
        )
      ) %>%
      dplyr::select(
        geog_name, geog_id,
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

    return(emis_tb)
  }

  emis_bau <-
    emis(
      tb = non_res_tb_bau,
      grid_decarb = 0.6,
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
      .enviro_factors = .enviro_factors
    )

  emis_final <-
    dplyr::right_join(
      emis_bau,
      emis_strategy,
      by = c("geog_name", "geog_id", "year"),
      suffix = c(".bau", ".scen")
    ) %>%
    tidyr::pivot_longer(
      names_to = "var",
      values_to = "value",
      cols = -c(geog_name, geog_id, year)
    ) %>%
    tidyr::separate(
      col = var,
      into = c("var", "scen"),
      sep = "\\."
    ) %>%
    dplyr::ungroup()

  return(emis_final)
}
