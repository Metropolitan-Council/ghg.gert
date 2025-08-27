#' @title Calculate residential building emissions
#' @family buildings
#' @family residential
#' @family emissions
#'
#' @description Estimates total energy demand
#'      and emissions from the residential building sector by city/township
#'      for the user-specified scenario, and the business-as-usual scenario.
#'
#' @note `calc_ghg_residential()` estimates the building energy demand and emissions
#'      based on the floor area assumptions. For a function that compiles all
#'      residential strategies refer to [`scen_residential_building()`].
#'
#' @param res_energy [tibble::tibble()].
#'      Table, table with residential energy,
#'      including columns `residential_mwh`, `residential_mcf`.
#' @inheritParams run_module_transportation
#' @inheritParams scen_building_residential
#' @inheritParams calc_energy_residential
#'
#'
#' @return [tibble::tibble()].
#'    A table with columns
#'    `geog_name`,
#'    `year`,
#'    `population`,
#'    `residential_mwh`,
#'    `residential_electricity_emissions_kg_co`,
#'    `residential_therms`, and
#'    `residential_natural_gas_emissions_kg_co`
#'
#' @export
#'
calc_ghg_residential <- function(res_energy,
                                 .selected_ctu,
                                 .grid_emissions = ghg.ccap::grid_emissions,
                                 .enviro_factors = ghg.ccap::enviro_factors) {
  # cli::cli_progress_message("*** calculating residential ghg emissions \n")

  # browser()
  res_energy <- filter_ctu(res_energy, .selected_ctu = .selected_ctu)

  res_emissions <- res_energy %>%
    dplyr::left_join(.grid_emissions, by = c("inventory_year")) %>%
    dplyr::mutate(
      electricity_emissions = residential_mwh * mt_co2e_per_mwh,
      natural_gas_emissions = residential_mcf * .enviro_factors$MT_CO2E_PER_MCF_NATGAS
    ) %>%
    dplyr::select(-c(factor_source, mt_co2e_per_mwh))

  return(res_emissions)
}
