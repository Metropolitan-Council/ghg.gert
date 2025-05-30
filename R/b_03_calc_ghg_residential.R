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
#' @param res_tb [tibble::tibble()].
#'      Table, table with residential building data.
#' @inheritParams run_module_transportation
#' @inheritParams scen_building_residential
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
#' @examples
#' \dontrun{
#' library(ghg.ccap)
#'
#' calc_ghg_residential(
#'   res_tb = building_data$residential,
#'   res_tb_bau = building_data$residential,
#'   .selected_ctu = "all",
#'   .grid_decarbonization_pct = 1,
#'   .enviro_factors = enviro_factors
#' )
#' }
#' @export
#'
calc_ghg_residential <- function(res_tb,
                                 .selected_ctu,
                                 grid_emissions = grid_emissions,
                                 .enviro_factors = ghg.ccap::enviro_factors) {
  # cli::cli_progress_message("*** calculating residential ghg emissions \n")

  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)

  res_emissions <- res_tb %>%
    left_join(grid_emissions) %>%
    mutate(electricity_emissions = residential_mwh * mt_co2e_per_mwh,
           natural_gas_emissions = residential_mcf * .enviro_factors$MT_CO2E_PER_MCF_NATGAS) %>%
    select(-c(factor_source, mt_co2e_per_mwh))

  return(res_emissions)
}
