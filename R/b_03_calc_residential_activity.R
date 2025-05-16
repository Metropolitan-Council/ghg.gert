#' @title Calculate residential building mwh
#' @family buildings
#' @family residential
#' @family emissions
#'
#' @description Estimates total energy demand
#'      from the residential building sector by city/township
#'      for the user-specified scenario, and the business-as-usual scenario.
#'
#' @note `calc_mwh_residential()` estimates the building energy demand and emissions
#'      based on the housing efficiency assumptions. For a function that compiles all
#'      residential strategies refer to [`scen_residential_building()`].
#'
#' @param res_tb [tibble::tibble()].
#'      Table, table with residential building data.
#' @inheritParams run_scenario_transportation
#' @inheritParams scen_building_residential
#'
#' @return [tibble::tibble()].
#'    A table with columns
#'    `ctu_name`,
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
calc_mwh_residential <- function(res_tb,
                                 res_tb_bau,
                                 mwh_coefficients = mwh_coefficients,
                                 .selected_ctu = .selected_ctu,
                                 .enviro_factors = ghg.ccap::enviro_factors) {
  # cli::cli_progress_message("*** calculating residential ghg emissions \n")

  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)
  res_tb_bau <- filter_ctu(res_tb_bau, .selected_ctu = .selected_ctu) %>%
    mutate(effective_unit_change = 0)

  mwh_calc <- function(tb,
                   mwh_coefficients = mwh_coefficients) {
    mwh_tb <- tb %>%
      left_join(mwh_coefficients,
                by = c("sp_categories" = "var")) %>%
      mutate(residential_mwh = mwh_per_unit * (value + effective_unit_change)) %>%
      dplyr::group_by(geog_name, geog_id, inventory_year) %>%
      dplyr::summarize(residential_mwh = sum(residential_mwh)) %>%
      dplyr::select(
        geog_name,
        inventory_year,
        geog_id,
        residential_mwh
      )

    return(mwh_tb)
  }

  mwh_bau <-
    mwh_calc(
      tb = res_tb_bau,
      mwh_coefficients = mwh_coefficients
    )

  mwh_strategy <-
    mwh_calc(
      tb = res_tb,
      mwh_coefficients = mwh_coefficients
    )

  mwh_final <-bind_rows(
    mwh_bau %>%
      mutate(scenario = "bau"),
    mwh_strategy %>%
      mutate(scenario = "strategy")
  )

  return(mwh_final)
}
