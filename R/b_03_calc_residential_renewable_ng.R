#' @title Calculate residential renewable natural gas
#' @family residential
#' @family buildings
#'
#' @description Calculates the impact on residential
#'       building emissions from transitioning natural gas to renewable natural gas.
#'
#' @inheritParams run_scenario_building
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' ghg.sp::calc_residential_renewable_ng(
#'   res_tb = ghg.sp::calc_ghg_residential(
#'     res_tb = building_energy_bau_data$residential,
#'     res_tb_bau = building_energy_bau_data$residential,
#'     .selected_ctu = "all",
#'     .grid_decarbonization_pct = 1,
#'     .enviro_factors = enviro_factors
#'   ),
#'   .enviro_factors = enviro_factors
#' )
#' }
calc_residential_renewable_ng <- function(res_tb,
                                          .selected_ctu,
                                          .enviro_factors = .enviro_factors) {
  cat("*** calculating residential renewable natural gas strategy \n")
  new_res_tb <-
    res_tb %>%
    tidyr::pivot_wider(., names_from = c(var, scen, year), names_sep = ".", values_from = value) %>%
    dplyr::mutate(
      reduced_therms =
        (residential_therms.bau.2040
        - residential_therms.scen.2040)
    ) %>%
    dplyr::mutate(
      residential_natural_gas_emissions_kg_co.scen.2040 =
        (residential_therms.bau.2040 -
          (
            reduced_therms - (78 * population.bau.2040)
          )) *
          .enviro_factors$KG_CO2E_PER_THERM_FORECAST
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
    )

  return(new_res_tb)
}
