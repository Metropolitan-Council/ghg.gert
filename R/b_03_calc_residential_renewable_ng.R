#' @title Calculate Residential Renewable Natural Gas Impact
#' @family residential
#' @family buildings
#'
#' @description This function estimates the impact of transitioning from natural gas
#'    to renewable natural gas (RNG) on residential building emissions.
#'    The function takes into account the differences in emissions between
#'    natural gas and RNG, and computes the resulting changes in
#'    greenhouse gas (GHG) emissions for the selected city or township..
#'
#' @inheritParams run_scenario_building
#' @inheritParams filter_ctu
#' @param .renewable_ng_res logical, documentation needed
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_residential_renewable_ng(
#'   res_tb = calc_ghg_residential(
#'    res_tb = building_data$residential,
#'    res_tb_bau = building_data$residential,
#'    .selected_ctu = "all",
#'    .grid_decarbonization_pct = 1,
#'    .enviro_factors = enviro_factors
#'   ),
#'.  selected_ctu = "all",
#'  .enviro_factors = enviro_factors
#'  )
#' }
#'
#'
calc_residential_renewable_ng <- function(res_tb,
                                          .selected_ctu,
                                          .renewable_ng_res,
                                          .enviro_factors = enviro_factors) {

  # cli::cli_progress_message("*** calculating residential renewable natural gas strategy \n")

  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)

  if (.renewable_ng_res == TRUE) {
    new_res_tb <-
      res_tb %>%
      tidyr::pivot_wider(., names_from = c(var, scen, year),
                         names_sep = ".",
                         values_from = value) %>%
      dplyr::mutate(
        reduced_therms.scen.2040 =
          (residential_therms.bau.2040 - residential_therms.scen.2040),
        residential_natural_gas_emissions_kg_co.scen.2040 =
          (residential_therms.bau.2040 -
            (
              reduced_therms.scen.2040 - (78 * population.bau.2040)
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
      ) %>%
      ungroup()
  } else {
    new_res_tb <- res_tb
  }

  return(new_res_tb)

}
