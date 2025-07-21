#' @title Execute land use scenarios
#' @family land_use
#'
#' @description This function generates the expected density of land use inputs.
#'    It incorporates residential density inputs from Thrive 2040 and allows
#'    cities to make 2050 modifications that in turn change expected density
#'    that can percolate to other sectors. Outputs are
#'    provided as a tibble with columns `geog_name`, `scen`, `expected_density`.
#'
#' @return [tibble::tibble()].
#'       Returns a table with columns `geog_name`, `scen`, `expected_density`..
#'       The table is the output of the land use module, any modification to
#'       the inputs of the land use module must be specified as an argument
#'       to the function `run_scenario_land_use()`
#'
#' @export
#' @importFrom cli cli_progress_message
#' @examples
#' \dontrun{
#'
#' library(ghg.ccap)
#'
#'
run_scenario_land_use <- function(tb = planned_land_use$ctu_planned_land_use_parcel,
                                  tb_strategy = NULL,
                                  .selected_ctu = "all",
                                  .scenario = "alt"
) {
  # browser()
  tb_bau <- filter_ctu(tb, .selected_ctu = .selected_ctu)

  if(is.null(tb_strategy)){
  tb_strategy <- filter_ctu(
    planned_land_use$ctu_planned_land_use_parcel,
    .selected_ctu = .selected_ctu) } else {
    tb_strategy <- land_use_update(tb_bau = tb_bau,
                                   tb_strategy = tb_strategy,
                    .selected_ctu = .selected_ctu,
                    .scenario = .scenario
    )
  }

  bau_avg_dens <- sum(tb_bau$unit_mean * tb_bau$acres) / sum(tb_bau$acres)
  strategy_avg_dens <- sum(tb_strategy$unit_mean * tb_strategy$acres) / sum(tb_strategy$acres)

  density_output <- data.frame(geog_name = .selected_ctu,
    scen = c("BAU", .scenario),
    expected_density = c(bau_avg_dens,
                         strategy_avg_dens))

  return(density_output)
}
