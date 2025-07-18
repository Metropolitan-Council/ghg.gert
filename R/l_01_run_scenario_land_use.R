#' @title Execute land use scenarios
#' @family land_use
#'
#' @description This function generates the expected density of land use inputs.
#'    It incorporates residential density inputs from Thrive 2040 and allows
#'    cities to make 2050 modifications that in turn change expected density
#'    that can percolate to other sectors. Outputs are
#'    provided as a tibble with columns `geog_name`, `geog_id`, `var`, `scen`, `year`, and `value`.
#'
#' @return [tibble::tibble()].
#'       Returns a table with columns `geog_name`, `geog_id`, `var`, `scen`, `year`, and `value`.
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
#' run_scenario_building(
#'   res_tb = building_data$residential,
#'   non_res_tb = building_data$non_residential,
#'   res_tb_bau = building_data$residential,
#'   non_res_tb_bau = building_data$non_residential,
#'   run_residential = TRUE,
#'   run_non_residential = TRUE,
#'   .selected_ctu = "all",
#'   .enviro_factors = enviro_factors,
#'   .electrified_buildings_pct = 0.40,
#'   .smart_grid_energy_reduction_pct = 1.00,
#'   .new_homes_to_multifamily_pct = 0.50,
#'   .existing_high_efficiency_buildings_pct = 0.80,
#'   .home_behavior_change_pct = 1.00,
#'   .single_family_floor_area_growth_pct = 0.05,
#'   .new_homes_affected_pct = 0.30,
#'   .new_homes_leed_gold_pct = 0.50,
#'   .existing_home_retrofit_pct = 0.80,
#'   .existing_home_ultra_retrofit_pct = 0.20,
#'   .additional_electrified_residential_buildings_pct = 0.45,
#'   .grid_decarbonization_pct = 1
#' )
#' }
#'
#'
run_scenario_land_use <- function(tb = planned_land_use$ctu_planned_land_use_parcel,
                                  tb_strategy = planned_land_use$ctu_planned_land_use_parcel,
                                  .selected_ctu = "all",
                                  .scenario = "alt"
) {
  # browser()
  tb_bau <- filter_ctu(tb, .selected_ctu = .selected_ctu)
  tb_strategy <- filter_ctu(tb_strategy, .selected_ctu = .selected_ctu)

  bau_avg_dens <- sum(tb_bau$unit_mean * tb_bau$acres) / sum(tb_bau$acres)
  strategy_avg_dens <- sum(tb_strategy$unit_mean * tb_strategy$acres) / sum(tb_strategy$acres)

  density_ouput <- data.frame(scenario = c("BAU", .scenario),
                              expected_density = c(bau_avg_dens,
                                                   strategy_avg_dens))

  return(density_ouput)
}
