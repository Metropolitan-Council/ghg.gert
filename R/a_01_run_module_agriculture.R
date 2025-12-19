#' @title Execute agriculture module
#' @family agriculture
#'
#' @description This function generates the outputs of the agriculture module
#'    for the given scenario at the county/city/township level. It splits action into
#'    livestock, manure, and crop soils.
#'
#' @inheritParams scen_building_residential
#' @inheritParams scen_building_non_residential
#' @inheritParams calc_residential_renewable_ng
#' @inheritParams calc_ghg_non_residential
#' @inheritParams calc_energy_residential
#' @inheritParams calc_residential_retrofit
#' @inheritParams calc_housing_leed
#' @inheritParams adj_unit_counts
#' @inheritParams run_all_modules
#'
#'
#' @return [tibble::tibble()].
#'       Returns a table with columns `geog_name`, `geog_id`, `var`, `scen`, `year`, and `value`.
#'       The table is the output of the agriculture module, any modification to
#'       the inputs of the agriculture module must be specified as an argument
#'       to the function `run_scenario_building()`
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
run_module_agriculture <- function(livestock_tb = agriculture_activity_data$livestock,
                                  fertilizer_tb = agriculture_activity_data$fertilizer,
                                  crops_tb = agriculture_activity_data$crops,
                                  # adjust_livestock_baseline = FALSE,
                                  adjust_cropland_baseline = FALSE,
                                  run_manure = TRUE,
                                  run_fertilizer = TRUE,
                                  run_crops = TRUE,
                                  .baseline_year = 2022,
                                  .ag_land_perserved = 1,
                                  .selected_ctu = "all",
                                  .scenario = "alt",
                                  # # manure
                                  # .manure_start_year = 2028,
                                  # .manure_handling = 0.0,
                                  # fertilizer
                                  .smart_fertilizer_start_year = 2028,
                                  .smart_fertilizer_current = 0.0,
                                  .smart_fertilizer_goal = 0.0,
                                  # crops
                                  .cover_crops_start_year = 2028,
                                  .cover_crops_current = 0.0,
                                  .cover_crops_goal = 0.0,
                                  .no_till_start_year = 2028,
                                  .no_till_current = 0.0,
                                  .no_till_goal = 0.0
) {

  livestock_tb <- filter_ctu(livestock_tb, .selected_ctu = .selected_ctu)

  fertilizer_tb <- filter_ctu(fertilizer_tb, .selected_ctu = .selected_ctu)
  crops_tb <-filter_ctu(crops_tb, .selected_ctu = .selected_ctu)

  # l_names <- c(
  #   # manure
  #   "manure_management_start_year",
  #   "manure_management",
  #   # fertilizer
  #   "smart_fertilizer_start_year",
  #   "smart_fertilizer_application",
  #   # crops
  #   "cover_crops_start_year",
  #   "cover_crops",
  #   "no_till_start_year",
  #   "no_till_agriculture"
  # )
  #
  # l_vals <- list(
  #   # manure
  #   .manure_start_year,
  #   .manure_handling,
  #   # fertilizer
  #   .smart_fertilizer_start_year ,
  #   .smart_fertilizer,
  #   # crops
  #   .cover_crops_start_year,
  #   .cover_crops,
  #   .no_till_start_year,
  #   .no_till
  # )
  #
  # purrr::map2(l_names, l_vals, check_inputs)

  if (run_manure = =TRUE) {
    manure <-
      calculate_manure_emissions(
        agriculture_variables = ghg.ccap::agriculture_variables,
        run_manure = .run_manure,
        gwp_list = ghg.ccap::gwp_list,
        .selected_ctu = .selected_ctu
      )
  }

  if (run_fertilizer == TRUE) {
    fertilizer <-
      run_module_croplands(
        agriculture_variables = ghg.ccap::agriculture_variables,
        run_manure = .run_manure,
        gwp_list = ghg.ccap::gwp_list,
        .selected_ctu = .selected_ctu
      )
  }


  building_module_ouput <- res

  return(building_module_ouput)
}
