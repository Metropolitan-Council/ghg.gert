#' @title Run building energy scenarios
#' @family buildings
#'
#' @description Produces the outputs of the building energy module by
#'     city/township for the specified scenario
#'
#' @inheritParams scen_building_residential
#' @inheritParams scen_building_non_residential
#' @param .grid_decarbonization_pct numeric,
#'       a value between `0` and `1`.
#'
#' @return [tibble::tibble()].
#'       Returns a table with columns `ctu_name`, `var`, `scen`, `year`, and `value`.
#'       The table is the output of the building energy module, any modification to
#'       the inputs of the building energy module must be specified as an argument
#'       to the function `run_scenario_building()`
#'
#' @export
#' @importFrom cli cli_progress_message
#' @examples
#' \dontrun{
#'
#' library(ghg.sp)
#' run_scenario_building(
#'   res_tb = building_energy_bau_data$residential,
#'   non_res_tb = building_data$non_residential,
#'   res_tb_bau = building_energy_bau_data$residential,
#'   non_res_tb_bau = building_data$non_residential,
#'   run_residential = TRUE,
#'   run_non_residential = TRUE,
#'   .enviro_factors = enviro_factors,
#'   .electrified_buildings_pct = 0.40,
#'   .non_res_natural_gas_for_water_heating_pct = 0.20,
#'   .non_res_natural_gas_for_space_heating_pct = 0.69,
#'   .commercial_smart_grid_pct = 1.00,
#'   .industrial_smart_grid_pct = 1.00,
#'   .smart_grid_energy_reduction_pct = 1.00,
#'   .new_homes_to_multifamily_pct = 0.50,
#'   .existing_high_efficiency_buildings_pct = 0.80,
#'   .home_behavior_change_pct = 1.00,
#'   .single_family_floor_area_growth_pct = 0.05,
#'   .new_homes_affected_pct = 0.30,
#'   .new_homes_leed_gold_pct = 0.50,
#'   .existing_home_retrofit_pct = 0.80,
#'   .existing_home_ultra_retrofit_pct = 0.20,
#'   .res_natural_gas_for_space_heating_pct = 0.71,
#'   .res_natural_gas_for_water_heating_pct = 0.24,
#'   .additional_electrified_residential_buildings_pct = 0.45,
#'   .grid_decarbonization_pct = 1
#' )
#' }
#'
run_scenario_building <- function(res_tb = building_energy_bau_data$residential,
                                  non_res_tb = building_data$non_residential,
                                  res_tb_bau = building_energy_bau_data$residential,
                                  non_res_tb_bau = building_data$non_residential,
                                  run_residential = TRUE,
                                  run_non_residential = TRUE,
                                  .enviro_factors = enviro_factors,
                                  # selected CTU
                                  .selected_ctu = "all",
                                  # non-residential
                                  # electrification
                                  .electrified_buildings_pct = 0.40,
                                  .non_res_natural_gas_for_water_heating_pct = 0.20,
                                  .non_res_natural_gas_for_space_heating_pct = 0.69,
                                  # smartgrid
                                  .commercial_smart_grid_pct = 1.00,
                                  .industrial_smart_grid_pct = 1.00,
                                  .smart_grid_energy_reduction_pct = 1.00,
                                  # residential
                                  .renewable_ng_res = FALSE,
                                  .renewable_ng_nonres = FALSE,
                                  # floor_area
                                  .new_homes_to_multifamily_pct = 0.50,
                                  .existing_high_efficiency_buildings_pct = 0.80,
                                  .home_behavior_change_pct = 1.00,
                                  .single_family_floor_area_growth_pct = 0.05,
                                  .new_homes_affected_pct = 0.30,
                                  .new_homes_leed_gold_pct = 0.50,
                                  .existing_home_retrofit_pct = 0.80,
                                  .existing_home_ultra_retrofit_pct = 0.20,
                                  # electrification
                                  .res_natural_gas_for_space_heating_pct = 0.71,
                                  .res_natural_gas_for_water_heating_pct = 0.24,
                                  .additional_electrified_residential_buildings_pct = 0.45,
                                  # grid
                                  .grid_decarbonization_pct = 1) {
  cli::cli_progress_message("\n  === RUNNING BUILDING ENERGY MODULE === \n")
  cli::cli_progress_message(msg = c("...... selected CTU:", .selected_ctu, "\n"))
  cli::cli_progress_message(c("...... percent of electrified buildings:", .electrified_buildings_pct, "\n"))
  cli::cli_progress_message(c("...... percent of non-residential natural gas for water heating:", .non_res_natural_gas_for_water_heating_pct, "\n"))
  cli::cli_progress_message(c("...... percent of non-residential natural gas for space heating:", .non_res_natural_gas_for_space_heating_pct, "\n"))
  cli::cli_progress_message(c("...... percent of commercial smart grid:", .commercial_smart_grid_pct, "\n"))
  cli::cli_progress_message(c("...... percent of industrial smart grid:", .industrial_smart_grid_pct, "\n"))
  cli::cli_progress_message(c("...... percent of smart grid energy reduction:", .smart_grid_energy_reduction_pct, "\n"))
  cli::cli_progress_message(c("...... percent of new homes to multifamily:", .new_homes_to_multifamily_pct, "\n"))
  cli::cli_progress_message(c("...... percent of existing high efficiency buildings:", .existing_high_efficiency_buildings_pct, "\n"))
  cli::cli_progress_message(c("...... percent of home behavior change:", .home_behavior_change_pct, "\n"))
  cli::cli_progress_message(c("...... percent of single family floor area growth:", .single_family_floor_area_growth_pct, "\n"))
  cli::cli_progress_message(c("...... percent of new homes affected:", .new_homes_affected_pct, "\n"))
  cli::cli_progress_message(c("...... percent of new homes leed gold:", .new_homes_leed_gold_pct, "\n"))
  cli::cli_progress_message(c("...... percent of existing home retrofit:", .existing_home_retrofit_pct, "\n"))
  cli::cli_progress_message(c("...... percent of existing home ultra retrofit:", .existing_home_ultra_retrofit_pct, "\n"))
  cli::cli_progress_message(c("...... percent of residential natural gas for space heating:", .res_natural_gas_for_space_heating_pct, "\n"))
  cli::cli_progress_message(c("...... percent of residential natural gas for water heating:", .res_natural_gas_for_water_heating_pct, "\n"))
  cli::cli_progress_message(c("...... percent of additional electrified residential buildings:", .additional_electrified_residential_buildings_pct, "\n"))
  cli::cli_progress_message(c("...... percent of grid decarbonization:", .grid_decarbonization_pct, "\n"))
  cli::cli_progress_message("========================================== \n")

  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)
  res_tb_bau <-
    filter_ctu(res_tb_bau, .selected_ctu = .selected_ctu)
  non_res_tb <-
    filter_ctu(non_res_tb, .selected_ctu = .selected_ctu)
  non_res_tb_bau <-
    filter_ctu(non_res_tb_bau, .selected_ctu = .selected_ctu)

  l_names <- c(
    # non-residential
    # electrification
    "electrified_buildings_pct",
    "non_res_natural_gas_for_water_heating_pct",
    "non_res_natural_gas_for_space_heating_pct",
    # smartgrid
    "commercial_smart_grid_pct",
    "industrial_smart_grid_pct",
    "smart_grid_energy_reduction_pct",
    # residential
    # floor_area
    "new_homes_to_multifamily_pct",
    "existing_high_efficiency_buildings_pct",
    "home_behavior_change_pct",
    "single_family_floor_area_growth_pct",
    "new_homes_affected_pct",
    "new_homes_leed_gold_pct ",
    "existing_home_retrofit_pct",
    "existing_home_ultra_retrofit_pct",
    # electrificatio
    "res_natural_gas_for_space_heating_pct",
    "res_natural_gas_for_water_heating_pct",
    "additional_electrified_residential_buildings_pct",
    # grid
    "grid_decarbonization_pct",
    "renewable_ng_res",
    "renewable_ng_nonres"
  )

  l_vals <- list(
    # non-residential
    # electrification
    .electrified_buildings_pct,
    .non_res_natural_gas_for_water_heating_pct,
    .non_res_natural_gas_for_space_heating_pct,

    # smartgrid
    .commercial_smart_grid_pct,
    .industrial_smart_grid_pct,
    .smart_grid_energy_reduction_pct,
    # residential
    # floor_area
    .new_homes_to_multifamily_pct,
    .existing_high_efficiency_buildings_pct,
    .home_behavior_change_pct,
    .single_family_floor_area_growth_pct,
    .new_homes_affected_pct,
    .new_homes_leed_gold_pct,
    .existing_home_retrofit_pct,
    .existing_home_ultra_retrofit_pct,
    # electrification
    .res_natural_gas_for_space_heating_pct,
    .res_natural_gas_for_water_heating_pct,
    .additional_electrified_residential_buildings_pct,
    # grid
    .grid_decarbonization_pct,
    .renewable_ng_res,
    .renewable_ng_nonres
  )

  purrr::map2(l_names, l_vals, check_inputs)

  if (run_residential == TRUE) {
    res <-
      scen_building_residential(
        res_tb = res_tb,
        res_tb_bau = res_tb_bau,
        .selected_ctu = .selected_ctu,
        .renewable_ng_res = .renewable_ng_res,
        .new_homes_to_multifamily_pct = .new_homes_to_multifamily_pct,
        .single_family_floor_area_growth_pct = .single_family_floor_area_growth_pct,
        .home_behavior_change_pct = .home_behavior_change_pct,
        .new_homes_affected_pct = .new_homes_affected_pct,
        .new_homes_leed_gold_pct = .new_homes_leed_gold_pct,
        .existing_home_retrofit_pct = .existing_home_retrofit_pct,
        .existing_home_ultra_retrofit_pct = .existing_home_ultra_retrofit_pct,
        .additional_electrified_residential_buildings_pct = .additional_electrified_residential_buildings_pct,
        .res_natural_gas_for_space_heating_pct = .res_natural_gas_for_space_heating_pct,
        .res_natural_gas_for_water_heating_pct = .res_natural_gas_for_water_heating_pct,
        .grid_decarbonization_pct = .grid_decarbonization_pct,
        .enviro_factors = .enviro_factors
      )
  }
  if (run_non_residential == TRUE) {
    non_res <-
      scen_building_non_residential(
        non_res_tb = non_res_tb,
        non_res_tb_bau = non_res_tb_bau,
        .selected_ctu = .selected_ctu,
        .existing_high_efficiency_buildings_pct = .existing_high_efficiency_buildings_pct,
        .electrified_buildings_pct = .electrified_buildings_pct,
        .non_res_natural_gas_for_water_heating_pct = .non_res_natural_gas_for_water_heating_pct,
        .non_res_natural_gas_for_space_heating_pct = .non_res_natural_gas_for_space_heating_pct,
        .commercial_smart_grid_pct = .commercial_smart_grid_pct,
        .industrial_smart_grid_pct = .industrial_smart_grid_pct,
        .grid_decarbonization_pct = .grid_decarbonization_pct,
        .smart_grid_energy_reduction_pct = .smart_grid_energy_reduction_pct,
        .renewable_ng_nonres = .renewable_ng_nonres,
        .enviro_factors = .enviro_factors
      )
  }

  building_module_ouput <-
    if (run_residential == TRUE & run_non_residential == TRUE) {
      bind_rows(res, non_res)
    } else if (run_residential == FALSE) {
      non_res
    } else {
      res
    }
  return(building_module_ouput)
}
