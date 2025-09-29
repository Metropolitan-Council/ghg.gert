#' @title Execute building energy scenarios
#' @family buildings
#'
#' @description This function generates the outputs of the building energy module
#'    for the given scenario at the city/township level. It incorporates various
#'    parameters to evaluate and analyze different energy consumption and efficiency
#'    scenarios for both residential and non-residential buildings. Outputs are
#'    provided as a tibble with columns `geog_name`, `geog_id`, `var`, `scen`, `year`, and `value`.
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
#' @param .grid_decarbonization_pct numeric, a value between `0` and `1`.
#'   Default value is `0.6`.
#' @param .grid_emissions table,
#'   Default is `ghg.ccap::grid_emissions`
#'
#' @return [tibble::tibble()].
#'       Returns a table with columns `geog_name`, `geog_id`, `var`, `scen`, `year`, and `value`.
#'       The table is the output of the building energy module, any modification to
#'       the inputs of the building energy module must be specified as an argument
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
run_scenario_building <- function(res_tb = building_data$residential,
                                  non_res_tb = building_energy_data$jobs,
                                  res_tb_bau = building_data$residential,
                                  non_res_tb_bau = building_energy_data$jobs,
                                  run_residential = TRUE,
                                  run_non_residential = FALSE,
                                  .baseline_year = 2022,
                                  .selected_ctu = "all",
                                  .scenario = "alt",

                                  # shared -- may need to break out parameters for res and non-res retrofitting
                                  .retrofit_start_year = 2028,
                                  .retrofit_end_year = 2050,
                                  .heatpump_start_year = 2028,
                                  .heatpump_end_year = 2050,

                                  # residential
                                  .new_homes_to_multifamily_pct = 0.0,
                                  .electrified_buildings_pct = 0.0,
                                  .sf_heat_pump_pct = 0.0,
                                  .mf_heat_pump_pct = 0.0,
                                  .app_elec_start_year = 2028,
                                  .app_elec_end_year = 2050,
                                  .sf_app_elec_pct = 0.0,
                                  .mf_app_elec_pct = 0.0,
                                  .smart_grid_energy_reduction_pct = 0.0,
                                  .new_sf_homes_leed_gold_pct = 0.0,
                                  .new_mf_homes_leed_gold_pct = 0.0,
                                  .existing_sf_retrofit_pct = 0.0,
                                  .existing_mf_retrofit_pct = 0.0,

                                  # non-residential
                                  .new_jobs_leed_gold_pct = 0.0,
                                  .existing_jobs_retrofit_pct = 0.0,
                                  .leed_start_year = 2028,
                                  .jobs_heatpump_pct = 0.0,

                                  # emissions factors and elasticities
                                  .grid_emissions = ghg.ccap::grid_emissions,
                                  .enviro_factors = ghg.ccap::enviro_factors

                                  # Deprecated parameters
                                  # .sf_elec_appliance_pct = 0.0,
                                  # .mf_elec_appliance_pct = 0.0,
                                  # .additional_electrified_residential_buildings_pct = 0.0
                                  # .renewable_ng_res = FALSE,
                                  # .renewable_ng_nonres = FALSE,
                                  # .home_behavior_change_pct = 0.0,
                                  # .single_family_floor_area_growth_pct = 0.05,
                                  # .new_homes_affected_pct = 0.0,
) {
  # browser()
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)
  res_tb_bau <-
    filter_ctu(res_tb_bau, .selected_ctu = .selected_ctu)
  non_res_tb <-
    filter_ctu(non_res_tb, .selected_ctu = .selected_ctu)
  non_res_tb_bau <-
    filter_ctu(non_res_tb_bau, .selected_ctu = .selected_ctu)


  l_names <- c(

    # Residential – efficiency / LEED / retrofits / demand flex
    "new_sf_homes_leed_gold_pct",
    "new_mf_homes_leed_gold_pct",
    "existing_sf_retrofit_pct",
    "existing_mf_retrofit_pct",
    "smart_grid_energy_reduction_pct",

    # Residential – electrification via heatpump
    "sf_heat_pump_pct",
    "mf_heat_pump_pct",
    "electrified_buildings_pct",

    # Non-residential – LEED / retrofits / electrification via heatpump
    "new_jobs_leed_gold_pct",
    "existing_jobs_retrofit_pct",
    "jobs_heatpump_pct"
  )

  l_vals <- list(

    # Residential – efficiency / LEED / retrofits / demand flex
    .new_sf_homes_leed_gold_pct,
    .new_mf_homes_leed_gold_pct,
    .existing_sf_retrofit_pct,
    .existing_mf_retrofit_pct,
    .smart_grid_energy_reduction_pct,

    # Residential – electrification via heatpump
    .sf_heat_pump_pct,
    .mf_heat_pump_pct,
    .electrified_buildings_pct,

    # Non-residential – LEED / retrofits / electrification via heatpump
    .new_jobs_leed_gold_pct,
    .existing_jobs_retrofit_pct,
    .jobs_heatpump_pct
  )

  purrr::map2(l_names, l_vals, check_inputs)

  if (run_residential == TRUE) {
    res <-
      scen_building_residential(
        res_tb = res_tb,
        res_tb_bau = res_tb_bau,
        .selected_ctu = .selected_ctu,
        .scenario = .scenario,
        .baseline_year = .baseline_year,
        .leed_start_year = .leed_start_year,
        .retrofit_start_year = .retrofit_start_year,
        .heatpump_start_year = .heatpump_start_year,
        .retrofit_end_year = .retrofit_end_year,
        .heatpump_end_year = .heatpump_end_year,
        .new_sf_homes_leed_gold_pct = .new_sf_homes_leed_gold_pct,
        .new_mf_homes_leed_gold_pct = .new_mf_homes_leed_gold_pct,
        .existing_sf_retrofit_pct = .existing_sf_retrofit_pct,
        .existing_mf_retrofit_pct = .existing_mf_retrofit_pct,
        .sf_heat_pump_pct = .sf_heat_pump_pct,
        .mf_heat_pump_pct = .mf_heat_pump_pct,
        .enviro_factors = .enviro_factors,
        .grid_emissions = .grid_emissions
      )
  }

  if (run_non_residential == TRUE) {
    non_res <-
      scen_building_non_residential(
        non_res_tb = non_res_tb,
        non_res_tb_bau = non_res_tb_bau,
        .scenario = .scenario,
        .selected_ctu = .selected_ctu,
        .baseline_year = .baseline_year,
        .jobs_heatpump_pct = .jobs_heatpump_pct,
        .heatpump_start_year = .heatpump_start_year,
        .heatpump_end_year = .heatpump_end_year,
        .existing_jobs_retrofit_pct = .existing_jobs_retrofit_pct,
        .retrofit_start_year = .retrofit_start_year,
        .retrofit_end_year = .retrofit_end_year,
        .new_business_leed_gold_pct = .new_business_leed_gold_pct,
        .leed_start_year = .leed_start_year,
        .grid_emissions = ghg.ccap::grid_emissions,
      )
  }


  building_module_output <-
    if (run_residential == TRUE & run_non_residential == TRUE) {
      dplyr::bind_rows(
        (res %>%
          dplyr::mutate(year = as.character(year))),
        (non_res %>%
          dplyr::mutate(year = as.character(year)))
      )
    } else if (run_residential == FALSE) {
      non_res
    } else {
      res
    }

  return(building_module_output)
}
