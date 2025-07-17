#' @title Execute land use scenarios
#' @family land_use
#'
#' @description This function generates the outputs of the land use module
#'    for change in land use plans at the city/township level. It incorporates
#'    residential density inputs from Thrive 2040 and allows cities to make 2050
#'    modifications. On the backend, we will calculate a change in expected density
#'    and allow that to percolate to other sectors. Outputs are
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
run_scenario_land_use <- function(tb_bau = ctu_planned_land_use_residential,
                                  .selected_ctu = "all",
                                  .scenario = "alt"
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
    # non-residential
    # electrification
    "electrified_buildings_pct",
    # smartgrid
    "smart_grid_energy_reduction_pct",
    # residential
    # floor_area
    # "new_homes_to_multifamily_pct",
    "existing_high_efficiency_buildings_pct",
    # "home_behavior_change_pct",
    # "single_family_floor_area_growth_pct",
    # "new_homes_affected_pct",
    "new_sf_homes_leed_gold_pct ",
    "new_mf_homes_leed_gold_pct",
    "existing_sf_retrofit_pct",
    "existing_mf_retrofit_pct",
    # electrification
    "sf_heat_pump_pct",
    "mf_heat_pump_pct"
    # grid
    # "renewable_ng_res",
    # "renewable_ng_nonres"
  )

  l_vals <- list(
    # non-residential
    # electrification
    .electrified_buildings_pct,

    # smartgrid
    .smart_grid_energy_reduction_pct,

    # residential

    # floor_area
    # .new_homes_to_multifamily_pct,
    .existing_high_efficiency_buildings_pct,
    # .home_behavior_change_pct,
    # .single_family_floor_area_growth_pct,
    # .new_homes_affected_pct,
    .new_sf_homes_leed_gold_pct,
    .new_mf_homes_leed_gold_pct,
    .existing_sf_retrofit_pct,
    .existing_mf_retrofit_pct,
    # electrification
    .sf_heat_pump_pct,
    .mf_heat_pump_pct
    # grid
    # .renewable_ng_res,
    # .renewable_ng_nonres
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

  # if (run_non_residential == TRUE) {
  #   non_res <-
  #     scen_building_non_residential(
  #       non_res_tb = non_res_tb,
  #       non_res_tb_bau = non_res_tb_bau,
  #       .selected_ctu = .selected_ctu,
  #       .existing_high_efficiency_buildings_pct = .existing_high_efficiency_buildings_pct,
  #       .electrified_buildings_pct = .electrified_buildings_pct,
  #       .grid_decarbonization_pct = .grid_decarbonization_pct,
  #       .smart_grid_energy_reduction_pct = .smart_grid_energy_reduction_pct,
  #       .renewable_ng_nonres = .renewable_ng_nonres,
  #       .enviro_factors = .enviro_factors
  #     ) %>%
  #     dplyr::mutate(year = as.character(year)) %>%
  #     dplyr::filter(!(var %in% c(
  #       "commercial_electricity_emissions_kg_co",
  #       "industrial_electricity_emissions_kg_co",
  #       "commercial_natural_gas_emissions_kg_co",
  #       "industrial_natural_gas_emissions_kg_co",
  #       "total_industrial_commercial_emissions"
  #     )))
  # }

  # building_module_ouput <-
  #   if (run_residential == TRUE & run_non_residential == TRUE) {
  #     dplyr::bind_rows(
  #       (res %>%
  #         dplyr::mutate(year = as.character(year))),
  #       (non_res %>%
  #         dplyr::mutate(year = as.character(year)))
  #     )
  #   } else if (run_residential == FALSE) {
  #     non_res
  #   } else {
  #     res
  #   }

  building_module_ouput <- res

  return(building_module_ouput)
}
