#' @title Execute building energy scenarios
#' @family buildings
#'
#' @description This function generates the outputs of the building energy module
#'    for the given scenario at the city/township level. It incorporates various
#'    parameters to evaluate and analyze different energy consumption and efficiency
#'    scenarios for both residential and non-residential buildings.
#'
#' @inheritParams scen_building_residential
#' @inheritParams scen_building_non_residential
#' @inheritParams calc_ghg_non_residential
#' @inheritParams calc_energy_residential
#' @inheritParams calc_residential_retrofit
#' @inheritParams calc_housing_leed
#' @inheritParams adj_unit_counts
#' @inheritParams run_all_modules
#'
#' @return [tibble::tibble()] with columns `geog_name`, `geog_id`,
#'   `emissions_year`, `scenario`, `sector`,
#'   `elec_mwh`, `natgas_mcf`, `propane_mmbtu`, `kerosene_mmbtu`,
#'   `electricity_emissions`, `natural_gas_emissions`, `liquid_fuel_emissions`.
#'   Non-residential rows have `0` for propane/kerosene/liquid fuel columns.
#' @param .grid_emissions table,
#'   Default is `ghg.gert::grid_emissions`
#'
#' @export
#' @importFrom cli cli_progress_message
#' @examples
#' \dontrun{
#' library(ghg.ccap)
#'
#' library(ghg.gert)
#' run_scenario_building(
#'   res_tb = building_energy_data$residential,
#'   non_res_tb = building_energy_data$jobs,
#'   res_tb_bau = building_energy_data$residential,
#'   non_res_tb_bau = building_energy_data$jobs,
#'   run_residential = TRUE,
#'   run_non_residential = TRUE,
#'   .selected_ctu = "all",
#'   .sf_heatpump_pct = 0.10,
#'   .existing_sf_retrofit_pct = 0.80
#'   .enviro_factors = enviro_factors,
#'   .home_behavior_change_pct = 1.00,
#'   .single_family_floor_area_growth_pct = 0.05,
#'   .new_homes_affected_pct = 0.30,
#'   .new_homes_leed_gold_pct = 0.50,
#'   .existing_home_retrofit_pct = 0.80,
#'   .existing_home_ultra_retrofit_pct = 0.20,
#'   .additional_electrified_residential_buildings_pct = 0.45
#' )
#' }
#'
run_scenario_building <- function(res_tb = building_energy_data$residential,
                                  non_res_tb = building_energy_data$jobs,
                                  res_tb_bau = building_energy_data$residential,
                                  non_res_tb_bau = building_energy_data$jobs,
                                  run_residential = TRUE,
                                  run_non_residential = FALSE,
                                  .baseline_year = 2022,
                                  .selected_ctu,
                                  .scenario = "alt",
                                  # shared
                                  .density_output,
                                  .leed_start_year = 2028,
                                  .retrofit_start_year = 2028,
                                  .retrofit_end_year = 2050,
                                  .heatpump_start_year = 2028,
                                  .heatpump_end_year = 2050,
                                  # residential
                                  .sf_heatpump_pct = 0.0,
                                  .mf_heatpump_pct = 0.0,
                                  .app_elec_start_year = 2028,
                                  .app_elec_end_year = 2050,
                                  .sf_app_elec_pct = 0.0,
                                  .mf_app_elec_pct = 0.0,
                                  .new_sf_homes_leed_gold_pct = 0.0,
                                  .new_mf_homes_leed_gold_pct = 0.0,
                                  .existing_sf_retrofit_pct = 0.0,
                                  .existing_mf_retrofit_pct = 0.0,
                                  # non-residential
                                  .new_jobs_leed_gold_pct = 0.0,
                                  .existing_jobs_retrofit_pct = 0.0,
                                  .jobs_heatpump_pct = 0.0) {
  res_tb     <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)
  res_tb_bau <- filter_ctu(res_tb_bau, .selected_ctu = .selected_ctu)
  non_res_tb     <- filter_ctu(non_res_tb, .selected_ctu = .selected_ctu)
  non_res_tb_bau <- filter_ctu(non_res_tb_bau, .selected_ctu = .selected_ctu)
                                  .jobs_heatpump_pct = 0.0,
                                  # emissions factors and elasticities
                                  .grid_emissions = ghg.gert::grid_emissions,
                                  .enviro_factors = ghg.gert::enviro_factors) {
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)
  res_tb_bau <-
    filter_ctu(res_tb_bau, .selected_ctu = .selected_ctu)
  non_res_tb <-
    filter_ctu(non_res_tb, .selected_ctu = .selected_ctu)
  non_res_tb_bau <-
    filter_ctu(non_res_tb_bau, .selected_ctu = .selected_ctu)

  # input validation ----

  l_names <- c(
    "new_sf_homes_leed_gold_pct",
    "new_mf_homes_leed_gold_pct",
    "existing_sf_retrofit_pct",
    "existing_mf_retrofit_pct",

    # Residential – electrification via heatpump
    "sf_heatpump_pct",
    "mf_heatpump_pct",

    # Non-residential – LEED / retrofits / electrification via heatpump
    "new_jobs_leed_gold_pct",
    "existing_jobs_retrofit_pct",
    "jobs_heatpump_pct"
  )

  l_vals <- list(
    .new_sf_homes_leed_gold_pct,
    .new_mf_homes_leed_gold_pct,
    .existing_sf_retrofit_pct,
    .existing_mf_retrofit_pct,

    # Residential – electrification via heatpump
    .sf_heatpump_pct,
    .mf_heatpump_pct,

    # Non-residential – LEED / retrofits / electrification via heatpump
    .new_jobs_leed_gold_pct,
    .existing_jobs_retrofit_pct,
    .jobs_heatpump_pct
  )

  purrr::map2(l_names, l_vals, check_inputs)

  # residential ----

  if (run_residential == TRUE) {
    res <-
      scen_building_residential(
        res_tb = res_tb,
        res_tb_bau = res_tb_bau,
        .selected_ctu = .selected_ctu,
        .scenario = .scenario,
        .density_output = .density_output,
        .baseline_year = .baseline_year,
        .leed_start_year = .leed_start_year,
        .retrofit_start_year = .retrofit_start_year,
        .retrofit_end_year = .retrofit_end_year,
        .heatpump_start_year = .heatpump_start_year,
        .heatpump_end_year = .heatpump_end_year,
        .new_sf_homes_leed_gold_pct = .new_sf_homes_leed_gold_pct,
        .new_mf_homes_leed_gold_pct = .new_mf_homes_leed_gold_pct,
        .existing_sf_retrofit_pct = .existing_sf_retrofit_pct,
        .existing_mf_retrofit_pct = .existing_mf_retrofit_pct,
        .sf_heatpump_pct = .sf_heatpump_pct,
        .mf_heatpump_pct = .mf_heatpump_pct
      ) %>%
      dplyr::mutate(sector = "Residential") %>%
      dplyr::rename(
        elec_mwh       = residential_mwh,
        natgas_mcf     = residential_mcf,
        propane_mmbtu  = residential_propane_mmbtu,
        kerosene_mmbtu = residential_kerosene_mmbtu
      )
  }

  # non-residential ----

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
        .new_jobs_leed_gold_pct = .new_jobs_leed_gold_pct,
        .leed_start_year = .leed_start_year
        .leed_start_year = .leed_start_year,
        .grid_emissions = ghg.gert::grid_emissions,
      ) %>%
      dplyr::mutate(
        sector = "Non-residential",
        propane_mmbtu        = 0,
        kerosene_mmbtu       = 0,
        liquid_fuel_emissions = 0
      ) %>%
      dplyr::rename(
        elec_mwh   = non_residential_mwh,
        natgas_mcf = non_residential_mcf
      )
  }

  # combine ----

  building_module_output <-
    if (run_residential & run_non_residential) {
      dplyr::bind_rows(res, non_res)
    } else if (!run_residential) {
      non_res
    } else {
      res
    }

  return(building_module_output)
}
