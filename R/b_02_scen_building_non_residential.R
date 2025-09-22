#' @title Calculate non-residential building strategies
#' @family buildings, non-residential
#'
#' @description This function estimates the emissions of non-residential buildings under a user defined
#'    decarbonization scenario. It takes into account strategies such as energy efficiency improvements,
#'    electrification of heating systems, grid decarbonization, and renewable natural gas adoption.
#'
#'
#' @note To run the Building Energy Module, refer to function `run_scenario_building()`
#'     For more details, see `vignette("building_energy_module_inputs_residential")`
#'
#' @param non_res_tb [tibble::tibble()], data table with non-residential building attributes (jobs).
#'      Package provided dataset `building_energy$non_residential` is suitable and the
#'      default value.
#'     `non_res_tb_bau` is only used for the "business as usual" scenario, in contrast
#'     `non_res_tb` is used as the input for the decarbonization scenario.
#' @param non_res_tb_bau [tibble::tibble()]. Data table with non residential building attributes (jobs).
#'     Package provided dataset `building_energy$non_residential` is suitable and
#'     the default value. `non_res_tb_bau` is only used for the "business as usual"
#'     scenario, in contrast `non_res_tb` is used as the input for the decarbonization scenario.
#'
#' @inheritParams calc_electrify_residential_heating
#' @inheritParams calc_ghg_non_residential
#' @inheritParams calc_energy_non_residential
#' @inheritParams scen_building_non_residential
#' @inheritParams run_scenario_building
#' @inheritParams calc_vmt_forecast
#' @inheritParams run_all_modules
#' @inheritParams calc_housing_leed
#' @inheritParams calc_non_residential_retrofit
#'
#'
#' @return [tibble::tibble()], Data table with columns
#'     `geog_name`, `var`, `value`, `scen`, and `year`.
#'
#'      @field `geog_name` character,  Name of the city/township.
#'      @field `year` numeric, Year.
#'      @field `var` character,. Can be `non_residential_mwh`, `non_residential_therms`,
#'           `residential_electricity_emissions_kg_co`,
#'           or `residential_natural_gas_emissions_kg_co`.
#'      @field value, numeric. The numeric value of `var`.
#'      @field scen, character. One of  `bau` or `scenario`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.ccap)
#'
#' scen_building_non_residential(
#'    non_res_tb = non_res_tb,
#'    non_res_tb_bau = non_res_tb_bau,
#'    .selected_ctu = "all"
#'    .existing_high_efficiency_buildings_pct = 0.25
#'    .electrified_buildings_pct = 0.5
#'    .enviro_factors = .enviro_factors,
#'    .grid_emissions = .grid_emissions
#'   )
#'
#' }
scen_building_non_residential <- function(non_res_tb = non_res_tb,
                                      non_res_tb_bau = non_res_tb_bau,
                                      .scenario = "",
                                      .selected_ctu,
                                      .baseline_year,
                                      .electrified_buildings_start_year,
                                      .electrified_buildings_end_year,
                                      .electrified_buildings_pct,
                                      .high_efficiency_start_year,
                                      .high_efficiency_end_year,
                                      .existing_high_efficiency_buildings_pct,
                                      .new_business_leed_gold_pct,
                                      .leed_start_year,
                                      .grid_emissions = ghg.ccap::grid_emissions,
                                      .enviro_factors = ghg.ccap::enviro_factors) {
  # cli::cli_progress_message("** compiling nonresidential strategies \n")

  # STILL NEED TO RECTIFY FUNCTION COMPOSITION
  # Apply Imagine community designation-specific per-job mwh/mcf factors to generate scenario estimates
  # Based on 'electrification' in comstock_summaries, derived from p
  tb02 <- calc_electrified_buildings(
    non_res_tb = non_res_tb,
    .selected_ctu = .selected_ctu,
    .electrified_buildings_start_year = .electrified_buildings_start_year,
    .electrified_buildings_end_year = .electrified_buildings_end_year,
    .electrified_buildings_pct = .electrified_buildings_pct,
    .enviro_factors = .enviro_factors
  )

  # B.R4 + BR5 (Retrofit Homes)
  tb03 <- calc_business_retrofit(
    non_res_tb = non_res_tb,
    .selected_ctu = .selected_ctu,
    .existing_jobs_retrofit_pct = .existing_jobs_retrofit_pct,
    .retrofit_start_year = .retrofit_start_year,
    .retrofit_end_year = .retrofit_end_year,
    .enviro_factors = .enviro_factors
  )

  tb04 <- bind_rows(
    tb02,
    tb03
  )

  # repeat with no changes for BAU scenario
  tb05 <- calc_electrified_buildings(
    non_res_tb = non_res_tb,
    .selected_ctu = .selected_ctu,
    .electrified_buildings_start_year = .electrified_buildings_start_year,
    .electrified_buildings_end_year = .electrified_buildings_end_year,
    .electrified_buildings_pct = 0.0,
    .enviro_factors = .enviro_factors
  )

  tb06 <- calc_high_efficiency(
    non_res_tb = non_res_tb,
    .selected_ctu = .selected_ctu,
    .high_efficiency_start_year = .high_efficiency_start_year,
    .high_efficiency_end_year = .high_efficiency_end_year,
    .existing_high_efficiency_buildings_pct = 0.0,
    .enviro_factors = .enviro_factors
  )

  tb07 <- bind_rows(
    tb05,
    tb06
  )

  tb09 <- calc_energy_non_residential(
    non_res_tb = tb04,
    non_res_tb_bau = tb07,
    .scenario = .scenario,
    .baseline_year = .baseline_year,
    .heatpump_start_year = .heatpump_start_year,
    .heatpump_end_year = .heatpump_end_year,
    .jobs_heatpump_pct = .jobs_heatpump_pct,
    .selected_ctu = .selected_ctu,
    .enviro_factors = .enviro_factors
  )

  tb_out <- calc_ghg_non_residential(
    non_res_energy = tb09,
    .selected_ctu = .selected_ctu,
    .grid_emissions = .grid_emissions,
    .enviro_factors = .enviro_factors
  )


  return(tb_out)
}
