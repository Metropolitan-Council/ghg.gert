#' @title Calculate new housing - LEED certified
#' @family buildings
#'
#' @description Calculates the efficiency of new commercial/industrial facilities
#'    built high efficiency (????), considering the proportion of
#'    new homes built to these standards, the difference in single-family
#'    jobs between 2022 and 2050, and the reduction in energy use
#'    intensity due to LEED Gold construction. This function is designed to
#'    estimate the impact of energy-efficient construction on non-residential
#'    greenhouse gas emissions.
#'
#' @param .existing_high_efficiency_buildings_pct numeric,  a value between `0` and `1`.
#'      The percentage of new jobs (as an analogue for new commercial/industrial buildings) subject to high efficiency construction
#'      Default is `0.0`
#'
#' @inheritParams run_scenario_building
#' @inheritParams calc_parking_lot_land_cover
#' @inheritParams run_all_modules
#' @inheritParams run_scenario_land_use
#' @inheritParams filter_ctu
#'
#' @details
#'    Uses the expected growth of jobs according to UrbanSim estimates as an analogue for commercial/industrial building construction
#'
#' @return [tibble::tibble()].
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.ccap)
#'
#' calc_floor_area_leed(
#'   res_tb = building_data$residential,
#'   .selected_ctu = "all",
#'   .new_homes_leed_gold_pct = 0.5,
#'   .enviro_factors = enviro_factors
#' )
#' }
#'
calc_retrofit_efficiency <- function(non_res_tb,
                              .selected_ctu,
                              .existing_high_efficiency_buildings_pct,
                              .high_efficiency_start_year,
                              .high_efficiency_end_year,
                              .enviro_factors = ghg.ccap::enviro_factors) {
  # cli::cli_progress_message("*** calculating floor area LEED Gold certification strategy \n")

  non_res_tb <- filter_ctu(non_res_tb, .selected_ctu = .selected_ctu)

  check_inputs(name = ".existing_high_efficiency_buildings_pct", .existing_high_efficiency_buildings_pct)
  check_inputs(name = ".high_efficiency_start_year ", .high_efficiency_start_year)
  check_inputs(name = ".high_efficiency_end_year ", .high_efficiency_end_year)

  # if (.new_sf_homes_leed_gold_pct == 0) {
  #   #cli::cli_warn("No change in new single family home energy efficiency")
  #   new_sf <- res_tb %>%
  #     dplyr::filter(grepl("single", sp_categories)) %>%
  #     mutate(
  #       new_leed = 0,
  #       new_units = ifelse(value_change_from_base < 0, 0, value_change_from_base),
  #       effective_unit_change_leed = 0
  #     )
  # } else if (.new_sf_homes_leed_gold_pct != 0) {

  new_jobs <- non_res_tb %>%
    dplyr::filter(grepl("single", sp_categories)) %>%
    dplyr::mutate(
      new_jobs = ifelse(value_change_from_base < 0, 0, value_change_from_base),
      new_efficient_jobs = if_else(inventory_year < .high_efficiency_start_year,
                         0,
                         round(new_jobs * .existing_high_efficiency_buildings_pct)
      ),
      new_non_efficient_jobs = new_jobs - new_efficient_jobs
    ) %>%
    pivot_longer(
      cols = c(new_efficient_jobs, new_non_efficient_jobs),
      names_to = "efficiency_description",
      values_to = "efficiency_unit_value"
    )

  # }

  # if (.new_mf_homes_leed_gold_pct == 0) {
  #   cli::cli_warn("No change in new multifamily home energy efficiency")
  #   new_mf <- res_tb %>%
  #     dplyr::filter(grepl("multi", sp_categories)) %>%
  #     mutate(
  #       new_leed = 0,
  #       new_units = ifelse(value_change_from_base < 0, 0, value_change_from_base),
  #       effective_unit_change_leed = 0
  #     )
  # } else if (.new_mf_homes_leed_gold_pct != 0) {

  return(new_jobs)
}


#' @title Calculate housing retrofit
#' @family buildings
#'
#' @description Calculates number of homes targeted for retrofits based on user inputs
#' and CTU housing projections. This function must inherit an object from calc_housing_leed()
#'
#' @param .existing_home_retrofit_pct numeric,  a value between `0` and `1`.
#'      Percentage of existing homes retrofitted to reduce energy usage by *33%*.
#'      Default is `0.0`.
#' @param .existing_home_ultra_retrofit_pct numeric,  a value between `0` and `1`.
#'      Percentage of existing homes retrofitted to reduce energy usage by *66%*.
#'      Default is `0.00`.
#'
#' @inheritParams run_scenario_building
#' @inheritParams calc_parking_lot_land_cover
#' @inheritParams run_all_modules
#' @inheritParams run_scenario_land_use
#' @inheritParams filter_ctu
#'
#' @details Uses the average single family floor area in 2018
#'
#' @return [tibble::tibble()]
#'       A table with columns `ctu_name`, `year`, `var`, and `value`.
#'       Table contains adjusted `single_family_units`,
#'       `single_family_average_floor_area_sqft_ctu`, `multifamily_units`, and
#'       `multifamily_average_floor_area_sqft_county` records for column `var`
#'       when `year == 2040` relative to the residential inputs table.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.ccap)
#'
#' calc_floor_area_retrofit(
#'   res_tb = building_data$residential,
#'   .selected_ctu = "all",
#'   .existing_home_retrofit_pct = 0.80,
#'   .existing_home_ultra_retrofit_pct = 0.20,
#'   .enviro_factors = ghg.ccap::enviro_factors
#' )
#' }
#'
