#' @title Calculate new housing - LEED certified
#' @family buildings
#'
#' @description Calculates the efficiency of single-family homes
#'    built in accordance with LEED Gold standards, considering the proportion of
#'    new homes built to these standards, the difference in single-family
#'    housing units between 2021 and 2050, and the reduction in energy use
#'    intensity due to LEED Gold construction. This function is designed to
#'    estimate the impact of energy-efficient construction on residential
#'    greenhouse gas emissions.
#'
#' @param .new_sf_homes_leed_gold_pct numeric,  a value between `0` and `1`.
#'      The percentage of new single-family homes built according to *LEED Gold* standards.
#'      Default is `0.0`
#' @param .new_mf_homes_leed_gold_pct numeric,  a value between `0` and `1`.
#'      The percentage of new multi-family homes built according to *LEED Gold* standards.
#'      Default is `0.0`
#'
#' @inheritParams run_scenario_building
#' @inheritParams calc_parking_lot_land_cover
#' @inheritParams run_all_modules
#' @inheritParams run_scenario_land_use
#' @inheritParams filter_ctu
#'
#' @details
#'    Uses the expected growth of new housing according to UrbanSim estimates
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
calc_housing_leed <- function(res_tb,
                              .selected_ctu,
                              .new_sf_homes_leed_gold_pct,
                              .new_mf_homes_leed_gold_pct,
                              .leed_start_year,
                              .enviro_factors = ghg.ccap::enviro_factors) {
  # cli::cli_progress_message("*** calculating floor area LEED Gold certification strategy \n")
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)

  check_inputs(name = "new_sf_homes_leed_gold_pct", .new_sf_homes_leed_gold_pct)
  check_inputs(name = "new_mf_homes_leed_gold_pct", .new_mf_homes_leed_gold_pct)
  check_inputs(name = "leed_start_year", .leed_start_year)


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
    new_sf <- res_tb %>%
      dplyr::filter(grepl("single", sp_categories)) %>%
      dplyr::mutate(
        new_units = ifelse(value_change_from_base < 0, 0, value_change_from_base),
        new_leed = if_else(inventory_year < .leed_start_year,
          0,
          round(new_units * .new_sf_homes_leed_gold_pct)
        ),
        new_non_leed = new_units - new_leed
      ) %>%
      pivot_longer(cols = c(new_leed, new_non_leed),
                   names_to = "efficiency_description",
                   values_to = "efficiency_unit_value")
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
    new_mf <- res_tb %>%
      dplyr::filter(grepl("multi", sp_categories)) %>%
      dplyr::mutate(
        new_units = ifelse(value_change_from_base < 0, 0, value_change_from_base),
        new_leed = if_else(inventory_year < .leed_start_year,
                           0,
                           round(new_units * .new_mf_homes_leed_gold_pct)
        ),
        new_non_leed = new_units - new_leed
      ) %>%
      pivot_longer(cols = c(new_leed, new_non_leed),
                   names_to = "efficiency_description",
                   values_to = "efficiency_unit_value")
  # }

  leed_buildings <- bind_rows(
    new_sf %>%
      select(
        geog_name,
        geog_id,
        # geog_id_type,
        sp_categories,
        # geog_level,
        inventory_year,
        value,
        value_change_from_base,
        new_units,
        efficiency_description,
        efficiency_unit_value
      ),
    new_mf %>%
      select(
        geog_name,
        geog_id,
        # geog_id_type,
        sp_categories,
        # geog_level,
        inventory_year,
        value,
        value_change_from_base,
        new_units,
        efficiency_description,
        efficiency_unit_value
      )
  )

  return(leed_buildings)
}

# LEED buildings will use less energy
# Here, we are effectively reducing the housing count to account
# for the energy savings from LEED buildings


#' @title Calculate housing retrofit
#' @family buildings
#'
#' @description adjusts single and multifamily housing unit
#' forecast under the assumption of energy use reduction due to home
#' retrofits.
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
calc_residential_retrofit <- function(res_tb,
                                      .selected_ctu,
                                      .existing_sf_retrofit_pct,
                                      .existing_mf_retrofit_pct,
                                      .retrofit_start_year,
                                      .enviro_factors = ghg.ccap::enviro_factors) {
  # cli::cli_progress_message("*** calculating floor area retrofit strategy \n")
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)


  check_inputs(name = "existing_sf_retrofit_pct", .existing_sf_retrofit_pct)
  check_inputs(name = "existing_mf_retrofit_pct", .existing_mf_retrofit_pct)
  check_inputs(name = "retrofit_start_year", .retrofit_start_year)

  # if (.existing_sf_retrofit_pct == 0) {
  #   cli::cli_warn("No change in existing single family home energy efficiency")
  #   existing_sf <- res_tb %>%
  #     dplyr::filter(grepl("single", sp_categories)) %>%
  #     mutate(
  #       retrofit_units = 0,
  #       effective_unit_change_retro = 0
  #     )
  # } else if (.existing_sf_retrofit_pct != 0) {
    existing_sf <- res_tb %>%
      dplyr::filter(grepl("single", sp_categories)) %>%
      dplyr::mutate(
        existing_units = value - new_units,
        retrofit_units = if_else(inventory_year < .retrofit_start_year,
          0,
          round(existing_units * .existing_sf_retrofit_pct)),
        existing_nonretrofit = existing_units - retrofit_units) %>%
      select(-c(efficiency_description,efficiency_unit_value)) %>%
  pivot_longer(cols = c(retrofit_units, existing_nonretrofit),
               names_to = "efficiency_description",
               values_to = "efficiency_unit_value")


    # }


  # if (.existing_mf_retrofit_pct == 0) {
  #   cli::cli_warn("No change in existing multifamily home energy efficiency")
  #   existing_mf <- res_tb %>%
  #     dplyr::filter(grepl("multi", sp_categories)) %>%
  #     mutate(
  #       retrofit_units = 0,
  #       effective_unit_change_retro = 0
  #     )
  # } else if (.existing_mf_retrofit_pct != 0) {
    existing_mf <- res_tb %>%
      dplyr::filter(grepl("multi", sp_categories)) %>%
      dplyr::mutate(
        existing_units = value - new_units,
        retrofit_units = if_else(inventory_year < .retrofit_start_year,
                                 0,
                                 round(existing_units * .existing_mf_retrofit_pct)),
        existing_nonretrofit = existing_units - retrofit_units) %>%
      select(-c(efficiency_description,efficiency_unit_value)) %>%
      pivot_longer(cols = c(retrofit_units, existing_nonretrofit),
                   names_to = "efficiency_description",
                   values_to = "efficiency_unit_value")
  # }

  retrofit_results <- bind_rows(
    existing_sf %>%
      select(
        geog_name,
        geog_id,
        # geog_id_type,
        sp_categories,
        # geog_level,
        inventory_year,
        value,
        value_change_from_base,
        new_units,
        efficiency_description,
        efficiency_unit_value
      ),
    existing_mf %>%
      select(
        geog_name,
        geog_id,
        # geog_id_type,
        sp_categories,
        # geog_level,
        inventory_year,
        value,
        value_change_from_base,
        new_units,
        efficiency_description,
        efficiency_unit_value
      ),
    res_tb
  )

  return(retrofit_results)
}

# Here, we are effectively reducing the effective existing housing count to account
# for the energy savings from retrofitted building
