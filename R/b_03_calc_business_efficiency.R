#' @title Calculate new housing - LEED certified
#' @family buildings
#'
#' @description Calculates the efficiency of new buildings housing new jobs
#'    built in accordance with LEED Gold standards, considering the proportion of
#'    new buildings built to these standards, the difference in jobs
#'    between 2022 and 2050, and the reduction in energy use
#'    intensity due to LEED Gold construction. This function is designed to
#'    estimate the impact of energy-efficient construction on non-residential
#'    greenhouse gas emissions.
#'
#' @param .new_business_leed_gold_pct numeric,  a value between `0` and `1`.
#'      The percentage of new jobs based in buildings built according to *LEED Gold* standards.
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
calc_business_leed <- function(non_res_tb,
                              .selected_ctu,
                              .new_jobs_leed_gold_pct,
                              .leed_start_year,
                              .enviro_factors = ghg.ccap::enviro_factors) {
  # cli::cli_progress_message("*** calculating floor area LEED Gold certification strategy \n")

  non_res_tb <- filter_ctu(non_res_tb, .selected_ctu = .selected_ctu)

  check_inputs(name = "new_business_leed_gold_pct", .new_sf_homes_leed_gold_pct)
  check_inputs(name = "leed_start_year", .leed_start_year)

  leed_jobs <- non_res_tb %>%
    dplyr::mutate(
      new_jobs = ifelse(value_change_from_base < 0, 0, value_change_from_base),
      new_leed_jobs = if_else(inventory_year < .leed_start_year,
        0,
        round(new_jobs * .new_jobs_leed_gold_pct)
      ),
      new_non_leed_jobs = new_jobs - new_leed_jobs
    ) %>%
    pivot_longer(
      cols = c(new_leed_jobs, new_non_leed_jobs),
      names_to = "efficiency_description",
      values_to = "efficiency_unit_value"
    ) %>%
    # subset of columns to match residential pull
    select(
      geog_name,
      geog_id,
      #imagine_designation,
      sp_categories,
      inventory_year,
      value,
      value_change_from_base,
      new_units,
      efficiency_description,
      efficiency_unit_value
    )

  return(leed_jobs)
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
calc_business_retrofit <- function(non_res_tb,
                                      .selected_ctu,
                                      .existing_jobs_retrofit_pct,
                                      .retrofit_start_year,
                                      .retrofit_end_year,
                                      .enviro_factors = ghg.ccap::enviro_factors) {
  # cli::cli_progress_message("*** calculating floor area retrofit strategy \n")
  non_res_tb <- filter_ctu(non_res_tb, .selected_ctu = .selected_ctu)

  check_inputs(name = "existing_jobs_retrofit_pct", .existing_jobs_retrofit_pct)
  check_inputs(name = "retrofit_start_year", .retrofit_start_year)

  ### ramp up retrofits evenly from start year to end year

  ramp_years <- .retrofit_start_year:.retrofit_end_year
  n_ramp <- length(ramp_years)

  pct_ramp <- tibble::tibble(
    inventory_year = ramp_years,
    job_pct = seq(
      from = .existing_jobs_retrofit_pct / n_ramp,
      to = .existing_jobs_retrofit_pct,
      length.out = n_ramp
    )
  )

  # Join pct values by condition
  pct_by_year <- tibble::tibble(inventory_year = 2005:2050) %>%
    left_join(pct_ramp, by = "inventory_year") %>%
    dplyr::mutate(
      ret_pct = dplyr::case_when(
        inventory_year < .retrofit_start_year ~ 0,
        inventory_year > .retrofit_end_year ~ .existing_mf_retrofit_pct,
        TRUE ~ job_pct
      )
    )

  retrofit_results <- non_res_tb %>%
    left_join(pct_by_year %>% select(inventory_year, ret_pct),
      by = "inventory_year"
    ) %>%
    dplyr::mutate(
      new_jobs = if_else(value_change_from_base > 0, value_change_from_base, 0),
      existing_jobs = value - new_jobs,
      retrofit_jobs = if_else(inventory_year < .retrofit_start_year,
        0,
        round(existing_jobs * ret_pct)
      ),
      existing_nonretrofit_jobs = existing_jobs - retrofit_jobs
    ) %>%
    select(-ret_pct) %>%
    pivot_longer(
      cols = c(retrofit_jobs, existing_nonretrofit_jobs),
      names_to = "efficiency_description",
      values_to = "efficiency_unit_value"
    ) %>%
    ungroup() %>%
    select(
      geog_name,
      geog_id,
      #imagine_designation,
      sp_categories,
      inventory_year,
      value,
      value_change_from_base,
      new_jobs,
      efficiency_description,
      efficiency_unit_value
    )


  return(retrofit_results)
}

# Here, we are effectively reducing the effective existing housing count to account
# for the energy savings from retrofitted building
