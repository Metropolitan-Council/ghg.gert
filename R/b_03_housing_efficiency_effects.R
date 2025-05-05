#' @title Calculate new home LEED
#' @family buildings
#'
#' @description Calculates the adjusted floor area of single-family homes
#'    in accordance with LEED Gold standards, considering the proportion of
#'    new homes built to these standards, the difference in single-family
#'    housing units between 2018 and 2040, and the reduction in energy use
#'    intensity due to LEED Gold construction. This function is designed to
#'    estimate the impact of energy-efficient construction on residential
#'    floor area and associated greenhouse gas emissions.
#'
#' @param .new_homes_leed_gold_pct numeric,  a value between `0` and `1`.
#'      The percentage of new single-family homes built according to *LEED Gold* standards.
#'      Default is `0.0`
#'
#' @inheritParams run_scenario_building
#' @inheritParams calc_parking_lot_land_cover
#' @inheritParams run_all_modules
#' @inheritParams run_scenario_land_use
#' @inheritParams filter_ctu
#'
#' @details
#'    Uses the average single family floor area in 2018
#'
#' @return [tibble::tibble()].
#'       A table with columns `ctu_name`, `year`, `var`, and `value`.
#'       Table contains adjusted `single_family_units` and
#'       `single_family_average_floor_area_sqft_ctu` records for column `var`
#'       when `year == 2040` relative to the residential inputs table.
#'
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
calc_sfh_leed <- function(res_tb,
                                 .selected_ctu,
                                 .new_homes_leed_gold_pct,
                                 .enviro_factors = enviro_factors) {
  # cli::cli_progress_message("*** calculating floor area LEED Gold certification strategy \n")
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)

  if (.new_homes_leed_gold_pct == 0) {
    cli::cli_alert_warning("No change in new single family home energy efficiency")
    return(res_tb)
  } else if (.new_homes_leed_gold_pct != 0) {
    new_units <- res_tb %>%
      dplyr::filter(var == "single_family_units") %>%
      dplyr::group_by(ctu_name, var) %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = value,
        names_prefix = "year_"
      ) %>%
      dplyr::mutate(
        diff_units = year_2040 - year_2018,
        new_units = ifelse(diff_units < 0, 0, diff_units),
        new_pct_leed = new_units * .new_homes_leed_gold_pct,
        prop_of_all_new = new_units / year_2040
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(ctu_name, prop_of_all_new)


    new_leed_floor_area <- res_tb %>%
      dplyr::filter(var %in% c("single_family_average_floor_area_sqft_ctu")) %>%
      dplyr::group_by(ctu_name, var) %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = value,
        names_prefix = "year_",
        values_fn = sum
      ) %>%
      dplyr::mutate(new_forecast = year_2040 * .enviro_factors$LEED_GOLD_REDUCTION_PCT) %>%
      dplyr::left_join(new_units, by = "ctu_name") %>%
      dplyr::mutate(
        new_weighted_mean_forecast =
          stats::weighted.mean(
            c(
              year_2018,
              new_forecast,
              year_2040
            ),
            c(
              1 - prop_of_all_new,
              prop_of_all_new * .new_homes_leed_gold_pct,
              prop_of_all_new * (1 - .new_homes_leed_gold_pct)
            )
          )
      )

    new_leed_avg_floor_area <- res_tb %>%
      dplyr::filter(
        var %in% c("single_family_average_floor_area_sqft_ctu"),
        year == 2040
      ) %>%
      dplyr::left_join(new_leed_floor_area, by = c("ctu_name", "var")) %>%
      dplyr::mutate(value = new_weighted_mean_forecast) %>%
      dplyr::select(names(res_tb))

    new_res_tb_fin <- res_tb %>%
      dplyr::anti_join(new_leed_avg_floor_area, by = c("ctu_name", "year", "var")) %>%
      dplyr::bind_rows(new_leed_avg_floor_area) %>%
      dplyr::ungroup()

    return(new_res_tb_fin)
  }

  # Holding the floor area constant, LEED buildings will use less energy
  # Here, we are effectively reducing the average floor area to account
  # for the energy savings from LEED buildings
}

#' @title Calculate floor area retrofit
#' @family buildings
#'
#' @description adjusts single and multifamily average
#' floor area forecast under the assumption of energy use reduction due to home
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
#'   .enviro_factors = enviro_factors
#' )
#' }
#'
calc_sfh_retrofit <- function(res_tb,
                                     .selected_ctu,
                                     .existing_home_retrofit_pct,
                                     .existing_home_ultra_retrofit_pct,
                                     .enviro_factors = enviro_factors) {
  # cli::cli_progress_message("*** calculating floor area retrofit strategy \n")
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)

  # browser()
  if (.existing_home_retrofit_pct == 0) {
    cli::cli_alert_warning("No change in existing home energy efficiency")
    return(res_tb)
  } else if (.existing_home_retrofit_pct != 0) {
    existing_units <- res_tb %>%
      dplyr::filter(var %in% c(
        "single_family_units",
        "multifamily_units"
      )) %>%
      dplyr::group_by(ctu_name, var) %>%
      tidyr::pivot_wider(names_from = year, values_from = value, names_prefix = "year_") %>%
      dplyr::mutate(
        existing_units = year_2018,
        # existing_pct_retrofit = existing_units * .existing_home_retrofit_pct,
        # proportion of homes in 2040 that were built before 2018
        prop_of_all_existing = existing_units / year_2040
      ) %>%
      dplyr::select(ctu_name, var, prop_of_all_existing) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(
        names_from = var,
        values_from = prop_of_all_existing,
        names_glue = "proportion_existing_{var}"
      )


    retrofit_results <- res_tb %>%
      dplyr::filter(
        var %in% c(
          "single_family_average_floor_area_sqft_ctu",
          "multifamily_average_floor_area_sqft_county"
        )
      ) %>%
      dplyr::group_by(ctu_name, var) %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = value,
        names_prefix = "year_",
        values_fn = sum
      ) %>%
      dplyr::left_join(existing_units, by = "ctu_name") %>%
      dplyr::mutate(
        new_weighted_mean_forecast =
          dplyr::case_when(
            var == "single_family_average_floor_area_sqft_ctu" ~
              stats::weighted.mean(
                c(
                  year_2040,
                  year_2040 - (
                    year_2040 * .enviro_factors$EXISTING_HOME_RETROFIT_REDUCTION_PCT
                  ),
                  year_2040 - (
                    year_2040 * .enviro_factors$EXISTING_HOME_ULTRA_RETROFIT_REDUCTION_PCT
                  )
                ),
                c(
                  1 - proportion_existing_single_family_units,
                  # new homes
                  proportion_existing_single_family_units * .existing_home_retrofit_pct,
                  # existing homes, retrofitted
                  proportion_existing_single_family_units * .existing_home_ultra_retrofit_pct
                ) # existing homes, ultra retrofitted
              ),
            var == "multifamily_average_floor_area_sqft_county" ~
              stats::weighted.mean(
                c(
                  year_2040,
                  year_2040 - (
                    year_2040 * .enviro_factors$EXISTING_HOME_RETROFIT_REDUCTION_PCT
                  ),
                  year_2040 - (
                    year_2040 * .enviro_factors$EXISTING_HOME_ULTRA_RETROFIT_REDUCTION_PCT
                  )
                ),
                # existing homes, ultra retrofitted
                c(
                  1 - proportion_existing_multifamily_units,
                  proportion_existing_multifamily_units * .existing_home_retrofit_pct,
                  proportion_existing_multifamily_units * .existing_home_ultra_retrofit_pct
                )
              )
          )
      )

    new_retrofit_floor_area <- res_tb %>%
      dplyr::filter(
        var %in% c(
          "single_family_average_floor_area_sqft_ctu",
          "multifamily_average_floor_area_sqft_county"
        ),
        year == 2040
      ) %>%
      dplyr::left_join(retrofit_results, by = c("ctu_name", "var")) %>%
      dplyr::mutate(value = new_weighted_mean_forecast) %>%
      dplyr::select(names(res_tb))

    new_res_tb_fin <- res_tb %>%
      dplyr::anti_join(new_retrofit_floor_area, by = c("ctu_name", "year", "var")) %>%
      dplyr::bind_rows(new_retrofit_floor_area) %>%
      dplyr::ungroup()

    return(new_res_tb_fin)
  }

  # Holding the floor area constant, LEED buildings will use less energy
  # Here, we are effectively reducing the average floor area to account
  # for the energy savings from LEED buildings
}


#' @title Calculate floor area retrofit
#' @family buildings
#'
#' @description adjusts single and multifamily average
#' floor area forecast under the assumption of energy use reduction due to home
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
#'   .enviro_factors = enviro_factors
#' )
#' }
#'

calc_mfh_retrofit <- function(res_tb,
                              .selected_ctu,
                              .existing_home_retrofit_pct,
                              .existing_home_ultra_retrofit_pct,
                              .enviro_factors = enviro_factors) {
  # cli::cli_progress_message("*** calculating floor area retrofit strategy \n")
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)

  # browser()
  if (.existing_home_retrofit_pct == 0) {
    cli::cli_alert_warning("No change in existing home energy efficiency")
    return(res_tb)
  } else if (.existing_home_retrofit_pct != 0) {
    existing_units <- res_tb %>%
      dplyr::filter(var %in% c(
        "single_family_units",
        "multifamily_units"
      )) %>%
      dplyr::group_by(ctu_name, var) %>%
      tidyr::pivot_wider(names_from = year, values_from = value, names_prefix = "year_") %>%
      dplyr::mutate(
        existing_units = year_2018,
        # existing_pct_retrofit = existing_units * .existing_home_retrofit_pct,
        # proportion of homes in 2040 that were built before 2018
        prop_of_all_existing = existing_units / year_2040
      ) %>%
      dplyr::select(ctu_name, var, prop_of_all_existing) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(
        names_from = var,
        values_from = prop_of_all_existing,
        names_glue = "proportion_existing_{var}"
      )


    retrofit_results <- res_tb %>%
      dplyr::filter(
        var %in% c(
          "single_family_average_floor_area_sqft_ctu",
          "multifamily_average_floor_area_sqft_county"
        )
      ) %>%
      dplyr::group_by(ctu_name, var) %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = value,
        names_prefix = "year_",
        values_fn = sum
      ) %>%
      dplyr::left_join(existing_units, by = "ctu_name") %>%
      dplyr::mutate(
        new_weighted_mean_forecast =
          dplyr::case_when(
            var == "single_family_average_floor_area_sqft_ctu" ~
              stats::weighted.mean(
                c(
                  year_2040,
                  year_2040 - (
                    year_2040 * .enviro_factors$EXISTING_HOME_RETROFIT_REDUCTION_PCT
                  ),
                  year_2040 - (
                    year_2040 * .enviro_factors$EXISTING_HOME_ULTRA_RETROFIT_REDUCTION_PCT
                  )
                ),
                c(
                  1 - proportion_existing_single_family_units,
                  # new homes
                  proportion_existing_single_family_units * .existing_home_retrofit_pct,
                  # existing homes, retrofitted
                  proportion_existing_single_family_units * .existing_home_ultra_retrofit_pct
                ) # existing homes, ultra retrofitted
              ),
            var == "multifamily_average_floor_area_sqft_county" ~
              stats::weighted.mean(
                c(
                  year_2040,
                  year_2040 - (
                    year_2040 * .enviro_factors$EXISTING_HOME_RETROFIT_REDUCTION_PCT
                  ),
                  year_2040 - (
                    year_2040 * .enviro_factors$EXISTING_HOME_ULTRA_RETROFIT_REDUCTION_PCT
                  )
                ),
                # existing homes, ultra retrofitted
                c(
                  1 - proportion_existing_multifamily_units,
                  proportion_existing_multifamily_units * .existing_home_retrofit_pct,
                  proportion_existing_multifamily_units * .existing_home_ultra_retrofit_pct
                )
              )
          )
      )

    new_retrofit_floor_area <- res_tb %>%
      dplyr::filter(
        var %in% c(
          "single_family_average_floor_area_sqft_ctu",
          "multifamily_average_floor_area_sqft_county"
        ),
        year == 2040
      ) %>%
      dplyr::left_join(retrofit_results, by = c("ctu_name", "var")) %>%
      dplyr::mutate(value = new_weighted_mean_forecast) %>%
      dplyr::select(names(res_tb))

    new_res_tb_fin <- res_tb %>%
      dplyr::anti_join(new_retrofit_floor_area, by = c("ctu_name", "year", "var")) %>%
      dplyr::bind_rows(new_retrofit_floor_area) %>%
      dplyr::ungroup()

    return(new_res_tb_fin)
  }

  # Holding the floor area constant, LEED buildings will use less energy
  # Here, we are effectively reducing the average floor area to account
  # for the energy savings from LEED buildings
}

