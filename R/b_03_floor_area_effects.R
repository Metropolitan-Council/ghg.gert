#' @title Calculate floor area growth
#' @family residential
#' @family buildings
#'
#' @description Adjusts single and multifamily average floor area
#'      forecast in residential table. Allows the user to specify the percentage
#'      increase in single family floor areas, and the percentage of homes
#'      affected.
#'
#' @param .single_family_floor_area_growth_pct numeric,
#'      a value between `0` and `1`.
#'      Percentage growth rate in single family home floor area.
#'      Default is `0.05`.
#' @param .new_homes_affected_pct numeric,
#'      a value between `0` and `1`.
#'      Percentage of all new single-family
#'      households that will respond to increased energy costs by decreasing
#'      home size.
#'      Default is `0.30`.
#'
#' @inheritParams scen_building_residential
#' @inheritParams run_scenario_building
#'
#' @details
#'    Uses the average single family floor area in 2018
#' @return data table
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_floor_area_growth(
#'   res_tb = building_data$residential,
#'   .selected_ctu = "all",
#'   .single_family_floor_area_growth_pct = 0.05,
#'   .new_homes_affected_pct = 0.30,
#'   .enviro_factors = enviro_factors
#' )
#' }
#'
#' @importFrom stats weighted.mean
calc_floor_area_growth <- function(res_tb,
                                   .selected_ctu,
                                   .single_family_floor_area_growth_pct,
                                   .new_homes_affected_pct,
                                   .enviro_factors) {

  cat("*** calculating floor area growth \n")
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)

  if (.single_family_floor_area_growth_pct == 0) {
    warning("No change in single family floor area growth.")
    return(res_tb)
  } else if (.single_family_floor_area_growth_pct != 0) {
    res_tb_units <- res_tb %>%
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
        new_units_affected = new_units * .new_homes_affected_pct,
        prop_of_all_new = new_units / year_2040
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(ctu_name, prop_of_all_new)

    # in the new units ONLY, 50% will have the adjusted floor area mean
    # otherwise, they will have the BAU floor area mean
    # the existing units will keep the BAU floor area
    # so the average floor area for ALL single family units in the forecast year
    # (newly built and built before 2018)
    # will be weighted by
    # proportion of all single family homes  == existing
    # proportion of NEW single family homes  == smaller
    # proportion of NEW single family homes  == same
    # 2000 at (1 - .12)
    # 2300 at (0.12 * 0.5)
    # 2500 at (0.12 * 0.5)

    new_avg_floor_area <- res_tb %>%
      dplyr::filter(var %in% c("single_family_average_floor_area_sqft_ctu")) %>%
      dplyr::group_by(ctu_name, var) %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = value,
        names_prefix = "year_"
      ) %>%
      dplyr::mutate(new_forecast = (.single_family_floor_area_growth_pct * year_2018) + year_2018) %>%
      dplyr::left_join(res_tb_units, by = "ctu_name") %>%
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
              prop_of_all_new * .new_homes_affected_pct,
              prop_of_all_new * (1 - .new_homes_affected_pct)
            )
          )
      )


    new_res_avg_floor_area <- res_tb %>%
      dplyr::filter(
        var %in% c("single_family_average_floor_area_sqft_ctu"),
        year == 2040
      ) %>%
      dplyr::left_join(new_avg_floor_area, by = c("ctu_name", "var")) %>%
      dplyr::mutate(value = new_weighted_mean_forecast) %>%
      dplyr::select(names(res_tb))

    new_res_tb <- res_tb %>%
      dplyr::anti_join(new_res_avg_floor_area, by = c("ctu_name", "year", "var")) %>%
      dplyr::bind_rows(new_res_avg_floor_area) %>%
      dplyr::ungroup()

    return(new_res_tb)
  }
}
#' @title Calculate floor area LEED
#' @family buildings
#'
#' @description Adjusts single and multifamily average
#' floor area forecast in accordance with LEED reduction.
#'
#' @param .new_homes_leed_gold_pct numeric,  a value between `0` and `1`.
#'      The percentage of new single-family homes built according to *LEED Gold* standards.
#'      Default is `0.5`
#'
#' @inheritParams run_scenario_building
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
#' library(ghg.sp)
#'
#' calc_floor_area_leed(
#'   res_tb = building_data$residential,
#'   .selected_ctu = "all",
#'   .new_homes_leed_gold_pct = 0.5,
#'   .enviro_factors = enviro_factors
#' )
#' }
#'
calc_floor_area_leed <- function(res_tb,
                                 .selected_ctu,
                                 .new_homes_leed_gold_pct,
                                 .enviro_factors) {

  cat("*** calculating floor area LEED Gold certification strategy \n")
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)

  if (.new_homes_leed_gold_pct == 0) {
    warning("No change in new single family home energy efficiency")
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
#' floor area forecast under the assumptio of energy use reduction due to home
#' retrofits.
#'
#' @param .existing_home_retrofit_pct numeric,  a value between `0` and `1`.
#'      Percentage of existing homes retrofitted to reduce energy usage by *33%*.
#'      Default is `0.80`.
#' @param .existing_home_ultra_retrofit_pct numeric,  a value between `0` and `1`.
#'      Percentage of existing homes retrofitted to reduce energy usage by *66%*.
#'      Default is `0.20`.
#'
#' @inheritParams run_scenario_building
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
#' library(ghg.sp)
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
calc_floor_area_retrofit <- function(res_tb,
                                     .selected_ctu,
                                     .existing_home_retrofit_pct,
                                     .existing_home_ultra_retrofit_pct,
                                     .enviro_factors) {

  cat("*** calculating floor area retrofit strategy \n")
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)

  # browser()
  if (.existing_home_retrofit_pct == 0) {
    warning("No change in existing home energy efficiency")
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
      pivot_wider(
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
          case_when(
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


#' @title Calculate floor area energy intensity reduction from behavior change
#' @family buildings
#'
#' @description Adjusts residential floor area
#'      by city/township based on proportion of households that change
#'      behavior to reduce energy use.
#'
#' @param .home_behavior_change_pct numeric,  a value between `0` and `1`.
#'      Percentage of households that change behavior to reduce household emissions.
#'      Default is `1.00`.
#'
#' @inheritParams run_scenario_building
#'
#' @return [tibble::tibble()].
#'       A table with columns `ctu_name`, `year`, `var`, and `value`.
#'       Table contains adjusted
#'       `single_family_average_floor_area_sqft_ctu` and
#'       `multifamily_average_floor_area_sqft_county` records for column `var`
#'       when `year == 2040` relative to the residential inputs table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_floor_area_behavior_change(
#'   res_tb = building_data$residential,
#'   .selected_ctu = "all",
#'   .home_behavior_change_pct = 1.00,
#'   .enviro_factors = enviro_factors
#' )
#' }
calc_floor_area_behavior_change <- function(res_tb,
                                            .selected_ctu,
                                            .home_behavior_change_pct,
                                            .enviro_factors) {

  cat("*** calculating floor area behavior change strategy \n")
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)

  if (.home_behavior_change_pct == 0) {
    warning("No change in household behavior.")
    return(res_tb)
  } else if (.home_behavior_change_pct != 0) {
    # browser()
    new_behavior_change <- res_tb %>%
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
        values_fn = sum
      ) %>%
      dplyr::mutate(new_forecast = `2040` - (.enviro_factors$BEHAVIOR_CHANGE_REDUCTION_PCT * `2040`)) %>%
      dplyr::mutate(
        new_weighted_mean_forecast =
          stats::weighted.mean(
            c(
              new_forecast,
              `2040`
            ),
            c(
              .home_behavior_change_pct,
              (1 - .home_behavior_change_pct)
            )
          )
      )
    new_fla <- res_tb %>%
      dplyr::filter(
        var %in% c(
          "single_family_average_floor_area_sqft_ctu",
          "multifamily_average_floor_area_sqft_county"
        ),
        year == 2040
      ) %>%
      dplyr::left_join(new_behavior_change, by = c("ctu_name", "var")) %>%
      dplyr::mutate(value = new_weighted_mean_forecast) %>%
      dplyr::select(names(res_tb))


    new_res_tb_fin <- res_tb %>%
      dplyr::anti_join(new_fla, by = c("ctu_name", "year", "var")) %>%
      dplyr::bind_rows(new_fla) %>%
      dplyr::ungroup()

    return(new_res_tb_fin)
  }
}

#' @title Calculate affordable floor area effects
#' @family residential
#' @family buildings
#'
#' @description  Calculates forecasted reduction in single family
#' floor area from increased energy prices by city/township.
#'
#' @inheritParams run_scenario_building
#' @param .single_family_floor_area_growth_pct numeric, a value between `0` and `1`.
#'       Percentage of single family floor area that gets reduced due to increase energy prices.
#'       Default is `0.05`.
#'       Should not be greater than 0.05 or *%5*.
#'
#' @return [tibble::tibble()]. A table with columns `ctu_name`, `year`, `var`, and `value`.
#'       Table contains adjusted records for `single_family_average_floor_area_sqft_ctu` and
#'       `single_family_units` for the `var` column when `year == 2040`relative
#'       to the residential inputs table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_affordable_floor_area
#' ghg.sp::calc_affordable_floor_area(
#'   res_tb = building_data$residential,
#'   .selected_ctu = "all",
#'   .single_family_floor_area_growth_pct = 0.05
#' )
#' }
calc_affordable_floor_area <- function(res_tb,
                                       .selected_ctu,
                                       .single_family_floor_area_growth_pct) {

  cat("*** calculating affordable floor area strategy \n")
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)

  if (.single_family_floor_area_growth_pct > 0.05) {
    warning("Single family floor area growth cannot be greater than %5")
    return(res_tb)
  } else {
    new_res_tb <-
      res_tb %>%
      dplyr::filter(
        var %in% c(
          "single_family_average_floor_area_sqft_ctu",
          "single_family_units"
        )
      ) %>%
      tidyr::pivot_wider(
        names_from = c(var, year),
        values_from = value,
        names_sep = ".",
        values_fn = sum
      ) %>%
      dplyr::mutate(
        reduction_floor_area =
          0.5
          * (
              single_family_average_floor_area_sqft_ctu.2040 - (1 + .single_family_floor_area_growth_pct) *
                single_family_average_floor_area_sqft_ctu.2018
            ),
        value = single_family_units.2040 - reduction_floor_area
      ) %>%
      dplyr::mutate(
        year = 2040,
        var = "single_family_units"
      ) %>%
      dplyr::select(ctu_name, year, var, value) %>%
      dplyr::bind_rows(
        .,
        res_tb %>%
          dplyr::filter(var != "single_family_units" &
            year == 2040)
      ) %>%
      bind_rows(., res_tb %>%
        dplyr::filter(year == 2018)) %>%
      dplyr::ungroup()

    return(new_res_tb)
  }
}
