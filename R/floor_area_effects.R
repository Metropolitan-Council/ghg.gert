#' @title Calculate strategy effects on residential building floor area

#'
#' Adjust single and multifamily average floor area
#'     forecast in residential table
#'
#' @param .single_family_floor_area_growth_pct numeric, growth rate
#'     in single family home floor area.
#' @param .new_homes_affected_pct numeric, percentage of all new single-family
#'     households that will respond to increased energy costs by decreasing
#'     home size.
#' @inheritParams scen_building_residential
#' @inheritParams run_scenario
#' @family building
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
#' floor_area_growth(
#'   res_tb = building_data$residential,
#'   .single_family_floor_area_growth_pct = 0.05,
#'   .new_homes_affected_pct = 0.30,
#'   .enviro_factors = enviro_factors
#' )
#' }
#'
floor_area_growth <- function(res_tb,
                              .single_family_floor_area_growth_pct,
                              .new_homes_affected_pct,
                              .enviro_factors) {
  if (.single_family_floor_area_growth_pct == 0) {
    warning("No change in single family floor area growth .")
    return(res_tb)
  } else if (.single_family_floor_area_growth_pct != 0) {
    res_tb_units <- res_tb %>%
      filter(var == "single_family_units") %>%
      dplyr::group_by(ctu_name, var) %>%
      tidyr::pivot_wider(names_from = year, values_from = value) %>%
      mutate(
        diff_units = `2040` - `2018`,
        new_units = ifelse(diff_units < 0, 0, diff_units),
        new_units_affected = new_units * .new_homes_affected_pct,
        prop_of_all_new = new_units / `2040`
      ) %>%
      ungroup() %>%
      select(ctu_name, prop_of_all_new)

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
      filter(var %in% c("single_family_average_floor_area_sqft_ctu")) %>%
      dplyr::group_by(ctu_name, var) %>%
      tidyr::pivot_wider(names_from = year, values_from = value) %>%
      mutate(new_forecast = (.single_family_floor_area_growth_pct * `2018`) + `2018`) %>%
      left_join(res_tb_units, by = "ctu_name") %>%
      mutate(
        new_weighted_mean_forecast =
          weighted.mean(
            c(
              `2018`,
              new_forecast,
              `2040`
            ),
            c(
              1 - prop_of_all_new,
              prop_of_all_new * .new_homes_affected_pct,
              prop_of_all_new * (1 - .new_homes_affected_pct)
            )
          )
      )


    new_res_avg_floor_area <- res_tb %>%
      filter(
        var %in% c("single_family_average_floor_area_sqft_ctu"),
        year == 2040
      ) %>%
      left_join(new_avg_floor_area, by = c("ctu_name", "var")) %>%
      mutate(value = new_weighted_mean_forecast) %>%
      select(names(res_tb))

    new_res_tb <- res_tb %>%
      anti_join(new_res_avg_floor_area, by = c("ctu_name", "year", "var")) %>%
      bind_rows(new_res_avg_floor_area)

    return(new_res_tb)
  }
}

#' Adjust single and multifamily average floor area forecast in
#'      accordance with LEED reduction
#' @param .new_homes_leed_gold_pct numeric, percentage of new single-family homes
#'   built according to LEED Gold standards.
#' @inheritParams run_scenario
#' @inheritParams scen_building_residential
#' @family building
#'
#' @details
#'    Uses the average single family floor area in 2018
#'
#' @return data table
#' @export
floor_area_leed <- function(res_tb,
                            .new_homes_leed_gold_pct,
                            .enviro_factors) {
  if (.new_homes_leed_gold_pct == 0) {
    warning("No change in new single family home energy efficiency")
    return(res_tb)
  } else if (.new_homes_leed_gold_pct != 0) {
    new_units <- res_tb %>%
      filter(var == "single_family_units") %>%
      dplyr::group_by(ctu_name, var) %>%
      tidyr::pivot_wider(names_from = year, values_from = value) %>%
      mutate(
        diff_units = `2040` - `2018`,
        new_units = ifelse(diff_units < 0, 0, diff_units),
        new_pct_leed = new_units * .new_homes_leed_gold_pct,
        prop_of_all_new = new_units / `2040`
      ) %>%
      ungroup() %>%
      select(ctu_name, prop_of_all_new)


    new_leed_floor_area <- res_tb %>%
      filter(var %in% c("single_family_average_floor_area_sqft_ctu")) %>%
      dplyr::group_by(ctu_name, var) %>%
      tidyr::pivot_wider(names_from = year, values_from = value) %>%
      mutate(new_forecast = `2040` * .enviro_factors$LEED_GOLD_REDUCTION_PCT) %>%
      left_join(new_units, by = "ctu_name") %>%
      mutate(
        new_weighted_mean_forecast =
          weighted.mean(
            c(
              `2018`,
              new_forecast,
              `2040`
            ),
            c(
              1 - prop_of_all_new,
              prop_of_all_new * .new_homes_leed_gold_pct,
              prop_of_all_new * (1 - .new_homes_leed_gold_pct)
            )
          )
      )

    new_leed_avg_floor_area <- res_tb %>%
      filter(
        var %in% c("single_family_average_floor_area_sqft_ctu"),
        year == 2040
      ) %>%
      left_join(new_leed_floor_area, by = c("ctu_name", "var")) %>%
      mutate(value = new_weighted_mean_forecast) %>%
      select(names(res_tb))

    new_res_tb_fin <- res_tb %>%
      anti_join(new_leed_avg_floor_area, by = c("ctu_name", "year", "var")) %>%
      bind_rows(new_leed_avg_floor_area)

    return(new_res_tb_fin)
  }

  # Holding the floor area constant, LEED buildings will use less energy
  # Here, we are effectively reducing the average floor area to account
  # for the energy savings from LEED buildings
}


#' Adjust single and multifamily average floor area forecast in accordance with LEED reduction
#'
#' @param .existing_home_retrofit_pct numeric, percentage of existing homes
#'     retrofitted to reduce energy usage by 33%
#' @param .existing_home_ultra_retrofit_pct numeric, percentage of existing homes
#'      retrofitted to reduce energy usage by 66%
#'
#' @inheritParams scen_building_residential
#' @inheritParams run_scenario
#' @family building
#'
#' @details
#'    Uses the average single family floor area in 2018
#'
#' @return
#' @export
#'
floor_area_retrofit <- function(res_tb,
                                .existing_home_retrofit_pct,
                                .existing_home_ultra_retrofit_pct,
                                .enviro_factors) {
  # browser()
  if (.existing_home_retrofit_pct == 0) {
    warning("No change in existing home energy efficiency")
    return(res_tb)
  } else if (.existing_home_retrofit_pct != 0) {
    existing_units <- res_tb %>%
      filter(var %in% c(
        "single_family_units",
        "multifamily_units"
      )) %>%
      dplyr::group_by(ctu_name, var) %>%
      tidyr::pivot_wider(names_from = year, values_from = value) %>%
      mutate(
        existing_units = `2018`,
        # existing_pct_retrofit = existing_units * .existing_home_retrofit_pct,
        # proportion of homes in 2040 that were built before 2018
        prop_of_all_existing = existing_units / `2040`
      ) %>%
      select(ctu_name, var, prop_of_all_existing) %>%
      ungroup() %>%
      pivot_wider(
        names_from = var,
        values_from = prop_of_all_existing,
        names_glue = "proportion_existing_{var}"
      )


    retrofit_results <- res_tb %>%
      filter(var %in% c(
        "single_family_average_floor_area_sqft_ctu",
        "multifamily_average_floor_area_sqft_county"
      )) %>%
      dplyr::group_by(ctu_name, var) %>%
      tidyr::pivot_wider(names_from = year, values_from = value) %>%
      left_join(existing_units, by = "ctu_name") %>%
      mutate(
        new_weighted_mean_forecast =
          case_when(
            var == "single_family_average_floor_area_sqft_ctu" ~
            weighted.mean(
              c(
                `2040`,
                `2040` - (`2040` * .enviro_factors$EXISTING_HOME_RETROFIT_REDUCTION_PCT),
                `2040` - (`2040` * .enviro_factors$EXISTING_HOME_ULTRA_RETROFIT_REDUCTION_PCT)
              ),
              c(
                1 - proportion_existing_single_family_units, # new homes
                proportion_existing_single_family_units * .existing_home_retrofit_pct, # existing homes, retrofitted
                proportion_existing_single_family_units * .existing_home_ultra_retrofit_pct
              ) # existing homes, ultra retrofitted
            ),
            var == "multifamily_average_floor_area_sqft_county" ~
            weighted.mean(
              c(
                `2040`,
                `2040` - (`2040` * .enviro_factors$EXISTING_HOME_RETROFIT_REDUCTION_PCT),
                `2040` - (`2040` * .enviro_factors$EXISTING_HOME_ULTRA_RETROFIT_REDUCTION_PCT)
              ), # existing homes, ultra retrofitted
              c(
                1 - proportion_existing_multifamily_units,
                proportion_existing_multifamily_units * .existing_home_retrofit_pct,
                proportion_existing_multifamily_units * .existing_home_ultra_retrofit_pct
              )
            )
          )
      )


    new_retrofit_floor_area <- res_tb %>%
      filter(
        var %in% c(
          "single_family_average_floor_area_sqft_ctu",
          "multifamily_average_floor_area_sqft_county"
        ),
        year == 2040
      ) %>%
      left_join(retrofit_results, by = c("ctu_name", "var")) %>%
      mutate(value = new_weighted_mean_forecast) %>%
      select(names(res_tb))

    new_res_tb_fin <- res_tb %>%
      anti_join(new_retrofit_floor_area, by = c("ctu_name", "year", "var")) %>%
      bind_rows(new_retrofit_floor_area)

    return(new_res_tb_fin)
  }

  # Holding the floor area constant, LEED buildings will use less energy
  # Here, we are effectively reducing the average floor area to account
  # for the energy savings from LEED buildings
}

#
# floor_area_behavior_change <- function(res_tb,
#                                        .home_behavior_change_pct,
#                                        .enviro_factors){
#
#
#
#
#
#
# }
