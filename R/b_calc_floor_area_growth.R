#' @title Calculate Floor Area Growth
#' @family Residential
#' @family buildings
#'
#' @description  adjusts single and multifamily average floor area
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
#' @inheritParams run_scenario_transportation
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
#'   .single_family_floor_area_growth_pct = 0.05,
#'   .new_homes_affected_pct = 0.30,
#'   .enviro_factors = enviro_factors
#' )
#' }
#'
calc_floor_area_growth <- function(res_tb,
                                   .single_family_floor_area_growth_pct,
                                   .new_homes_affected_pct,
                                   .enviro_factors) {
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
          weighted.mean(
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
      dplyr::bind_rows(new_res_avg_floor_area)

    return(new_res_tb)
  }
}
