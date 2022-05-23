#' @title Calculate Floor Area LEED
#' @family building_energy_module
#'
#' @description `calc_floor_area_leed()` adjusts single and multifamily average floor area forecast in
#' accordance with LEED reduction
#'
#' @param .new_homes_leed_gold_pct Numeric. A number between `0` and `1`
#' The percentage of new single-family homes built according to LEED Gold standards.
#'      Default is `0.5`
#' @inheritParams run_scenario
#'
#' @details
#'    Uses the average single family floor area in 2018
#'
#' @return data table
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' ghg.sp::calc_floor_area_leed(
#'   res_tb = building_data$residential,
#'   .new_homes_leed_gold_pct = 0.5,
#'   .enviro_factors = enviro_factors
#' )
#' }
#'
calc_floor_area_leed <- function(res_tb,
                                 .new_homes_leed_gold_pct,
                                 .enviro_factors) {
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
        names_prefix = "year_"
      ) %>%
      dplyr::mutate(new_forecast = year_2040 * .enviro_factors$LEED_GOLD_REDUCTION_PCT) %>%
      dplyr::left_join(new_units, by = "ctu_name") %>%
      dplyr::mutate(new_weighted_mean_forecast =
                      weighted.mean(
                        c(year_2018,
                          new_forecast,
                          year_2040),
                        c(
                          1 - prop_of_all_new,
                          prop_of_all_new * .new_homes_leed_gold_pct,
                          prop_of_all_new * (1 - .new_homes_leed_gold_pct)
                        )
                      ))

    new_leed_avg_floor_area <- res_tb %>%
      dplyr::filter(var %in% c("single_family_average_floor_area_sqft_ctu"),
                    year == 2040) %>%
      dplyr::left_join(new_leed_floor_area, by = c("ctu_name", "var")) %>%
      dplyr::mutate(value = new_weighted_mean_forecast) %>%
      dplyr::select(names(res_tb))

    new_res_tb_fin <- res_tb %>%
      dplyr::anti_join(new_leed_avg_floor_area, by = c("ctu_name", "year", "var")) %>%
      dplyr::bind_rows(new_leed_avg_floor_area)

    return(new_res_tb_fin)
  }

  # Holding the floor area constant, LEED buildings will use less energy
  # Here, we are effectively reducing the average floor area to account
  # for the energy savings from LEED buildings
}
