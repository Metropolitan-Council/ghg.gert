#' @title Calculate Floor Area Energy Intensity Reduction from Behavior Change
#' @family building_energy_module
#'
#' @description `calc_floor_area_behavior_change()` adjusts residential floor area
#' by city/township based on proportion of households that change
#'     behavior to reduce energy use.
#'
#' @param .home_behavior_change_pct numeric, percentage of households that change
#'     behavior to reduce household emissions.
#' @inheritParams scen_building_residential
#' @inheritParams run_scenario
#'
#'
#'
#' @return
#' @export
#'
calc_floor_area_behavior_change <- function(res_tb,
                                       .home_behavior_change_pct,
                                       .enviro_factors) {
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
      tidyr::pivot_wider(names_from = year, values_from = value) %>%
      dplyr::mutate(new_forecast = `2040` - (.enviro_factors$BEHAVIOR_CHANGE_REDUCTION_PCT * `2040`)) %>%
      dplyr::mutate(new_weighted_mean_forecast =
                      weighted.mean(c(new_forecast,
                                      `2040`),
                                    c(
                                      .home_behavior_change_pct,
                                      (1 - .home_behavior_change_pct)
                                    )))
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
      dplyr::bind_rows(new_fla)

    return(new_res_tb_fin)

  }
}
