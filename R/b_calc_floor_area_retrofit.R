#' @title Calculate Floor Area Retrofit
#' @family Buildings
#'
#' @description adjusts single and multifamily average
#' floor area forecast under the assumptio of energy use reduction due to home
#' retrofits.
#'
#' @param .existing_home_retrofit_pct numeric,  A number between `0` and `1`.
#'      Percentage of existing homes retrofitted to reduce energy usage by *33%*.
#'      Default is `0.80`.
#' @param .existing_home_ultra_retrofit_pct numeric,  A number between `0` and `1`.
#'      Percentage of existing homes retrofitted to reduce energy usage by *66%*.
#'      Default is `0.20`.
#'
#' @inheritParams run_scenario_transportation
#'
#' @details
#'    Uses the average single family floor area in 2018
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
#'   .existing_home_retrofit_pct = 0.80,
#'   .existing_home_ultra_retrofit_pct = 0.20,
#'   .enviro_factors = enviro_factors
#' )
#' }
#'
calc_floor_area_retrofit <- function(res_tb,
                                     .existing_home_retrofit_pct,
                                     .existing_home_ultra_retrofit_pct,
                                     .enviro_factors) {


  # browser()
  if (.existing_home_retrofit_pct == 0) {
    warning("No change in existing home energy efficiency")
    return(res_tb)
  } else if (.existing_home_retrofit_pct != 0) {
    existing_units <- res_tb %>%
      dplyr::filter(var %in% c("single_family_units",
                               "multifamily_units")) %>%
      dplyr::group_by(ctu_name, var) %>%
      tidyr::pivot_wider(names_from = year, values_from = value, names_prefix = "year_") %>%
      dplyr::mutate(existing_units = year_2018,
                    # existing_pct_retrofit = existing_units * .existing_home_retrofit_pct,
                    # proportion of homes in 2040 that were built before 2018
                    prop_of_all_existing = existing_units / year_2040) %>%
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
      tidyr::pivot_wider(names_from = year, values_from = value, names_prefix = "year_") %>%
      dplyr::left_join(existing_units, by = "ctu_name") %>%
      dplyr::mutate(
        new_weighted_mean_forecast =
          case_when(
            var == "single_family_average_floor_area_sqft_ctu" ~
              weighted.mean(
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
              weighted.mean(
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
      dplyr::bind_rows(new_retrofit_floor_area)

    return(new_res_tb_fin)
  }

  # Holding the floor area constant, LEED buildings will use less energy
  # Here, we are effectively reducing the average floor area to account
  # for the energy savings from LEED buildings
}
