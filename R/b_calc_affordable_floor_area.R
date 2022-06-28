#' @title Calculate Affordable Floor Area Effects
#' @family Residential
#' @family Buildings
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
#'      res_tb = building_data$residential,
#'      .single_family_floor_area_growth_pct = 0.05
#' }
calc_affordable_floor_area <- function(res_tb,
                                       .single_family_floor_area_growth_pct) {

  if (.single_family_floor_area_growth_pct > 0.05) {
    warning("Single family floor area growth cannot be greater than %5")
    return(res_tb)
  } else{
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
        names_sep = "."
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
      dplyr::mutate(year = 2040,
                    var = "single_family_units") %>%
      dplyr::select(ctu_name, year, var, value) %>%
      dplyr::bind_rows(.,
                       res_tb %>%
                         dplyr::filter(var != "single_family_units" &
                                         year == 2040)) %>%
      bind_rows(., res_tb %>%
                  dplyr::filter(year == 2018))

    return(new_res_tb)
  }

}
