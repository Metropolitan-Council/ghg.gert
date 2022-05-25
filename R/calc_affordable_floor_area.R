#' @title Calculate Affordable Floor Area Effects
#'
#' @param .pct_growth_single_family_floor_area
#'
#' @return
#' @export
#'
#' @examples
calc_affordable_floor_area <-
  function(res_tb,
           .pct_growth_single_family_floor_area) {
    if (.pct_growth_single_family_floor_area > 0.05) {
      warning("Single Family Floor Area Growth Cannot Be Greater than %5")
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
        mutate(
          reduction_floor_area =
            0.5
          * (
            single_family_average_floor_area_sqft_ctu.2040 - (1 + .pct_growth_single_family_floor_area) *
              single_family_average_floor_area_sqft_ctu.2018
          ),
          value = single_family_units.2040 - reduction_floor_area
        ) %>%
        mutate(year = 2040,
               var = "single_family_units") %>%
        select(ctu_name, year, var, value) %>%
        bind_rows(.,
                  res_tb %>%
                    filter(var != "single_family_units" &
                             year == 2040)) %>%
        bind_rows(., res_tb %>%
                    filter(year == 2018))

      return(new_res_tb)
    }

  }
