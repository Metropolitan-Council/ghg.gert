#' Calculate reduction in waste activity
#'
#' @param waste_tb table, waste activity data
#' @param waste_characterization table, output of 01_mpca_waste_characterization.R
#' @param .waste_reduction_pct single value, percent reduction in waste activity
#' @param .waste_reduction_start single value, year to start waste reduction
#' @param .waste_reduction_end single value, year to end waste reduction
#' @return a data table with geoid, source, inventory_year, value_activity,
#' units_activity, value_emissions, and units_emissions
#' @export
calculate_waste_reduction <- function(waste_tb,
                                      .waste_reduction_pct = 0,
                                      .waste_reduction_start = 2025,
                                      .waste_reduction_end = 2050) {
  # Input checks
  if (!is.numeric(.waste_reduction_pct) || .waste_reduction_pct < 0 || .waste_reduction_pct > 1) {
    stop(".waste_reduction_pct must be a number between 0 and 1.")
  }


  # create empty methane recovery df
  inventory_year <- unique(waste_tb$inventory_year)
  projections_table <- tibble::tibble(
    inventory_year,
    percent_reduced = rep(.waste_reduction_pct, length(inventory_year))
  )

  # now let's create a table but where the percentage increases linearly over time
  # between start and end year. So if pct change == 0.5,
  # and start == 2025, and end == 2050, then
  # the percentage will be 0 in 2025 and will increase linearly to 0.5 by the year 2050

  if (.waste_reduction_pct > 0) {
    projections_table <- projections_table %>%
      dplyr::mutate(
        percent_reduced = dplyr::case_when(
          inventory_year < .waste_reduction_start ~ 0,
          inventory_year >= .waste_reduction_start & inventory_year <= .waste_reduction_end ~
            (.waste_reduction_pct / (.waste_reduction_end - .waste_reduction_start)) * (inventory_year - .waste_reduction_start),
          TRUE ~ .waste_reduction_pct
        )
      )
  }

  # Join the projections table with the waste activity table
  waste_proj <- waste_tb %>%
    left_join(projections_table, by = "inventory_year") %>%
    dplyr::mutate(
      value_activity = value_activity * (1 - percent_reduced)
    ) %>%
    dplyr::select(-percent_reduced)

  # Return the modified waste activity table
  return(waste_proj)
}
