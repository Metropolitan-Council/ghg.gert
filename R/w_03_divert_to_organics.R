#' Calculate waste activity diverted to organics
#'
#' @param waste_tb table, waste activity data
#' @param .diverted_to_organics_pct single value, percent of all solid waste activity that is composted
#' @param .diverted_to_organics_start single value, year to start source diversion
#' @param .diverted_to_organics_end single value, year to end source diversion
#' @return a data table with geoid, source, inventory_year, value_activity,
#' units_activity, value_emissions, and units_emissions
#' @export
divert_to_organics <- function(waste_tb,
                                .diverted_to_organics_pct = 0,
                                .diverted_to_organics_start = 2025,
                                .diverted_to_organics_end = 2050) {


  # Input checks
  if (!is.numeric(.diverted_to_organics_pct) || .diverted_to_organics_pct < 0 || .diverted_to_organics_pct > 100) {
    stop(".diverted_to_organics_pct must be a number between 0 and 100.")
  }

  # create empty df
  inventory_year = unique(waste_tb$inventory_year)
  projections_table = tibble::tibble(
    inventory_year, percent_diverted_to_organics = rep(.diverted_to_organics_pct, length(inventory_year))
    )

  # now let's create a table but where the percentage increases linearly over time
  # between start and end year. So if pct change == 0.5,
  # and start == 2025, and end == 2050, then
  # the percentage will be 0 in 2025 and will increase linearly to 0.5 by the year 2050

  if (.diverted_to_organics_pct > 0) {
    projections_table <- projections_table %>%
      dplyr::mutate(
        percent_diverted_to_organics = dplyr::case_when(
          inventory_year < .diverted_to_organics_start ~ 0,
          inventory_year >= .diverted_to_organics_start & inventory_year <= .diverted_to_organics_end ~
            (.diverted_to_organics_pct / (.diverted_to_organics_end - .diverted_to_organics_start)) * (inventory_year - .diverted_to_organics_start),
          TRUE ~ .diverted_to_organics_pct
        )
      )
  }



  # Now we need to calculate the change in value_activity for Landfill and Organics
  waste_proj <- waste_tb %>%
    group_by(inventory_year) %>%
    mutate(
      total_activity = sum(value_activity, na.rm = TRUE),
      pct_of_total = value_activity / total_activity * 100
    ) %>%
    ungroup() %>%
    left_join(projections_table, by = "inventory_year") %>%
    filter(source %in% c("Landfill","Organics")) %>%
    pivot_wider(
      names_from = source,
      values_from = c(value_activity, pct_of_total),
      values_fill = 0
    ) %>%
    mutate(
      change_in_Landfill = case_when(
        # if percent_diverted_to_organics is less than the current percentage
        # that Organics is already at, then we don't change the value_activity
        percent_diverted_to_organics <= pct_of_total_Organics ~ 0,
        # otherwise, we calculate the change based on the percentage diverted
        TRUE ~ -(percent_diverted_to_organics - pct_of_total_Organics) / 100 * total_activity
      ),
      change_in_Organics = -change_in_Landfill,
      value_activity_Landfill = value_activity_Landfill + change_in_Landfill,
      value_activity_Organics = value_activity_Organics + change_in_Organics
    ) %>%
    dplyr::select(-c(change_in_Landfill, change_in_Organics,
                     total_activity, percent_diverted_to_organics,
                     pct_of_total_Landfill, pct_of_total_Organics)) %>%
    # remove "value_activity_" prefix from column names
    rename_with(~ gsub("value_activity_", "", .), starts_with("value_activity_")) %>%
    pivot_longer(
      cols = c("Landfill", "Organics"),
      names_to = "source",
      values_to = "value_activity"
    ) %>% relocate(c(source, value_activity), .before="units_activity")


  waste_proj <- waste_tb %>%
    filter(!source %in% c("Landfill", "Organics")) %>%
    bind_rows(waste_proj) %>%
    arrange(inventory_year, source)


  # Return the modified waste activity table
  return(waste_proj)

}
