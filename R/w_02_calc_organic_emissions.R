#' Calculate GHG emissions from municipal solid waste sent to organics facilities.
#'
#' @param waste_inv table, waste inventory data
#' @param waste_future table, projected waste data
#' @param .anaerobic_digestion_pct single value, anaerobic digestion percentage
#' @param .anaerobic_digestion_start single value, year when anaerobic digestion starts
#' @param .anaerobic_digestion_end single value, year when anaerobic digestion ends
#' @param .methane_recovery_pct single value, methane recovery percentage
#' @param .methane_recovery_start single value, year when methane recovery starts
#' @param .methane_recovery_end single value, year when methane recovery ends
#'
#' @return a data table with geoid, source, inventory_year, value_activity,
#' units_activity, value_emissions, and units_emissions
#' @export
calculate_organic_emissions <- function(
  waste_inv,
  waste_future,
  .anaerobic_digestion_pct = 0,
  .anaerobic_digestion_start = 2025,
  .anaerobic_digestion_end = 2050,
  .methane_recovery_pct = 0,
  .methane_recovery_start = 2025,
  .methane_recovery_end = 2050
) {
  inventory_year <- unique(waste_future$inventory_year)
  methane_recovery_table <- tibble::tibble(
    inventory_year,
    percent_recovered = rep(.methane_recovery_pct, length(inventory_year))
  )
  anaerobic_digestion_table <- tibble::tibble(
    inventory_year,
    percent_ad = rep(.anaerobic_digestion_pct, length(inventory_year))
  )

  # now let's create a table but where the percentage increases linearly over time
  # between methane_recovery_start and methane_recovery_end. So if methane_recovery_pct == 0.5,
  # and methane_recovery_start == 2025, and methane_recovery_end == 2050, then
  # the percentage will be 0 in 2025 and will increase linearly to 0.5 by the year 2050

  if (.methane_recovery_pct > 0) {
    methane_recovery_table <- methane_recovery_table %>%
      dplyr::mutate(
        percent_recovered = dplyr::case_when(
          inventory_year < .methane_recovery_start ~ 0,
          inventory_year >= .methane_recovery_start & inventory_year <= .methane_recovery_end ~
            (.methane_recovery_pct / (.methane_recovery_end - .methane_recovery_start)) * (inventory_year - .methane_recovery_start),
          TRUE ~ .methane_recovery_pct
        )
      )
  }

  if (.anaerobic_digestion_pct > 0) {
    anaerobic_digestion_table <- anaerobic_digestion_table %>%
      dplyr::mutate(
        percent_ad = dplyr::case_when(
          inventory_year < .anaerobic_digestion_start ~ 0,
          inventory_year >= .anaerobic_digestion_start & inventory_year <= .anaerobic_digestion_end ~
            (.anaerobic_digestion_pct / (.anaerobic_digestion_end - .anaerobic_digestion_start)) * (inventory_year - .anaerobic_digestion_start),
          TRUE ~ .anaerobic_digestion_pct
        )
      )
  }


  ch4_factor_compost <- 10 / 1000 # aggregate emissions factor for aerobic composting, 10 metric tons CH4/thousand metric tons waste, IPCC default
  ch4_factor_ad <- 2 / 1000 # aggregate emissions factor for anaerobic digestion, 2 metric tons CH4/thousand metric tons waste, IPCC default
  # if we were incorporating methane recovered, that would be added as a column to the dataframe

  n2o_factor_compost <- 0.6 / 1000 # aggregate emissions factor for aerobic composting, 0.6 metric tons N2O/thousand metric tons waste, IPCC default
  # N2O emissions from anaerobic digestion are assumed negligible

  # Create empty list
  organics_emissions <- list()

  organics_emissions$inv <- waste_inv %>%
    dplyr::filter(source == "Organics") %>%
    # dplyr::left_join(methane_recovery_table, by = dplyr::join_by(inventory_year)) %>%
    # dplyr::left_join(anaerobic_digestion_table, by = dplyr::join_by(inventory_year)) %>%
    dplyr::mutate(
      percent_recovered = 0,
      percent_ad = 0,
      "Metric tons CH4" = (value_activity * ch4_factor_compost * (1 - percent_ad)) * (1 - percent_recovered) +
        (value_activity * ch4_factor_ad * percent_ad),
      # Note methane recovery not included for anaerobic digestion as default emission
      # factors for anaerobic digestion already account for CH4 recovery.
      "Metric tons N2O" = (value_activity * n2o_factor_compost * (1 - percent_ad))
    ) %>%
    tidyr::pivot_longer(
      c("Metric tons CH4", "Metric tons N2O"),
      names_to = "units_emissions",
      values_to = "value_emissions"
    ) %>%
    dplyr::select(
      -c(percent_recovered, percent_ad)
    )


  organics_emissions$future <- waste_future %>%
    dplyr::filter(source == "Organics") %>%
    dplyr::left_join(methane_recovery_table, by = dplyr::join_by(inventory_year)) %>%
    dplyr::left_join(anaerobic_digestion_table, by = dplyr::join_by(inventory_year)) %>%
    dplyr::mutate(
      "Metric tons CH4" = (value_activity * ch4_factor_compost * (1 - percent_ad)) * (1 - percent_recovered) +
        (value_activity * ch4_factor_ad * percent_ad),
      # Note methane recovery not included for anaerobic digestion as default emission
      # factors for anaerobic digestion already account for CH4 recovery.
      "Metric tons N2O" = (value_activity * n2o_factor_compost * (1 - percent_ad))
    ) %>%
    tidyr::pivot_longer(
      c("Metric tons CH4", "Metric tons N2O"),
      names_to = "units_emissions",
      values_to = "value_emissions"
    ) %>%
    dplyr::select(
      -c(percent_recovered, percent_ad)
    )

  return(organics_emissions)
}
