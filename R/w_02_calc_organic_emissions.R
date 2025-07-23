#' Calculate GHG emissions from municipal solid waste sent to organics facilities.
#'
#' @inheritParams calculate_landfill_emissions
#' @param .anaerobic_digestion_pct single value, anaerobic digestion percentage
#' @return a data table with geoid, source, inventory_year, value_activity,
#' units_activity, value_emissions, and units_emissions
#' @export
calculate_organic_emissions <- function(
    waste_tb,
    .anaerobic_digestion_pct = 0,
    .methane_recovery_pct = 0
) {
  inventory_year = unique(waste_tb$inventory_year)
  methane_recovery_table = tibble::tibble(
    inventory_year, percent_recovered = rep(.methane_recovery_pct, length(inventory_year))
  )
  anaerobic_digestion_table = tibble::tibble(
    inventory_year, percent_ad = rep(.anaerobic_digestion_pct, length(inventory_year))
  )

  ch4_factor_compost <- 10 / 1000 # aggregate emissions factor for aerobic composting, 10 metric tons CH4/thousand metric tons waste, IPCC default
  ch4_factor_ad <- 2 / 1000 # aggregate emissions factor for anaerobic digestion, 2 metric tons CH4/thousand metric tons waste, IPCC default
  # if we were incorporating methane recovered, that would be added as a column to the dataframe

  n2o_factor_compost <- 0.6 / 1000 # aggregate emissions factor for aerobic composting, 0.6 metric tons N2O/thousand metric tons waste, IPCC default
  # N2O emissions from anaerobic digestion are assumed negligible

  organics_emissions <- waste_tb %>%
    dplyr::filter(source == "Organics") %>%
    dplyr::left_join(methane_recovery_table, by = dplyr::join_by(inventory_year)) %>%
    dplyr::left_join(anaerobic_digestion_table, by = dplyr::join_by(inventory_year)) %>%
    dplyr::mutate(
      "Metric tons CH4" = (value_activity * ch4_factor_compost * (1-percent_ad)) * (1 - percent_recovered) +
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
