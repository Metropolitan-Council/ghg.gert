#' @title Adjust BAU agriculture emissions
#' @family residential
#' @family buildings
#'
#' @description This function reduces BAU agricultural emissions proportionally
#' to expected loss of agricultural land in the municipality due to e.g.
#' development, abandonment, restoration etc
#'
#' @inheritParams calculate_cropland_emissions
#'
#' @param .cropland_decrease_2050 numeric, zero or a positive value between `0` and
#'      the agricultural area of the municipality in 2022.
#'      Default is `0.0`.
#'
#' @return [tibble::tibble()].
#' A table with the same columns as emissions input
#'
#' @export
#'
#' @examples
#' @importFrom dplyr filter group_by mutate select ungroup anti_join bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom cli cli_warn
adj_cropland_area <- function(emissions,
                              ag_area,
                              .baseline_year = .baseline_year,
                              .cropland_decrease_2050) {

  #browser()

  # reduction factor - 2022 ag area - reduction / 2022 ag area
  baseline_area <- ag_area %>%
    filter(inventory_year == .baseline_year) %>%
    pull(area)

  red_fac <- (baseline_area - .cropland_decrease_2050) / baseline_area

  emissions_predict <- emissions %>%
    filter(inventory_year == .baseline_year) %>%
  crossing(emissions_year = .baseline_year:2050) %>%
    # calculate adjusted emissions based on red_fac, reducing linearly from 2028 to 2050
    mutate(
      value_emissions = case_when(
        emissions_year <= 2028 ~ value_emissions,  # no changes from baseline to 2028
        emissions_year > 2028 ~ value_emissions * (1 - (1 - red_fac) * (emissions_year - 2028) / (2050 - 2028))  # linear reduction of ag loss from 2028 to 2050
      ),
      inventory_year = emissions_year
    ) %>%
    select(-emissions_year)


  emissions_bau <- bind_rows(emissions %>%
                               filter(inventory_year < .baseline_year),
                             emissions_predict) %>%
    mutate(scenario = "bau")

  #adjust ag area for later use

  ag_area_adj <- ag_area %>%
    mutate(
      ag_area_adj = case_when(
        inventory_year  <= 2028 ~ area,  # no changes from baseline to 2028
        inventory_year  > 2028 ~ area * (1 - (1 - red_fac) * (inventory_year - 2028) / (2050 - 2028))  # linear reduction of ag loss from 2028 to 2050
      )
    )

  cropland_adj <- list(emissions_bau = emissions_bau, ag_area_adj = ag_area_adj)

  return(cropland_adj)

}

