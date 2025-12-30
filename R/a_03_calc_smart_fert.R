#' @title Calculate smart fertilizer
#' @family cropland
#'
#' @description Calculates the emission reductions associated with switching to
#' smart-fertilizer application. Follows MN CAF technical documentation.
#'
#' @param .smart_fertilizer_current numeric,  a value between `0` and `1`.
#'      The current percentage of fertilizer already applied as 'smart'.
#'      Default is `0.0`
#' @param .smart_fertilizer_goal numeric,  a value between `0` and `1`.
#'      The percentage of fertilizer to be applied as 'smart'.
#'      Default is .smart_fertilizer_current
#' @param .smart_fertilizer_start_year numeric,  a value between `2028` and `2045`.
#'      The year new fertilizer program targets
#'
#' @inheritParams run_module_agriculture
#' @inheritParams calculate_cropland_emissions
#' @inheritParams filter_ctu
#'
#' @details
#'    Uses the coefficients found in MN CAF technical documenation
#'
#' @return [tibble::tibble()].
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.ccap)
#'
#'
#'
calc_smart_fertilizer <- function(emissions,
                                  .scenario,
                                  .baseline_year,
                                  .ag_area,
                                  .smart_fertilizer_current,
                                  .smart_fertilizer_goal,
                                  .smart_fertilizer_start_year) {


  fertilizer_emissions <- emissions %>%
    filter(grepl("fertilizer",source,ignore.case = TRUE)) %>%
    group_by(geog_name, geog_id, sector, category, inventory_year, scenario) %>%
    summarize(value_emissions = sum(value_emissions)) %>%
    ungroup() %>%
    mutate(source = "Fertilizer emissions")

  # waiting to see if MPCA emission reduction factors are applicable, for now using
  # https://doi.org/10.1111/j.1365-2486.2009.02031.x
  # which provides nitrogen inhibitor and slow-release (PCF) reduction estimates
  # of 38% and 35%, respectively.

  red_fac <- (0.38 + 0.35) / 2

  #browser()

  fertilizer_emissions_alt <- fertilizer_emissions %>%
  mutate(
    value_emissions = case_when(
      inventory_year < .smart_fertilizer_start_year ~ value_emissions,  # no changes from baseline to 2028
      inventory_year >= .smart_fertilizer_start_year ~
        value_emissions * (1- (red_fac) * (inventory_year - .smart_fertilizer_start_year) /
                             (2050 - .smart_fertilizer_start_year))  # linear increase of smart fertilizer from start year to 2050
    ),
    scenario = .scenario
  )

  fertilizer_emissions <- bind_rows(fertilizer_emissions, fertilizer_emissions_alt)

  return(fertilizer_emissions)
}
