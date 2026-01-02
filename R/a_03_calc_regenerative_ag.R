#' @title Calculate regenerative ag emission reductions
#' @family cropland
#'
#' @description Calculates the emission reductions associated with switching to
#' no till agriculture and cover cropping.
#'
#' @param .cover_crops_current numeric,  a value between `0` and `1`.
#'      The current percentage of acreage cover cropping.
#'      Default is `0.0`
#' @param .cover_crops_goal numeric,  a value between `0` and `1`.
#'      The percentage of acreage to have cover cropping in 2050
#' @param .cover_crops_start_year numeric,  a value between `2028` and `2045`.
#'      The year new cover crops program targets
#' @param .no_till_current numeric,  a value between `0` and `1`.
#'      The current percentage of acreage with no till.
#'      Default is `0.0`
#' @param .no_till_goal numeric,  a value between `0` and `1`.
#'      The percentage of acreage to have no till in 2050
#' @param .no_till_start_year numeric,  a value between `2028` and `2045`.
#'      The year new no till program targets
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
calc_crops <- function(emissions,
                                  .scenario,
                                  .baseline_year,
                                  .ag_area,
                       .cover_crops_start_year,
                       .cover_crops_current,
                       .cover_crops_goal,
                       .no_till_start_year,
                       .no_till_current,
                       .no_till_goal) {

  browser()

  crop_emissions <- emissions %>%
    filter(source == "Soil residue emissions")

  # waiting to see if MPCA emission reduction factors are applicable
  #for now using
  #  https://doi.org/10.1007/s13593-023-00911-x
  # which provides no-till estimate of 11% reduction

  no_till_red <- (0.11)

  ## article suggests cover crops increase C sequestration - reduction of N2O less certain
  # https://conservancy.umn.edu/server/api/core/bitstreams/e3fc83a3-ae4c-4c6a-b31d-4714b24082e0/content
  # 0.2 metric ton C acre-1 yr -1

  cc_c_seq <- 0.2 * 3.67 # covert MT C acre-1 yr-1 to MT CO2 acre-1 yr-1
  # this number exceeds estimated emissions of one township by 3 fold...

  # might need to calculate soil C sequestration based on area...
  # browser()

  no_till_emissions_alt <- fertilizer_emissions %>%
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
