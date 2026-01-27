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
#' library(ghg.ccap)
#'
#'
#'
calc_crops <- function(emissions,
                                  .scenario,
                                  .baseline_year,
                                  ag_area_adj,
                       .cover_crops_start_year,
                       .cover_crops_current,
                       .cover_crops_goal,
                       .no_till_start_year,
                       .no_till_current,
                       .no_till_goal) {

  #browser()

  crop_emissions <- emissions %>%
    filter(source == "Soil residue emissions")

  # waiting to see if MPCA emission reduction factors are applicable
  #for now using
  #  https://doi.org/10.1007/s13593-023-00911-x
  # which provides no-till estimate of 11% reduction

  no_till_red <- (0.11) * .no_till_goal

  ## article suggests cover crops increase C sequestration - reduction of N2O less certain
  # https://conservancy.umn.edu/server/api/core/bitstreams/e3fc83a3-ae4c-4c6a-b31d-4714b24082e0/content
  # 0.2 metric ton C acre-1 yr -1

  #cc_c_seq <- 0.2 * 3.67 # covert MT C acre-1 yr-1 to MT CO2 acre-1 yr-1
  # this number exceeds estimated emissions of one township by 3 fold...

  # TEMPORARY NUMBER TO GET SCRIPT WORKING
  cc_c_seq <- 0.02 * 3.67 # covert MT C acre-1 yr-1 to MT CO2 acre-1 yr-1

  # might need to calculate soil C sequestration based on area...
  # browser()

  no_till_emissions_alt <- crop_emissions %>%
    mutate(
      value_emissions = case_when(
        inventory_year < .no_till_start_year ~ value_emissions,  # no changes from baseline to 2028
        inventory_year >= .no_till_start_year ~
          value_emissions * (1- (no_till_red) * (inventory_year - .no_till_start_year) /
                               (2050 - .no_till_start_year))  # linear increase of no till from start year to 2050
      ),
      scenario = .scenario
    )

  ag_area_adj

  cc_sequestration <- ag_area_adj %>%
    mutate(
      cc_sequestration = case_when(
        inventory_year < .cover_crops_start_year ~ 0,  # start at 0
        inventory_year >= .cover_crops_start_year ~
          ag_area_adj * 247.105 * #convert to acres
          ((cc_c_seq) * (inventory_year - .cover_crops_start_year) /
                               (2050 - .cover_crops_start_year))  # linear increase of cover crop acreage from start year to 2050
      )
    ) %>%
    select(inventory_year, cc_sequestration)

  cropland_alt <- no_till_emissions_alt %>%
    left_join(cc_sequestration, by = "inventory_year")  %>%
    mutate(value_emissions_net = value_emissions - cc_sequestration)

  # Check if any year has negative net emissions and adjust
  if(any(cropland_alt$value_emissions_net < 0, na.rm = TRUE)) {
    cli_alert("Cropland emissions estimated to be net sink, adjusting to 0")
    cropland_alt <- cropland_alt %>%
      mutate(value_emissions_net = pmax(value_emissions_net, 0))
  }

  cropland_emissions <- bind_rows(crop_emissions, cropland_alt %>%
                                    select(-value_emissions, -cc_sequestration) %>%
                                    rename(value_emissions = value_emissions_net)
  )

  return(cropland_emissions)
}
