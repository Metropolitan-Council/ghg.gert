#' Title
#'
#' @param tb
#' @param .single_family_floor_area_growth_pct
#' @param .new_homes_leed_gold_pct
#' @param .existing_home_retrofit_pct
#' @param .existing_home_ultra_retrofit_pct
#' @param .home_behavior_change_pct
#' @param .homes_electric_heating_pct
#'
#' @inheritParams adj_unit_counts
#'
#' @return
#' @export
#'
#' @examples
scen_building_residential <- function(tb = building_data,
                                      .new_homes_to_multifamily_pct = 0.5,
                                      .single_family_floor_area_growth_pct = 0.05,
                                      .new_homes_leed_gold_pct = 0.5,
                                      .existing_home_retrofit_pct = 0.8,
                                      .existing_home_ultra_retrofit_pct = 0.2, # needs to be 1 - .existing_home_retrofit_pct
                                      .home_behavior_change_pct = 1,
                                      .homes_electric_heating_pct = 0.59){

  ### Compact buildings: half of new SF homes become MF
  ## 50% new multifamily

  tb <- adj_unit_counts(res_tb = tb,
                        .new_homes_to_multifamily_pct = .new_homes_to_multifamily_pct)


  s_percent_new_homes <- 0.5

  # tool assumes that floor area will increase over time


  ## 5% Growth Rate of Single Family Floor Area
  # assume that energy costs will rise, and people will get smaller homes
  # only about 50% of single family homes will be affected/will respond
  # to increased energy costs with smaller homes
  s_growth_rate_singlefamily_floor_area <- 0.05

  ## Percent of New Homes that LEED
  # 50% new homes to LEED Gold (25 kBTU/sf; electricity reduces by 64% and gas by 64%)
  s_percent_of_new_leed_homes <- 0.5

  ### Existing Homes (80%) Retrofitted to Performance-Based High Energy Efficiency Standards
  ## Percent of Single Family Units to be Retrofitted
  s_percent_of_existing_homes_to_retrofitted <- 0.8

  ## Percent of Largest Existing Homes to Become Passive Homes
  s_percent_of_largest_existing_homes_to_be_passive_homes <- 0.2

  # All homes receive effective messages, use in-home display, and smart meters (reduce HH energy use by 11%)
  s_percent_of_homes_changes_behaviors <- 1


  s_percent_of_additional_households_with_heating_electrified <- 0.59
}
