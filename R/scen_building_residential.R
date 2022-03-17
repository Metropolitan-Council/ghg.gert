#' Title
#'
#' @param tb table, data table with residential building attributes. Package provided
#'     dataset `building_energy$residential` is suitable and the default value.
#'
#' @inheritParams adj_unit_counts
#' @inheritParams floor_area_leed
#' @inheritParams floor_area_growth
#' @inheritParams floor_area_retrofit
#' @inheritParams floor_area_leed
#' @inheritParams run_scenario
#'
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' scen_building_residential(tb = building_data$residential)
#' }
scen_building_residential <- function(tb = building_data$residential,
                                      .new_homes_to_multifamily_pct = 0.5,
                                      .single_family_floor_area_growth_pct = 0.05,
                                      .new_homes_affected_pct = 0.5,
                                      .new_homes_leed_gold_pct = 0.5,
                                      .existing_home_retrofit_pct = 0.8,
                                      .existing_home_ultra_retrofit_pct = 0.2,
                                      .home_behavior_change_pct = 1,
                                      .homes_electric_heating_pct = 0.59,
                                      .enviro_factors = enviro_factors) {
  # browser()

  tb01 <- adj_unit_counts(
    res_tb = tb,
    .new_homes_to_multifamily_pct = .new_homes_to_multifamily_pct
  )


  tb02 <- floor_area_growth(
    res_tb = tb01,
    .single_family_floor_area_growth_pct = .single_family_floor_area_growth_pct,
    .new_homes_affected_pct = .new_homes_affected_pct,
    .enviro_factors = .enviro_factors
  )


  tb03 <- floor_area_leed(
    res_tb = tb02,
    .new_homes_leed_gold_pct = .new_homes_leed_gold_pct,
    .enviro_factors = .enviro_factors
  )

  tb04 <- floor_area_retrofit(
    res_tb = tb03,
    .existing_home_retrofit_pct = .existing_home_retrofit_pct,
    .existing_home_ultra_retrofit_pct = .existing_home_ultra_retrofit_pct,
    .enviro_factors = .enviro_factors
  )


  tb05 <- floor_area_behavior_change(
    res_tb = tb04,
    .home_behavior_change_pct = .home_behavior_change_pct,
    .enviro_factors = .enviro_factors
  )


  # All homes receive effective messages, use in-home display, and smart meters (reduce HH energy use by 11%)
  # s_percent_of_homes_changes_behaviors <- 1


  # s_percent_of_additional_households_with_heating_electrified <- 0.59

  return(tb05)
}
