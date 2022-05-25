#' @title Calculate Residential Building Strategies
#' @family building_energy_module
#'
#' @description `scen_building_residential()` calculates the effect of the residential building
#' strategies within the building energy module.
#'
#' @param tb table, data table with residential building attributes. Package provided
#'     dataset `building_energy$residential` is suitable and the default value.
#'
#' @inheritParams adj_unit_counts
#' @inheritParams calc_floor_area_leed
#' @inheritParams calc_floor_area_growth
#' @inheritParams calc_floor_area_retrofit
#' @inheritParams run_scenario
#'
#'
#' @return Tibble.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' scen_building_residential(tb = building_data$residential)
#' }
scen_building_residential <-
  function(tb = building_data$residential,
           .new_homes_to_multifamily_pct = 0.50,
           .single_family_floor_area_growth_pct = 0.05,
           .new_homes_affected_pct = 0.50,
           .new_homes_leed_gold_pct = 0.50,
           .existing_home_retrofit_pct = 0.80,
           .existing_home_ultra_retrofit_pct = 0.20,
           .home_behavior_change_pct = 1.00,
           .homes_electric_heating_pct = 0.59,
           .enviro_factors = enviro_factors) {


    # browser()

    #B.R1 (MF to SF)
    tb01 <- adj_unit_counts(res_tb = tb,
                            .new_homes_to_multifamily_pct = .new_homes_to_multifamily_pct)

    #B.R2 (Affordable Floor Area)
    tb02 <- calc_affordable_floor_area(res_tb = tb01,
                                       .pct_growth_single_family_floor_area = .pct_growth_single_family_floor_area)

    #B.R3 (New Homes LEED Gold)
    tb03 <- calc_floor_area_leed(
      res_tb = tb02,
      .new_homes_leed_gold_pct = .new_homes_leed_gold_pct,
      .enviro_factors = .enviro_factors
    )

    #B.R4 + BR5 (Retrofit Homes)
    tb04 <- calc_floor_area_retrofit(
      res_tb = tb03,
      .existing_home_retrofit_pct = .existing_home_retrofit_pct,
      .existing_home_ultra_retrofit_pct = .existing_home_ultra_retrofit_pct,
      .enviro_factors = .enviro_factors
    )

    #B.R6 (Behavior Change)
    tb05 <- calc_floor_area_behavior_change(
      res_tb = tb04,
      .home_behavior_change_pct = .home_behavior_change_pct,
      .enviro_factors = .enviro_factors
    )

    return(tb05)
  }
