#' @title Calculate Residential Building Strategies
#' @family buildings, Residential
#'
#' @description calculates the effect of the residential building
#'     strategies within the building energy module.
#' @note To run the Building Energy Module, refer to function `run_scenario_building()`
#'
#' @param res_tb [tibble::tibble()], Data table with residential building attributes.
#'      Package provided dataset `building_energy$residential` is suitable and the
#'      default value.
#'     `res_tb_bau` is only used for the "business as usual" scenario, in contrast
#'     `res_tb` is used as the input for the decarbonization scenario.
#' @param res_tb_bau [tibble::tibble()]. Data table with residential building attributes.
#'     Package provided dataset `building_energy$residential` is suitable and
#'     the default value. `res_tb_bau` is only used for the "business as usual"
#'     scenario, in contrast `res_tb` is used as the input for the decarbonization scenario.
#'
#' @inheritParams adj_unit_counts
#' @inheritParams calc_floor_area_leed
#' @inheritParams calc_floor_area_growth
#' @inheritParams calc_floor_area_retrofit
#' @inheritParams calc_electrify_residential_heating
#' @inheritParams calc_floor_area_behavior_change
#' @inheritParams calc_ghg_residential
#' @inheritParams calc_residential_renewable_ng
#'
#'
#' @return [tibble::tibble()], Data table with columns
#'     `ctu_name`, `var`, `value`, `scen`, and `year`.
#'
#'      @field `ctu_name` character,  Name of the city/township.
#'      @field `year` numeric, Year.
#'      @field `var` character,. Can be `residential_mwh`, `residential_therms`,
#'           `residential_electricity_emissions_kg_co`,
#'           or `residential_natural_gas_emissions_kg_co`.
#'      @field value, numeric. The numeric value of `var`.
#'      @field scen, character. One of  `bau` or `scenario`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' scen_building_residential(
#'      tb = building_data$residential,
#'      res_tb_bau = building_data$residential,
#'      .new_homes_to_multifamily_pct = 0.50,
#'      .single_family_floor_area_growth_pct = 0.05,
#'      .new_homes_affected_pct = 0.50,
#'      .new_homes_leed_gold_pct = 0.50,
#'      .existing_home_retrofit_pct = 0.80,
#'      .existing_home_ultra_retrofit_pct = 0.20,
#'      .home_behavior_change_pct = 1.00,
#'      .grid_decarbonization_pct = 1,
#'      .additional_electrified_residential_buildings_pct = 0.45,
#'      .res_natural_gas_for_space_heating_pct = 0.71
#'      .res_natural_gas_for_water_heating_pct = 0.24,
#'      .enviro_factors = enviro_factors
#' )
#' }
scen_building_residential <- function(res_tb = res_tb,
                                      res_tb_bau = res_tb_bau,
                                      .new_homes_to_multifamily_pct = .new_homes_to_multifamily_pct,
                                      .single_family_floor_area_growth_pct,
                                      .new_homes_affected_pct,
                                      .new_homes_leed_gold_pct,
                                      .existing_home_retrofit_pct,
                                      .existing_home_ultra_retrofit_pct,
                                      .home_behavior_change_pct,
                                      .grid_decarbonization_pct,
                                      .additional_electrified_residential_buildings_pct,
                                      .res_natural_gas_for_space_heating_pct,
                                      .res_natural_gas_for_water_heating_pct,
                                      .boiler_to_heat_pump_efficiency_ratio,
                                      .enviro_factors = enviro_factors) {
  # browser()

  # B.R1 (MF to SF)
  tb01 <- adj_unit_counts(
    res_tb = res_tb,
    .new_homes_to_multifamily_pct = .new_homes_to_multifamily_pct
  )

  #B.R2 (Affordable Floor Area)
  tb02 <- calc_affordable_floor_area(
    res_tb = tb01,
    .single_family_floor_area_growth_pct = .single_family_floor_area_growth_pct
  )

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

  tb06 <- calc_ghg_residential(
    res_tb = tb05,
    res_tb_bau = res_tb_bau,
    .grid_decarbonization_pct = .grid_decarbonization_pct,
    .enviro_factors = .enviro_factors
  )

  #B.R (Electrify Residential Buildings)
  tb07 <- calc_electrify_residential_heating(
    res_tb = tb06,
    .additional_electrified_residential_buildings_pct = .additional_electrified_residential_buildings_pct,
    .res_natural_gas_for_space_heating_pct = .res_natural_gas_for_space_heating_pct,
    .res_natural_gas_for_water_heating_pct = .res_natural_gas_for_water_heating_pct,
    .boiler_to_heat_pump_efficiency_ratio = .boiler_to_heat_pump_efficiency_ratio,
    .grid_decarbonization_pct = .grid_decarbonization_pct,
    .enviro_factors = .enviro_factors
  )

  # (Renewable Natural Gas)
  tb08 <- calc_residential_renewable_ng(
    res_tb = tb07,
    .enviro_factors = .enviro_factors)


  tb09 <-
    tb08 %>%
    tidyr::pivot_longer(
      cols = !ctu_name,
      names_to = c("var", "scen", "year"),
      names_sep = "[.]",
      values_to = "value"
    ) %>%
    dplyr::filter(
      var %in% c(
        "residential_mwh",
        "residential_therms",
        "residential_electricity_emissions_kg_co",
        "residential_natural_gas_emissions_kg_co"
      )
    )

  return(tb09)

}
