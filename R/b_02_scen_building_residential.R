#' @title Calculate residential building strategies
#' @family buildings, residential
#'
#' @description This function estimates the emissions of non-residential buildings under a user defined
#'    decarbonization scenario. It takes into account strategies such as energy efficiency improvements,
#'    electrification of heating systems, grid decarbonization, and renewable natural gas adoption.
#'
#'
#' @note To run the Building Energy Module, refer to function `run_scenario_building()`
#'     For more details, see `vignette("building_energy_module_inputs_residential")`
#'
#' @param res_tb [tibble::tibble()], data table with residential building attributes.
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
#' @inheritParams scen_building_non_residential
#' @inheritParams run_scenario_building
#' @inheritParams calc_vmt_forecast
#' @inheritParams run_all_modules
#'
#'
#' @return [tibble::tibble()], Data table with columns
#'     `geog_name`, `var`, `value`, `scen`, and `year`.
#'
#'      @field `geog_name` character,  Name of the city/township.
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
#' library(ghg.ccap)
#'
#' scen_building_residential(
#'   res_tb = building_data$residential,
#'   res_tb_bau = building_data$residential,
#'   .selected_ctu = "all",
#'   .new_homes_to_multifamily_pct = 0.50,
#'   .single_family_floor_area_growth_pct = 0.05,
#'   .new_homes_affected_pct = 0.50,
#'   .new_homes_leed_gold_pct = 0.50,
#'   .existing_home_retrofit_pct = 0.80,
#'   .existing_home_ultra_retrofit_pct = 0.20,
#'   .home_behavior_change_pct = 1.00,
#'   .grid_decarbonization_pct = 1,
#'   .additional_electrified_residential_buildings_pct = 0.45,
#'   .enviro_factors = enviro_factors
#' )
#' }
scen_building_residential <- function(res_tb = res_tb,
                                      res_tb_bau = res_tb_bau,
                                      .selected_ctu = .selected_ctu,
                                      .new_homes_to_multifamily_pct = .new_homes_to_multifamily_pct,
                                      .single_family_floor_area_growth_pct,
                                      .new_homes_affected_pct,
                                      .new_homes_leed_gold_pct,
                                      .existing_home_retrofit_pct,
                                      .existing_home_ultra_retrofit_pct,
                                      .home_behavior_change_pct,
                                      .grid_decarbonization_pct,
                                      .additional_electrified_residential_buildings_pct,
                                      .renewable_ng_res,
                                      .enviro_factors = enviro_factors) {
  # cli::cli_progress_message("** compiling residential strategies \n")

  # browser()

  # B.R1 (MF to SF)
  tb01 <- ghg.ccap::adj_unit_counts(
    res_tb = res_tb,
    .selected_ctu = .selected_ctu,
    .new_homes_to_multifamily_pct = .new_homes_to_multifamily_pct
  )

  # # B.R2 (Floor Area change)
  # # removed this function from active use as it was causing differences between BAU and scenario (with no strategies selected)
  # tb02 <- calc_floor_area_growth(
  #   res_tb = tb01,
  #   .selected_ctu = .selected_ctu,
  #   .single_family_floor_area_growth_pct = .single_family_floor_area_growth_pct,
  #   .new_homes_affected_pct = .new_homes_affected_pct
  # )

  # B.R3 (New Homes LEED Gold)
  tb03 <- calc_floor_area_leed(
    res_tb = tb01,
    .selected_ctu = .selected_ctu,
    .new_homes_leed_gold_pct = .new_homes_leed_gold_pct,
    .enviro_factors = .enviro_factors
  )

  # B.R4 + BR5 (Retrofit Homes)
  tb04 <- calc_floor_area_retrofit(
    res_tb = tb03,
    .selected_ctu = .selected_ctu,
    .existing_home_retrofit_pct = .existing_home_retrofit_pct,
    .existing_home_ultra_retrofit_pct = .existing_home_ultra_retrofit_pct,
    .enviro_factors = .enviro_factors
  )

  # B.R6 (Behavior Change)
  tb05 <- calc_floor_area_behavior_change(
    res_tb = tb04,
    .selected_ctu = .selected_ctu,
    .home_behavior_change_pct = .home_behavior_change_pct,
    .enviro_factors = .enviro_factors
  )

  tb06 <- calc_ghg_residential(
    res_tb = tb05,
    res_tb_bau = res_tb_bau,
    .selected_ctu = .selected_ctu,
    .grid_decarbonization_pct = .grid_decarbonization_pct,
    .enviro_factors = .enviro_factors
  )

  # B.R (Electrify residential Buildings)
  tb07 <- calc_electrify_residential_heating(
    res_tb = tb06,
    .selected_ctu = .selected_ctu,
    .additional_electrified_residential_buildings_pct = .additional_electrified_residential_buildings_pct,
    .grid_decarbonization_pct = .grid_decarbonization_pct,
    .enviro_factors = .enviro_factors
  )

  # (Renewable Natural Gas)
  tb08 <- calc_residential_renewable_ng(
    res_tb = tb07,
    .selected_ctu = .selected_ctu,
    .renewable_ng_res = .renewable_ng_res,
    .enviro_factors = .enviro_factors
  )


  tb09 <-
    tb08 %>%
    dplyr::filter(
      var %in% c(
        "residential_mwh",
        "residential_therms",
        "residential_electricity_emissions_kg_co",
        "residential_natural_gas_emissions_kg_co"
      )
    ) %>%
    tidyr::pivot_wider(names_from = var, values_from = value) %>%
    dplyr::mutate(
      residential_electricity_emissions_tonne = residential_electricity_emissions_kg_co / 1000,
      residential_natural_gas_emissions_tonne = residential_natural_gas_emissions_kg_co / 1000
    ) %>%
    tidyr::pivot_longer(
      cols = residential_mwh:residential_natural_gas_emissions_tonne,
      names_to = "var", values_to = "value"
    )

  return(tb09 %>% dplyr::mutate(year = as.numeric(year)))
}
