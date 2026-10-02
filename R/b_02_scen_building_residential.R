#' @title Calculate residential building strategies
#' @family buildings, residential
#'
#' @description This function estimates the emissions of residential buildings under a user defined
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
#' @inheritParams calc_ghg_residential
#' @inheritParams calc_energy_residential
#' @inheritParams scen_building_non_residential
#' @inheritParams run_scenario_building
#' @inheritParams calc_vmt_forecast
#' @inheritParams run_all_modules
#' @inheritParams calc_housing_leed
#' @inheritParams calc_residential_retrofit
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
scen_building_residential <- function(res_tb = res_tb,
                                      res_tb_bau = res_tb_bau,
                                      .scenario = "",
                                      .selected_ctu,
                                      .baseline_year,
                                      .density_output,
                                      .leed_start_year,
                                      .new_sf_homes_leed_gold_pct,
                                      .new_mf_homes_leed_gold_pct,
                                      .retrofit_start_year,
                                      .retrofit_end_year,
                                      .existing_sf_retrofit_pct,
                                      .existing_mf_retrofit_pct,
                                      .heatpump_start_year,
                                      .heatpump_end_year,
                                      .sf_heatpump_pct,
                                      .mf_heatpump_pct) {

  # B.R1 (SF to MF)
  tb01 <- ghg.ccap::adj_unit_counts(
    res_tb = res_tb,
    density_output = .density_output,
    .selected_ctu = .selected_ctu
  )

  # B.R3 (New Homes LEED Gold)
  tb02 <- calc_housing_leed(
    res_tb = tb01,
    .selected_ctu = .selected_ctu,
    .leed_start_year = .leed_start_year,
    .new_sf_homes_leed_gold_pct = .new_sf_homes_leed_gold_pct,
    .new_mf_homes_leed_gold_pct = .new_mf_homes_leed_gold_pct
  )

  # B.R4 + BR5 (Retrofit Homes)
  tb03 <- calc_residential_retrofit(
    res_tb = res_tb,
    .selected_ctu = .selected_ctu,
    .retrofit_start_year = .retrofit_start_year,
    .retrofit_end_year = .retrofit_end_year,
    .existing_sf_retrofit_pct = .existing_sf_retrofit_pct,
    .existing_mf_retrofit_pct = .existing_mf_retrofit_pct
  )

  tb04 <- bind_rows(
    tb02,
    tb03
  )

  tb05 <- calc_residential_electrification(
    res_tb = tb04,
    .selected_ctu = .selected_ctu,
    .heatpump_start_year = .heatpump_start_year,
    .heatpump_end_year = .heatpump_end_year,
    .sf_heatpump_pct = .sf_heatpump_pct,
    .mf_heatpump_pct = .mf_heatpump_pct
  )

  # repeat with no changes for BAU scenario

  tb06 <- calc_housing_leed(
    res_tb = res_tb,
    .selected_ctu = .selected_ctu,
    .leed_start_year = .leed_start_year,
    .new_sf_homes_leed_gold_pct = 0,
    .new_mf_homes_leed_gold_pct = 0
  )

  tb07 <- calc_residential_retrofit(
    res_tb = res_tb,
    .selected_ctu = .selected_ctu,
    .retrofit_start_year = .retrofit_start_year,
    .retrofit_end_year = .retrofit_end_year,
    .existing_sf_retrofit_pct = 0,
    .existing_mf_retrofit_pct = 0
  )

  tb08 <- bind_rows(
    tb06,
    tb07
  )

  tb09 <- calc_residential_electrification(
    res_tb = tb08,
    .selected_ctu = .selected_ctu,
    .heatpump_start_year = .heatpump_start_year,
    .heatpump_end_year = .heatpump_end_year,
    .sf_heatpump_pct = 0,
    .mf_heatpump_pct = 0
  )

  tb10 <- calc_energy_residential(
    res_tb = tb05,
    res_tb_bau = tb09,
    .scenario = .scenario,
    .baseline_year = .baseline_year,
    .selected_ctu = .selected_ctu
  )

  tb_out <- calc_ghg_residential(
    res_energy = tb10,
    .selected_ctu = .selected_ctu
  )


  return(tb_out)
}
