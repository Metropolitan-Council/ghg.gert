#' @title Calculate scenario building non-residential
#' @family commercial-industrial
#' @family buildings
#'
#' @description compiles all the strategies related to
#' non-residential buildings.
#' @note To run the Building Energy Module, refer to function `run_scenario_building()`
#'     For more details, see `vignette("building_energy_module_inputs_non_residential")`
#'
#' @inheritParams calc_existing_comm_building_efficiency
#' @inheritParams calc_ghg_non_residential
#' @inheritParams calc_electrify_commercial_heating
#' @inheritParams calc_non_res_renewable_ng
#'
#'
#' @param non_res_tb [tibble::tibble()].
#'      Data table with non-residential building attributes. Package provided.
#'      Dataset `building_data$non_residential` is suitable and the default value.
#'      `non_res_tb_bau` is only used for the "business as usual" scenario, in contrast
#'      `non_res_tb` is used as the input for the decarbonization scenario.
#' @param non_res_tb_bau [tibble::tibble()].
#'      Data table with non-residential building attributes. Package provided dataset `building_data$non_residential`
#'      is suitable and the default value.
#'      `non_res_tb_bau` is only used for the "business as usual" scenario, in contrast
#'      `non_res_tb` is used as the input for the decarbonization scenario.
#'
#' @return [tibble::tibble()].
#'      Data table with columns `ctu_name`, `var`, `value`, `scen`, and `year`.
#'      The output of the non-residential portion of the Building Energy Module of the.
#'
#'      @field ctu_name character, Name of the city/township.
#'      @field year, numeric, Year.
#'      @field var character, one of `commercial_mwh`, `industrial_mwh`,
#'          `commercial_therms`, `industrial_therms`,
#'          `commercial_electricity_emissions_kg_co`,
#'          `industrial_electricity_emissions_kg_co`,
#'          `commercial_natural_gas_emissions_kg_co`,
#'          `industrial_natural_gas_emissions_kg_co`,
#'          or `total_industrial_commercial_emissions`.
#'      @field value, numeric. The numeric value of `var`.
#'      @field scen, character. One of  `bau` or `scenario`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' scen_building_non_residential(
#'   non_res_tb = building_data$non_residential,
#'   non_res_tb_bau = building_data$non_residential,
#'   .selected_ctu = "all",
#'   .electrified_buildings_pct = 0.40,
#'   .non_res_natural_gas_for_water_heating_pct = 0.20,
#'   .non_res_natural_gas_for_space_heating_pct = 0.69,
#'   .commercial_smart_grid_pct = 1.00,
#'   .industrial_smart_grid_pct = 1.00,
#'   .smart_grid_energy_reduction_pct = 1.00,
#'   .grid_decarbonization_pct = 0.80,
#'   .existing_high_efficiency_buildings_pct = 0.80,
#'   .enviro_factors = enviro_factors
#' )
#' }
scen_building_non_residential <- function(non_res_tb,
                                          non_res_tb_bau,
                                          .selected_ctu,
                                          .electrified_buildings_pct,
                                          .non_res_natural_gas_for_water_heating_pct,
                                          .non_res_natural_gas_for_space_heating_pct,
                                          .commercial_smart_grid_pct,
                                          .industrial_smart_grid_pct,
                                          .smart_grid_energy_reduction_pct,
                                          .grid_decarbonization_pct,
                                          .existing_high_efficiency_buildings_pct,
                                          .enviro_factors) {
  cat("** compiling non-residential strategies \n")
  tb01 <- ghg.sp::calc_existing_comm_building_efficiency(
    non_res_tb = building_energy_bau_data$non_residential,
    .existing_high_efficiency_buildings_pct = .existing_high_efficiency_buildings_pct
  )

  # tb01 calculates energy efficiency reduction
  tb02 <- ghg.sp::calc_ghg_non_residential(
    non_res_tb = tb01,
    non_res_tb_bau = non_res_tb_bau,
    .commercial_smart_grid_pct = .commercial_smart_grid_pct,
    .industrial_smart_grid_pct = .industrial_smart_grid_pct,
    .smart_grid_energy_reduction_pct = .smart_grid_energy_reduction_pct,
    .grid_decarbonization_pct = .grid_decarbonization_pct,
    .existing_high_efficiency_buildings_pct = .existing_high_efficiency_buildings_pct,
    .enviro_factors = .enviro_factors
  )
  # tb02 calculates conversion to electric heating
  tb03 <- ghg.sp::calc_electrify_commercial_heating(
    non_res_tb = tb02,
    .electrified_buildings_pct = .electrified_buildings_pct,
    .non_res_natural_gas_for_water_heating_pct = .non_res_natural_gas_for_water_heating_pct,
    .non_res_natural_gas_for_space_heating_pct = .non_res_natural_gas_for_space_heating_pct,
    .grid_decarbonization_pct = .grid_decarbonization_pct,
    .enviro_factors = .enviro_factors
  )

  # tb03 calculates non residential renewable natural gas emissions reduction
  tb04 <- ghg.sp::calc_non_res_renewable_ng(
    non_res_tb = tb03,
    .enviro_factors = .enviro_factors
  )

  tb05 <-
    tb04 %>%
    dplyr::filter(
      var %in% c(
        "commercial_mwh",
        "industrial_mwh",
        "commercial_therms",
        "industrial_therms",
        "commercial_electricity_emissions_kg_co",
        "industrial_electricity_emissions_kg_co",
        "commercial_natural_gas_emissions_kg_co",
        "industrial_natural_gas_emissions_kg_co",
        "total_industrial_commercial_emissions"
      )
    )

  return(tb04)
}
