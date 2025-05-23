#' @title Calculate scenario building non-residential
#' @family commercial-industrial
#' @family buildings
#'
#' @description This function estimates the emissions of non-residential buildings under a given
#'    decarbonization scenario. It takes into account strategies such as energy efficiency improvements,
#'    electrification of heating systems, grid decarbonization, and renewable natural gas adoption.
#'
#' @note To run the Building Energy Module, refer to function run_scenario_building().
#' For more details, see `vignette("building_energy_module_inputs_non_residential")`.
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
#'      Data table with columns `geog_name`, `var`, `value`, `scen`, and `year`.
#'      The output of the non-residential portion of the Building Energy Module of the.
#'
#'      @field geog_name character, Name of the city/township.
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
#' library(ghg.ccap)
#'
#' scen_building_non_residential(
#'   non_res_tb = building_data$non_residential,
#'   non_res_tb_bau = building_data$non_residential,
#'   .selected_ctu = "all",
#'   .electrified_buildings_pct = 0.40,
#'   .smart_grid_energy_reduction_pct = 1.00,
#'   .grid_decarbonization_pct = 0.80,
#'   .existing_high_efficiency_buildings_pct = 0.80,
#'   .renewable_ng_nonres = FALSE,
#'   .enviro_factors = enviro_factors
#' )
#' }
scen_building_non_residential <- function(non_res_tb,
                                          non_res_tb_bau,
                                          .selected_ctu,
                                          .electrified_buildings_pct,
                                          .smart_grid_energy_reduction_pct,
                                          .grid_decarbonization_pct,
                                          .existing_high_efficiency_buildings_pct,
                                          .renewable_ng_nonres,
                                          .enviro_factors = enviro_factors) {
  # browser()

  non_res_tb <- filter_ctu(non_res_tb, .selected_ctu = .selected_ctu)

  non_res_tb_bau <- filter_ctu(non_res_tb_bau, .selected_ctu = .selected_ctu)

  tb01 <- calc_existing_comm_building_efficiency(
    non_res_tb = ghg.ccap::building_data$non_residential,
    .selected_ctu = .selected_ctu,
    .existing_high_efficiency_buildings_pct = .existing_high_efficiency_buildings_pct
  )
  # tb01 calculates energy efficiency reduction
  tb02 <- calc_ghg_non_residential(
    non_res_tb = tb01,
    non_res_tb_bau = non_res_tb_bau,
    .selected_ctu = .selected_ctu,
    .smart_grid_energy_reduction_pct = .smart_grid_energy_reduction_pct,
    .grid_decarbonization_pct = .grid_decarbonization_pct,
    .existing_high_efficiency_buildings_pct = .existing_high_efficiency_buildings_pct,
    .enviro_factors = .enviro_factors
  )
  # tb02 calculates conversion to electric heating
  tb03 <- calc_electrify_commercial_heating(
    non_res_tb = tb02,
    .selected_ctu = .selected_ctu,
    .electrified_buildings_pct = .electrified_buildings_pct,
    .grid_decarbonization_pct = .grid_decarbonization_pct,
    .enviro_factors = .enviro_factors
  )

  # tb03 calculates non residential renewable natural gas emissions reduction
  tb04 <- calc_non_res_renewable_ng(
    non_res_tb = tb03,
    .renewable_ng_nonres = .renewable_ng_nonres,
    .selected_ctu = .selected_ctu,
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
    ) %>%
    tidyr::pivot_wider(names_from = var, values_from = value) %>%
    dplyr::mutate(across(ends_with("kg_co"), ~ .x / 1000, .names = "{sub('kg_co', 'tonne', col)}"),
      total_industrial_commercial_emissions_tonnes = total_industrial_commercial_emissions / 1000
    ) %>%
    tidyr::pivot_longer(
      cols = commercial_mwh:total_industrial_commercial_emissions_tonnes,
      names_to = "var", values_to = "value"
    )

  return(tb05 %>% dplyr::mutate(year = as.numeric(year)))
}
