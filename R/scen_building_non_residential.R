#' @title Calculate Scenario Building Non-Residential
#' @family building_energy_module
#'
#' @description `scen_building_non_residential` compiles all the strategies related to
#' non-residential buildings.
#'
#' @inheritParams calc_ghg_non_residential
#' @inheritParams calc_electrify_commercial_heating
#' @inheritParams calc_non_res_renewable_ng
#'
#'
#' @return A tibble.
#' @export
scen_building_non_residential <-
  function(tb,
           .electrified_buildings_pct,
           .natural_gas_for_water_heating_pct,
           .natural_gas_for_space_heating_pct,
           .boiler_to_heat_pump_efficiency_ratio,
           .commercial_smart_grid_pct,
           .industrial_smart_grid_pct,
           .smart_grid_energy_reduction_pct,
           .grid_decarbonization_pct,
           .existing_high_efficiency_buildings_pct,
           .enviro_factors) {
    # tb01 calculates energy efficiency reduction
    tb01 <- calc_ghg_non_residential(
      non_res_tb = tb,
      .commercial_smart_grid_pct = .commercial_smart_grid_pct,
      .industrial_smart_grid_pct = .industrial_smart_grid_pct,
      .smart_grid_energy_reduction_pct = .smart_grid_energy_reduction_pct,
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .existing_high_efficiency_buildings_pct = .existing_high_efficiency_buildings_pct,
      .enviro_factors = .enviro_factors
    )
    # tb02 calculates conversion to electric heating
    tb02 <- calc_electrify_commercial_heating(
      non_res_tb = tb01,
      .electrified_buildings_pct = .electrified_buildings_pct,
      .natural_gas_for_water_heating_pct = .natural_gas_for_water_heating_pct,
      .natural_gas_for_space_heating_pct = .natural_gas_for_space_heating_pct,
      .boiler_to_heat_pump_efficiency_ratio = .boiler_to_heat_pump_efficiency_ratio,
      .enviro_factors = .enviro_factors
    )
    # tb03 calculates non residential renewable natural gas emissions reduction
    tb03 <- calc_non_res_renewable_ng(
      non_res_tb = tb02,
      .enviro_factors = .enviro_factors)

    tb04 <-
      tb03 %>%
      tidyr::pivot_longer(
        cols = !ctu_name,
        names_to = c("var", "scen", "year"),
        names_sep = "[.]",
        values_to = "value"
      ) %>%
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
