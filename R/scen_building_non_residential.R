#' @title Calculate Scenario Building Non-Residential
#'
#' @return
#' @export
scen_building_non_residential <-
  function(tb,
           .electrified_buildings_pct,
           .natural_gas_for_water_heating_pct,
           .natural_gas_for_space_heating_pct,
           .boiler_to_heat_pump_efficiency_ratio,
           .commercial_smart_grid_pct,
           .industrial_smart_grid_pct) {
    tb01 <- calc_ghg_non_residential(non_res_tb = tb,
                                     .commercial_smart_grid_pct = .commercial_smart_grid_pct,
                                     .industrial_smart_grid_pct = .industrial_smart_grid_pct,
                                     .smart_grid_energy_reduction_pct = .smart_grid_energy_reduction_pct,
                                     .grid_decarbonization_pct = .grid_decarbonization_pct,
                                     .existing_high_efficiency_buildings_pct = .existing_high_efficiency_buildings_pct)

    tb02 <- calc_electrify_commercial_heating(tb = tb01)

    tb03 <- calc_non_res_renewable_ng(tb = tb02) %>%
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

    return(tb03)

  }
