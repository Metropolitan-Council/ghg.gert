#' Calculate GHG emissions from municipal wastewater using population growth and EPA State Inventory Tool methods.
#'
#' @param waste_inv table, waste inventory data
#' @param waste_future table, projected waste data
#'
#' @inheritParams calculate_mww_ch4_emissions
#' @inheritParams calculate_mww_n2o_direct_emissions
#' @inheritParams calculate_mww_n2o_effluent_emissions
#'
#' @return a list containing two data tables with geoid, source, inventory_year, value_activity,
#' units_activity, value_emissions, and units_emissions
#' @export
calculate_wastewater_emissions <- function(waste_inv,
                                         waste_future) {



  wastewater_emissions <- list()

  waste_inv <- waste_inv %>%
    dplyr::filter(source == "Wastewater") %>%
    mutate(
      MWW_CH4 = ghg.ccap::calculate_mww_ch4_emissions(population = geog_pop, years = inventory_year)$value_emissions,
      MWW_N20_direct = ghg.ccap::calculate_mww_n2o_direct_emissions(population = geog_pop, years = inventory_year)$value_emissions,
      MWW_N20_effluent = ghg.ccap::calculate_mww_n2o_effluent_emissions(population = geog_pop, years= inventory_year)$value_emissions
    ) %>%
    pivot_longer(
      cols = c(MWW_CH4, MWW_N20_direct, MWW_N20_effluent),
      names_to = "units_emissions",
      values_to = "value_emissions"
    ) %>%
    mutate(
      units_emissions = case_when(
        units_emissions == "MWW_CH4" ~ "Metric tons CH4",
        units_emissions == "MWW_N20_direct" ~ "Metric tons N2O direct",
        units_emissions == "MWW_N20_effluent" ~ "Metric tons N2O effluent"
      )
    )

  wastewater_emissions$inv <-  waste_inv %>%
    filter(units_emissions != "Metric tons CH4") %>%
    pivot_wider(names_from = units_emissions, values_from = value_emissions) %>%
    mutate(
      `Metric tons N2O` = `Metric tons N2O direct` + `Metric tons N2O effluent`
    ) %>%
    dplyr::select(-c(`Metric tons N2O direct`, `Metric tons N2O effluent`)) %>%
    pivot_longer(
      cols = `Metric tons N2O`,
      names_to = "units_emissions",
      values_to = "value_emissions"
    ) %>%
    bind_rows(waste_inv %>%
                filter(units_emissions == "Metric tons CH4")) %>%
    arrange(inventory_year, units_emissions)



  waste_future <- waste_future %>%
    dplyr::filter(source == "Wastewater") %>%
    mutate(
      MWW_CH4 = ghg.ccap::calculate_mww_ch4_emissions(population = geog_pop, years = inventory_year)$value_emissions,
      MWW_N20_direct = ghg.ccap::calculate_mww_n2o_direct_emissions(population = geog_pop, years = inventory_year)$value_emissions,
      MWW_N20_effluent = ghg.ccap::calculate_mww_n2o_effluent_emissions(population = geog_pop, years= inventory_year)$value_emissions
    ) %>%
    pivot_longer(
      cols = c(MWW_CH4, MWW_N20_direct, MWW_N20_effluent),
      names_to = "units_emissions",
      values_to = "value_emissions"
    ) %>%
    mutate(
      units_emissions = case_when(
        units_emissions == "MWW_CH4" ~ "Metric tons CH4",
        units_emissions == "MWW_N20_direct" ~ "Metric tons N2O direct",
        units_emissions == "MWW_N20_effluent" ~ "Metric tons N2O effluent"
      )
    )

  wastewater_emissions$future <-  waste_future %>%
    filter(units_emissions != "Metric tons CH4") %>%
    pivot_wider(names_from = units_emissions, values_from = value_emissions) %>%
    mutate(
      `Metric tons N2O` = `Metric tons N2O direct` + `Metric tons N2O effluent`
    ) %>%
    dplyr::select(-c(`Metric tons N2O direct`, `Metric tons N2O effluent`)) %>%
    pivot_longer(
      cols = `Metric tons N2O`,
      names_to = "units_emissions",
      values_to = "value_emissions"
    ) %>%
    bind_rows(waste_future %>%
                filter(units_emissions == "Metric tons CH4")) %>%
    arrange(inventory_year, units_emissions)


  return(wastewater_emissions)
}
