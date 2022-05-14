#' Calculate Conservation Tillage
#'
#' @description calculates the impact of conservation tillage scenarios on carbon
#' stocks by city/township
#'
#' @family land_use_module
#'
#'
#' @param .w2w_diesel_emission_factor_kg_co2e_per_gal well-to-well diesel emissions
#' factor in kg of carbon dioxide equivalent (CO2e) per gallon
#'      Default is `12.50` kilograms of carbon dioxide equivalent (CO2e) per gallon of diesel
#'      @source `Hilman and Ramaswami, 2009`
#'
#' @param .avoided_emissions_tractor_use_mg_co2e_per_hectare
#'      Default is `0.102` million grams of carbon dioxide equivalent (CO2e) per hectare
#'      @source `United States Department of Agriculture`
#'
#' @param .agricultural_land_carbon_stock_mg_c_per_hectare
#'      Default is `41` million grams of carbon (C) per hectare
#'      @source `Tran et al., 2015` https://doi.org/10.1073/pnas.1512542112
#'
#'
#' @return
#' @export
#'
#' @examples
calc_conservation_tillage <- function(.w2w_diesel_emission_factor_kg_co2e_per_gal = 12.50,
                                      .avoided_emissions_tractor_use_mg_co2e_per_hectare = 0.0102,
                                      .agricultural_land_carbon_stock_mg_c_per_hectare = 3){

  calc_parking_lot_land_cover() %>%
    dplyr::select(ctu_name, year, agriculture) %>%
    pivot_wider(names_from = year, values_from = agriculture, names_prefix = 'agriculture_hectares_year_') %>%
    dplyr::mutate(baseline_carbon_stock_mg_co2_per_hectare = agriculture_hectares_year_2016 *
                    .agricultural_land_carbon_stock_mg_c_per_hectare)

}
