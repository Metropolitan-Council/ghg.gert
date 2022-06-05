#' @title Run Green Infrastructure Module
#' @family land_use_module
#'
#' @description `scen_green_infrastrucutre` generates the outputs of the land use and green
#' infrastructure module.
#'
#' @inheritParams calc_land_by_development_type
#' @inheritParams calc_tree_planting_land_cover
#' @inheritParams calc_conservation_tillage
#' @inheritParams calc_parking_lot_land_cover
#' @inheritParams calc_carbon_sequestration_per_ctu
#' @inheritParams calc_carbon_stock_per_ctu
#'
#' @return
#' @export
#'
scen_green_infrastructure <- function(tb = land_use_data.rda,
                                      detail = FALSE,
                                      .luse_scen = "compact_dev_with_drs",
                                      .conservation_tillage_scen = "current_conservation_tillage",
                                      .w2w_diesel_emission_factor_kg_co2e_per_gal = 12.50,
                                      .avoided_emissions_tractor_use_mg_co2e_per_hectare = 0.0102,
                                      .agricultural_land_carbon_stock_mg_c_per_hectare = 3,
                                      .maximum_soc_accumation_under_reduced_or_no_till_ag = 1.54,
                                      .tree_planting_intervention = "tree_planting_on_all_pervious",
                                      .tree_planting_per_capita = 0.26,
                                      .tree_planting_per_hectare = 247,
                                      .parking_lot_reduction_percentage = 0.8,
                                      .impervious_sequest_mg_c_per_hectare_per_year = 0,
                                      .grass_sequest_mg_c_per_hectare_per_year = -0.42,
                                      .trees_sequest_mg_c_per_hectare_per_year = -1.27,
                                      .water_sequest_mg_c_per_hectare_per_year = 0,
                                      .barren_sequest_mg_c_per_hectare_per_year = -0.014,
                                      .forest_sequest_mg_c_per_hectare_per_year = -0.625,
                                      .shrub_sequest_mg_c_per_hectare_per_year = -0.287,
                                      .grassland_sequest_mg_c_per_hectare_per_year = -0.287,
                                      .agriculture_sequest_mg_c_per_hectare_per_year = -0.19,
                                      .woody_wetland_sequest_mg_c_per_hectare_per_year = -0.625,
                                      .wetland_sequest_mg_c_per_hectare_per_year = -1.493,
                                      .parking_lot_sequest_mg_c_per_hectare_per_year = 0,
                                      .impervious_stock_mg_c_per_hectare = 33,
                                      .grass_stock_mg_c_per_hectare = 77.04,
                                      .trees_stock_mg_c_per_hectare = 115,
                                      .water_stock_mg_c_per_hectare = 0,
                                      .barren_stock_mg_c_per_hectare = 5,
                                      .forest_stock_mg_c_per_hectare = 117,
                                      .shrub_stock_mg_c_per_hectare = 49,
                                      .grassland_stock_mg_c_per_hectare = 49,
                                      .agriculture_stock_mg_c_per_hectare = 41,
                                      .woody_wetland_stock_mg_c_per_hectare = 117,
                                      .wetland_stock_mg_c_per_hectare = 296.75,
                                      .parking_lot_stock_mg_c_per_hectare = 33) {
  calc_carbon_sequestration_per_ctu(
    .impervious_sequest_mg_c_per_hectare_per_year = .impervious_sequest_mg_c_per_hectare_per_year,
    .grass_sequest_mg_c_per_hectare_per_year = .grass_sequest_mg_c_per_hectare_per_year,
    .trees_sequest_mg_c_per_hectare_per_year = .trees_sequest_mg_c_per_hectare_per_year,
    .water_sequest_mg_c_per_hectare_per_year = .water_sequest_mg_c_per_hectare_per_year,
    .barren_sequest_mg_c_per_hectare_per_year = .barren_sequest_mg_c_per_hectare_per_year,
    .forest_sequest_mg_c_per_hectare_per_year = .forest_sequest_mg_c_per_hectare_per_year,
    .shrub_sequest_mg_c_per_hectare_per_year = .shrub_sequest_mg_c_per_hectare_per_year,
    .grassland_sequest_mg_c_per_hectare_per_year = .grassland_sequest_mg_c_per_hectare_per_year,
    .agriculture_sequest_mg_c_per_hectare_per_year = .agriculture_sequest_mg_c_per_hectare_per_year,
    .woody_wetland_sequest_mg_c_per_hectare_per_year = .woody_wetland_sequest_mg_c_per_hectare_per_year,
    .wetland_sequest_mg_c_per_hectare_per_year = .wetland_sequest_mg_c_per_hectare_per_year,
    .parking_lot_sequest_mg_c_per_hectare_per_year = .parking_lot_sequest_mg_c_per_hectare_per_year
  )

}
