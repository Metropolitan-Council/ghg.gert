#' @title Calculate Carbon Stock by City/Township
#' @family Land Use
#' @family GHG Emissions
#'
#' @description `calc_carbon_stock_per_ctu()` calculates the carbon stock per land cover type by city/township
#'      under the selected scenario parameters
#'
#' @inheritParams calc_parking_lot_land_cover
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' ghg.sp::calc_carbon_stock_per_ctu(
#'      tb = land_use_data,
#'      .urban_form_scenario = "bau",
#'      .tree_planting_intervention = "tree_planting_on_all_pervious",
#'      .tree_planting_per_capita = 0.26,
#'      .tree_planting_per_hectare = 247,
#'      .parking_lot_reduction_percentage = 0.8,
#'      .conservation_tillage_intervention = "current_conservation_tillage",
#'      .w2w_diesel_emission_factor_kg_co2e_per_gal = 12.50,
#'      .agricultural_land_carbon_stock_mg_c_per_hectare = 3,
#'      .maximum_soc_accumation_under_reduced_or_no_till_ag = 1.54,
#'      .avoided_emissions_tractor_use_mg_co2e_per_hectare = 0.0102,
#'      detail = FALSE
#' )
#' }
calc_carbon_stock_per_ctu <-
  function(tb,
           .urban_form_scenario,
           .tree_planting_intervention,
           .tree_planting_per_capita,
           .tree_planting_per_hectare,
           .parking_lot_reduction_percentage,
           .conservation_tillage_intervention,
           .w2w_diesel_emission_factor_kg_co2e_per_gal,
           .agricultural_land_carbon_stock_mg_c_per_hectare ,
           .maximum_soc_accumation_under_reduced_or_no_till_ag,
           .avoided_emissions_tractor_use_mg_co2e_per_hectare,
           detail) {

    calc_conservation_tillage(
      tb = tb,
      detail = detail,
      .urban_form_scenario = .urban_form_scenario,
      .tree_planting_intervention = .tree_planting_intervention,
      .conservation_tillage_intervention = .conservation_tillage_intervention,
      .w2w_diesel_emission_factor_kg_co2e_per_gal = .w2w_diesel_emission_factor_kg_co2e_per_gal,
      .avoided_emissions_tractor_use_mg_co2e_per_hectare = .avoided_emissions_tractor_use_mg_co2e_per_hectare,
      .agricultural_land_carbon_stock_mg_c_per_hectare = .agricultural_land_carbon_stock_mg_c_per_hectare,
      .maximum_soc_accumation_under_reduced_or_no_till_ag = .maximum_soc_accumation_under_reduced_or_no_till_ag,
      .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
      .tree_planting_per_capita = .tree_planting_per_capita,
      .tree_planting_per_hectare = .tree_planting_per_hectare
    )

    carbon_stock_per_ctu <- calc_parking_lot_land_cover(
        tb = tb,
        .urban_form_scenario = .urban_form_scenario,
        .tree_planting_intervention = .tree_planting_intervention,
        .tree_planting_per_capita = .tree_planting_per_capita,
        .tree_planting_per_hectare = .tree_planting_per_hectare,
        .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
        detail = detail
      ) %>%
      dplyr::mutate(
        grass = grass * carbon_stock_factors$GRASS_STOCK_MG_C_PER_HECTARE,
        impervious = impervious * carbon_stock_factors$IMPERVIOUS_STOCK_MG_C_PER_HECTARE,
        trees = trees * carbon_stock_factors$TREES_STOCK_MG_C_PER_HECTARE,
        water = water * carbon_stock_factors$WATER_STOCK_MG_C_PER_HECTARE,
        barren = barren * carbon_stock_factors$BARREN_STOCK_MG_C_PER_HECTARE,
        forest = forest * carbon_stock_factors$FOREST_STOCK_MG_C_PER_HECTARE,
        shrub = shrub * carbon_stock_factors$SHRUB_STOCK_MG_C_PER_HECTARE,
        grassland = grassland * carbon_stock_factors$GRASSLAND_STOCK_MG_C_PER_HECTARE,
        agriculture = agriculture * carbon_stock_factors$AGRICULTURE_STOCK_MG_C_PER_HECTARE,
        woody_wetland = woody_wetland * carbon_stock_factors$WOODY_WETLAND_STOCK_MG_C_PER_HECTARE,
        wetland = wetland * carbon_stock_factors$WETLAND_STOCK_MG_C_PER_HECTARE,
        parking_lot = parking_lot * carbon_stock_factors$PARKING_LOT_STOCK_MG_C_PER_HECTARE
      )
    return(carbon_stock_per_ctu)
  }
