#' @title Calculate carbon stock by city/township
#' @family land use
#' @family emissions
#'
#' @description Calculates the carbon stock per land cover type by city/township
#'      under the selected scenario parameters
#'
#' @inheritParams calc_parking_lot_land_cover
#'
#' @return [tibble::tibble()] with column names...
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_carbon_stock_per_ctu(
#'   tb = land_use_data,
#'   .urban_form_scenario = "bau",
#'   .tree_planting_intervention = "tree_planting_on_all_pervious",
#'   .tree_planting_per_capita = 0.26,
#'   .tree_planting_per_hectare = 247,
#'   .parking_lot_reduction_percentage = 0.8,
#'   .conservation_tillage_intervention = "current_conservation_tillage",
#'   detail = FALSE
#' )
#' }
calc_carbon_stock_per_ctu <- function(tb,
                                      .urban_form_scenario,
                                      .tree_planting_intervention,
                                      .tree_planting_per_capita,
                                      .tree_planting_per_hectare,
                                      .parking_lot_reduction_percentage,
                                      .conservation_tillage_intervention,
                                      detail) {
  csf <- carbon_stock_factors

  # -------------------------------------------------------------------------
  conservation_tillage <-
    calc_conservation_tillage(
      tb = tb,
      detail = detail,
      .urban_form_scenario = .urban_form_scenario,
      .tree_planting_intervention = .tree_planting_intervention,
      .conservation_tillage_intervention = .conservation_tillage_intervention,
      .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
      .tree_planting_per_capita = .tree_planting_per_capita,
      .tree_planting_per_hectare = .tree_planting_per_hectare
    )


  # -------------------------------------------------------------------------
  parking_lot_land_cover <-
    calc_parking_lot_land_cover(
      tb = tb,
      .urban_form_scenario = .urban_form_scenario,
      .tree_planting_intervention = .tree_planting_intervention,
      .tree_planting_per_capita = .tree_planting_per_capita,
      .tree_planting_per_hectare = .tree_planting_per_hectare,
      .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
      detail = detail
    )

  # -------------------------------------------------------------------------
  carbon_stock_per_ctu <- parking_lot_land_cover %>%
    dplyr::mutate(
      grass = grass * csf$GRASS_STOCK_MG_C_PER_HECTARE,
      impervious = impervious * csf$IMPERVIOUS_STOCK_MG_C_PER_HECTARE,
      trees = trees * csf$TREES_STOCK_MG_C_PER_HECTARE,
      water = water * csf$WATER_STOCK_MG_C_PER_HECTARE,
      barren = barren * csf$BARREN_STOCK_MG_C_PER_HECTARE,
      forest = forest * csf$FOREST_STOCK_MG_C_PER_HECTARE,
      shrub = shrub * csf$SHRUB_STOCK_MG_C_PER_HECTARE,
      grassland = grassland * csf$GRASSLAND_STOCK_MG_C_PER_HECTARE,
      agriculture = agriculture * csf$AGRICULTURE_STOCK_MG_C_PER_HECTARE,
      woody_wetland = woody_wetland * csf$WOODY_WETLAND_STOCK_MG_C_PER_HECTARE,
      wetland = wetland * csf$WETLAND_STOCK_MG_C_PER_HECTARE,
      parking_lot = parking_lot * csf$PARKING_LOT_STOCK_MG_C_PER_HECTARE
    ) %>%
    dplyr::select(-c(total_area_hectares))

  # -------------------------------------------------------------------------
  return(carbon_stock_per_ctu)
}
