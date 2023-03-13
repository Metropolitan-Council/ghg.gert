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
#'   .selected_ctu = "all",
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
                                      .selected_ctu,
                                      .urban_form_scenario,
                                      .tree_planting_intervention,
                                      .tree_planting_per_capita,
                                      .tree_planting_per_hectare,
                                      .parking_lot_reduction_percentage,
                                      .conservation_tillage_intervention,
                                      detail) {
  cli::cli_progress_message("** calculating carbon stock \n")

  tb$ctu_forecast <- filter_ctu(tb$ctu_forecast, .selected_ctu = .selected_ctu)
  tb$ctu_land_use_hectares <- filter_ctu(tb$ctu_land_use_hectares, .selected_ctu = .selected_ctu)
  tb$ctu_land_use_2016_land_cover <- filter_ctu(tb$ctu_land_use_2016_land_cover, .selected_ctu = .selected_ctu)
  tb$ctu_county <- filter_ctu(tb$ctu_county, .selected_ctu = .selected_ctu)

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
      .selected_ctu = .selected_ctu,
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

  carbon_stock_per_ctu %>%
    dplyr::group_by(ctu_name) %>%
    tidyr::pivot_wider(
      values_from = c(grass, impervious, trees, water, barren, forest, shrub, grassland, agriculture, woody_wetland, wetland, parking_lot),
      names_from = "year"
    ) %>%
    dplyr::transmute(
      grass = ((grass_2016 - grass_2040) * 11 / 3) / 24,
      impervious = ((impervious_2040 - impervious_2016) * 11 / 3) / 24,
      trees = ((trees_2016 - trees_2040) * 11 / 3) / 24,
      water = ((water_2016 - water_2040) * 11 / 3) / 24,
      barren = ((barren_2016 - barren_2040) * 11 / 3) / 24,
      forest = ((forest_2016 - forest_2040) * 11 / 3) / 24,
      shrub = ((shrub_2016 - shrub_2040) * 11 / 3) / 24,
      grassland = ((grassland_2016 - grassland_2040) * 11 / 3) / 24,
      agriculture = ((agriculture_2016 - agriculture_2040) * 11 / 3) / 24,
      woody_wetland = ((woody_wetland_2016 - woody_wetland_2040) * 11 / 3) / 24,
      wetland = ((wetland_2016 - wetland_2040) * 11 / 3) / 24,
      parking_lot = ((parking_lot_2016 - parking_lot_2040) * 11 / 3) / 24
    )


  # -------------------------------------------------------------------------
  return(carbon_stock_per_ctu)
}
