#' @title Calculate carbon sequestration by city/township
#' @family land use
#' @family emissions
#'
#' @description Calculates the total carbon sequestration
#'      as megagrams of carbon
#'      per hectare by land cover type by city/township.
#'
#'
#' @return [tibble::tibble()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(ghg.sp)
#'
#' calc_carbon_sequestration_per_ctu(
#'   tb = land_use_data,
#'   .selected_ctu = "all",
#'   .tree_planting_intervention = "tree_planting_on_all_pervious",
#'   .tree_planting_per_capita = 0.26,
#'   .tree_planting_per_hectare = 247,
#'   .parking_lot_reduction_percentage = 0.80,
#'   detail = FALSE
#' )
#' }
#'
calc_carbon_sequestration_per_ctu <- function(tb,
                                              .selected_ctu,
                                              .parking_lot_reduction_percentage,
                                              .tree_planting_intervention,
                                              .tree_planting_per_capita,
                                              .tree_planting_per_hectare,
                                              detail = FALSE) {
  csf <- ghg.sp::carbon_sequestration_factors

  # -------------------------------------------------------------------------
  parking_lot_land_cover <- calc_parking_lot_land_cover(
    tb = tb,
    .selected_ctu = .selected_ctu,
    .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
    .tree_planting_intervention = .tree_planting_intervention,
    .tree_planting_per_capita = .tree_planting_per_capita,
    .tree_planting_per_hectare = .tree_planting_per_hectare,
    detail = FALSE
  )

  # -------------------------------------------------------------------------

  carbon_sequestration_per_ctu <-
    parking_lot_land_cover %>%
    tidyr::pivot_wider(
      names_from = year,
      values_from = !ctu_name,
      names_sep = "."
    ) %>%
    dplyr::mutate(
      impervious.2040 = ((impervious.2016 + impervious.2040) / 2) *
        csf$IMPERVIOUS_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,
      grass.2040 = ((grass.2016 + grass.2040) / 2) *
        csf$GRASS_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,
      trees.2040 = ((trees.2016 + trees.2040) / 2) *
        csf$TREES_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,
      water.2040 = ((water.2016 + water.2040) / 2) *
        csf$WATER_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,
      barren.2040 = ((barren.2016 + barren.2040) / 2) *
        csf$BARREN_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,
      forest.2040 = ((forest.2016 + forest.2040) / 2) *
        csf$FOREST_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,
      shrub.2040 = ((shrub.2016 + shrub.2040) / 2) *
        csf$SHRUB_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,
      grassland.2040 = ((grassland.2016 + grassland.2040) / 2) *
        csf$GRASSLAND_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,
      agriculture.2040 = ((agriculture.2016 + agriculture.2040) / 2) *
        csf$AGRICULTURE_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,
      woody_wetland.2040 = ((woody_wetland.2016 + woody_wetland.2040) / 2) *
        csf$WOODY_WETLAND_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,
      wetland.2040 = ((wetland.2016 + wetland.2040) / 2) *
        csf$WETLAND_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,
      parking_lot.2040 = ((parking_lot.2016 + parking_lot.2040) / 2) *
        csf$PARKING_LOT_SEQUEST_MG_C_PER_HECTARE_PER_YEAR
    ) %>%
    dplyr::group_by(ctu_name) %>%
    tidyr::pivot_longer(
      cols = -ctu_name,
      names_to = c("var", "year"),
      names_sep = "\\."
    ) %>%
    dplyr::filter(
      var != "year",
      year != "2016"
    ) %>%
    # Use pivot_wider to pivot the year column
    tidyr::pivot_wider(names_from = var, values_from = value) %>%
    dplyr::select(
      ctu_name,
      year,
      agriculture,
      barren,
      forest,
      grass,
      grassland,
      impervious,
      parking_lot,
      shrub,
      trees,
      water,
      wetland,
      woody_wetland
    )

  return(carbon_sequestration_per_ctu)
}
