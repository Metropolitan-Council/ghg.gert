#' @title Calculate Carbon Sequestration by City/Township
#' @family Land Use
#' @family GHG Emissions
#'
#' @description calculates the total carbon sequestration
#'      per hectares by land cover type by city/township.
#'
#'
#' @return **Tibble**
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_carbon_sequestration_per_ctu(
#'      tb = land_use_data,
#'      .urban_form_scenario = "bau",
#'      .tree_planting_intervention = "tree_planting_on_all_pervious",
#'      .tree_planting_per_capita = 0.26,
#'      .tree_planting_per_hectare = 247,
#'      .parking_lot_reduction_percentage = 0.80,
#'      detail = FALSE
#'      )
#' }
#'
calc_carbon_sequestration_per_ctu <-
  function(tb,
           .urban_form_scenario,
           .parking_lot_reduction_percentage,
           .tree_planting_intervention,
           .tree_planting_per_capita,
           .tree_planting_per_hectare,
           detail = FALSE) {
    calc_parking_lot_land_cover(tb = tb,
                                .urban_form_scenario = .urban_form_scenario,
                                .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
                                .tree_planting_intervention = .tree_planting_intervention,
                                .tree_planting_per_capita = .tree_planting_per_capita,
                                .tree_planting_per_hectare = .tree_planting_per_hectare,
                                detail = FALSE) %>%
      dplyr::group_by(ctu_name) %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = !ctu_name,
        names_sep = "."
      ) %>%
      dplyr::mutate(
        impervious = ((impervious.2016 + impervious.2040) / 2) *
          carbon_sequestration_factors$IMPERVIOUS_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,

        grass = ((grass.2016 + grass.2040) / 2) *
          carbon_sequestration_factors$GRASS_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,

        trees = ((trees.2016 + trees.2040) / 2) *
          carbon_sequestration_factors$TREES_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,

        water = ((water.2016 + water.2040) / 2) *
          carbon_sequestration_factors$WATER_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,

        barren = ((barren.2016 + barren.2040) / 2) *
          carbon_sequestration_factors$BARREN_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,

        forest = ((forest.2016 + forest.2040) / 2) *
          carbon_sequestration_factors$FOREST_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,

        shrub = ((shrub.2016 + shrub.2040) / 2) *
          carbon_sequestration_factors$SHRUB_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,

        grassland = ((grassland.2016 + grassland.2040) / 2) *
          carbon_sequestration_factors$GRASSLAND_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,

        agriculture = ((agriculture.2016 + agriculture.2040) / 2) *
          carbon_sequestration_factors$AGRICULTURE_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,

        woody_wetland = ((
          woody_wetland.2016 + woody_wetland.2040
        ) / 2) *
          carbon_sequestration_factors$WOODY_WETLAND_SEQUEST_MG_C_PER_HECTARE_PER_YEAR ,

        wetland = ((wetland.2016 + wetland.2040) / 2) *
          carbon_sequestration_factors$WETLAND_SEQUEST_MG_C_PER_HECTARE_PER_YEAR,

        parking_lot = ((parking_lot.2016 + parking_lot.2040) / 2) *
          carbon_sequestration_factors$PARKING_LOT_SEQUEST_MG_C_PER_HECTARE_PER_YEAR

      ) %>%
      dplyr::select(
        ctu_name,
        year.2040,
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
  }
