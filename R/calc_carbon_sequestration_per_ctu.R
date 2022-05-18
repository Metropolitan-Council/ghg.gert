#' Title
#'
#' @param .impervious_sequest_mg_c_per_hectare_per_year
#' @param .grass_sequest_mg_c_per_hectare_per_year
#' @param .trees_sequest_mg_c_per_hectare_per_year
#' @param .water_sequest_mg_c_per_hectare_per_year
#' @param .barren_sequest_mg_c_per_hectare_per_year
#' @param .forest_sequest_mg_c_per_hectare_per_year
#' @param .shrub_sequest_mg_c_per_hectare_per_year
#' @param .grassland_sequest_mg_c_per_hectare_per_year
#' @param .agriculture_sequest_mg_c_per_hectare_per_year
#' @param .woody_wetland_sequest_mg_c_per_hectare_per_year
#' @param .wetland_sequest_mg_c_per_hectare_per_year
#' @param .parking_lot_sequest_mg_c_per_hectare_per_year
#'
#' @return
#' @export
#'
#' @examples
calc_carbon_sequestration_per_ctu <-
  function(.impervious_sequest_mg_c_per_hectare_per_year = 0,
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
           .parking_lot_sequest_mg_c_per_hectare_per_year = 0) {
    calc_parking_lot_land_cover() %>%
      group_by(ctu_name) %>%
      pivot_wider(
        names_from = year,
        values_from = !ctu_name,
        names_sep = "."
      ) %>%
      mutate(
        impervious = ((impervious.2016 + impervious.2040) / 2) *
          .impervious_sequest_mg_c_per_hectare_per_year,

        grass = ((grass.2016 + grass.2040) / 2) *
          .grass_sequest_mg_c_per_hectare_per_year,

        trees = ((trees.2016 + trees.2040) / 2) *
          .trees_sequest_mg_c_per_hectare_per_year,

        water = ((water.2016 + water.2040) / 2) *
          .water_sequest_mg_c_per_hectare_per_year,

        barren = ((barren.2016 + barren.2040) / 2) *
          .barren_sequest_mg_c_per_hectare_per_year,

        forest = ((forest.2016 + forest.2040) / 2) *
          .forest_sequest_mg_c_per_hectare_per_year,

        shrub = ((shrub.2016 + shrub.2040) / 2) *
          .shrub_sequest_mg_c_per_hectare_per_year,

        grassland = ((grassland.2016 + grassland.2040) / 2) *
          .grassland_sequest_mg_c_per_hectare_per_year,

        agriculture = ((agriculture.2016 + agriculture.2040) / 2) *
          .agriculture_sequest_mg_c_per_hectare_per_year,

        woody_wetland = ((
          woody_wetland.2016 + woody_wetland.2040
        ) / 2) *
          .woody_wetland_sequest_mg_c_per_hectare_per_year,

        wetland = ((
          wetland.2016 + wetland.2040
        ) / 2) *
          .wetland_sequest_mg_c_per_hectare_per_year,

        parking_lot = ((parking_lot.2016 + parking_lot.2040) / 2) *
          .parking_lot_sequest_mg_c_per_hectare_per_year

      ) %>%
      select(
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
