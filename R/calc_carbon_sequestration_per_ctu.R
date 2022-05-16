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

    calc_parking_lot_land_cover()

  }
