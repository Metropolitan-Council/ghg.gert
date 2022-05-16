#' @title Calculate Carbon Stock per City/Township
#'
#' @family land_use_module
#'
#' @description This function calculates the carbon stock per land cover type by city/township
#' under the selected scenario parameters
#'
#' @return
#' @export
#'
#' @examples
calc_carbon_stock_per_ctu <- function(.impervious_stock_mg_c_per_hectare = 33,
                                      .grass_stock_mg_c_per_hectare = 74.04,
                                      .trees_stock_mg_c_per_hectare = 115,
                                      .water_stock_mg_c_per_hectare = 0,
                                      .barren_stock_mg_c_per_hectare = 5,
                                      .forest_stock_mg_c_per_hectare = 117,
                                      .shrub_stock_mg_c_per_hectare = 49,
                                      .grassland_stock_mg_c_per_hectare = 49,
                                      .agriculture_stock_mg_c_per_hectare = 41,
                                      .woody_wetland_stock_mg_c_per_hectare = 117,
                                      .werland_stock_mg_c_per_hectare = 296.75,
                                      .parking_lot_stock_mg_c_per_hectare = 33) {
  carbon_stock_per_ctu <-

    calc_parking_lot_land_cover() %>%
    mutate(parking_lot = parking_lot * .parking_lot_stock_mg_c_per_hectare)

  return(carbon_stock_per_ctu)
}
