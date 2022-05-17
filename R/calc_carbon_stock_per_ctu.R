#' @title Calculate Carbon Stock per City/Township
#'
#' @family land_use_module
#'
#' @description This function calculates the carbon stock per land cover type by city/township
#' under the selected scenario parameters
#'
#' @param .impervious_stock_mg_c_per_hectare A numeric object.
#' The estimated carbon stock of impervious surfaces in Mg (1 megagram = 1 tonne = 1 metric ton) of carbon per hectare.
#' The default is `33` megagrams of carbon per hectare.
#'
#' @param .grass_stock_mg_c_per_hectare A numeric object.
#' The estimated carbon stock of grass surfaces in Mg (1 megagram = 1 tonne = 1 metric ton) of carbon per hectare.
#' The default is `74.04` megagrams of carbon per hectare.
#'
#' @param .trees_stock_mg_c_per_hectare A numeric object.
#' The estimated carbon stock of trees surfaces in Mg (1 megagram = 1 tonne = 1 metric ton) of carbon per hectare.
#' The default is `115` megagrams of carbon per hectare.
#'
#' @param .water_stock_mg_c_per_hectare A numeric object.
#' The estimated carbon stock of water surfaces in Mg (1 megagram = 1 tonne = 1 metric ton) of carbon per hectare.
#' The default is `0` megagrams of carbon per hectare.
#'
#' @param .barren_stock_mg_c_per_hectare A numeric object.
#' The estimated carbon stock of barren surfaces in Mg (1 megagram = 1 tonne = 1 metric ton) of carbon per hectare.
#' The default is `5` megagrams of carbon per hectare.
#'
#' @param .forest_stock_mg_c_per_hectare A numeric object.
#' The estimated carbon stock of forest surfaces in Mg (1 megagram = 1 tonne = 1 metric ton) of carbon per hectare.
#' The default is `117` megagrammes of carbon per hectare.
#'
#' @param .shrub_stock_mg_c_per_hectare A numeric object.
#' The estimated carbon stock of shrub surfaces in Mg (1 megagram = 1 tonne = 1 metric ton) of carbon per hectare.
#' The default is `49` megagrams of carbon per hectare.
#'
#' @param .grassland_stock_mg_c_per_hectare A numeric object.
#' The estimated carbon stock of grass surfaces in Mg (1 megagram = 1 tonne = 1 metric ton) of carbon per hectare.
#' The default is `49` megagrammes of carbon per hectare.
#'
#' @param .agriculture_stock_mg_c_per_hectare A numeric object.
#' The estimated carbon stock of grass surfaces in Mg (1 megagram = 1 tonne = 1 metric ton) of carbon per hectare.
#' The default is `41` megagrams of carbon per hectare.
#'
#' @param .woody_wetland_stock_mg_c_per_hectare A numeric object.
#' The estimated carbon stock of grass surfaces in Mg (1 megagram = 1 tonne = 1 metric ton) of carbon per hectare.
#' The default is `117` megagrams of carbon per hectare.
#'
#' @param .wetland_stock_mg_c_per_hectare A numeric object.
#' The estimated carbon stock of grass surfaces in Mg (1 megagram = 1 tonne = 1 metric ton) of carbon per hectare.
#' The default is `296.75` megagrams of carbon per hectare.
#'
#' @param .parking_lot_stock_mg_c_per_hectare A numeric object.
#' The estimated carbon stock of grass surfaces in Mg (1 megagram = 1 tonne = 1 metric ton) of carbon per hectare.
#' The default is `33` megagrams of carbon per hectare.
#'
#' @return
#' @export
#'
#' @examples
calc_carbon_stock_per_ctu <-
  function(.impervious_stock_mg_c_per_hectare = 33,
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
    carbon_stock_per_ctu <-

      calc_parking_lot_land_cover() %>%
      mutate(
        grass = grass * .grass_stock_mg_c_per_hectare,
        impervious = impervious * .impervious_stock_mg_c_per_hectare,
        trees = trees * .trees_stock_mg_c_per_hectare,
        water = water * .water_stock_mg_c_per_hectare,
        barren = barren * .barren_stock_mg_c_per_hectare,
        forest = forest * .forest_stock_mg_c_per_hectare,
        shrub = shrub * .shrub_stock_mg_c_per_hectare,
        grassland = grassland * .grassland_stock_mg_c_per_hectare,
        agriculture = agriculture * .agriculture_stock_mg_c_per_hectare,
        woody_wetland = woody_wetland * .woody_wetland_stock_mg_c_per_hectare,
        wetland = wetland * .wetland_stock_mg_c_per_hectare,
        parking_lot = parking_lot * .parking_lot_stock_mg_c_per_hectare
      )

    return(carbon_stock_per_ctu)
  }
