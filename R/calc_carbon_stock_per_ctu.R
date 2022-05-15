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
calc_carbon_stock_per_ctu <- function() {
  carbon_stock_per_ctu <-

    calc_parking_lot_land_cover()

  return(carbon_stock_per_ctu)
}
