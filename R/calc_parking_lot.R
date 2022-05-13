#' Calculate Parking Lot Intervention
#'
#' @family land_use_module
#'
#' @references
#'
#' @description Recalculates the change in land cover types by city when implementing a
#' parking lot reduction intervention, the percent reduction of parking lot area is
#' defined in the argument '.parking_lot_reduction_percentage'
#'
#' @param .parking_lot_reduction_percentage specifies the percentage reduction of parking lot
#' area to be explored under the current scenario
#'      Default is '0.8'
#'
#' @return
#' @export
#'
#' @examples
parking_lot_land_cover <-
  function(.parking_lot_reduction_percentage = 0.8,
           .luse_scen = "compact_dev_with_drs") {
    calc_tree_planting_land_cover() %>%
      dplyr::mutate(
        parking_lot_2 =
          case_when(
            year == 2016 ~ parking_lot,
            year == 2040 &
              .luse_scen == "compact_dev_with_drs"  ~
              (parking_lot * (1 - .parking_lot_reduction_percentage)),
            year == 2040 &
              .luse_scen != "compact_dev_with_drs" ~
              parking_lot
          ),
        decreased_parking_lot = parking_lot - parking_lot_2,
        scaling_factor = (total_area_hectares + decreased_parking_lot) / total_area_hectares,
        impervious =  impervious * scaling_factor,
        grass =  grass * scaling_factor,
        trees =  trees * scaling_factor,
        barren =  barren * scaling_factor,
        forest =  forest * scaling_factor,
        shrub = shrub * scaling_factor,
        grassland = grassland * scaling_factor,
        agriculture = agriculture * scaling_factor,
        woody_wetland = woody_wetland * scaling_factor,
        wetland = wetland * scaling_factor,
        parking_lot = parking_lot_2
      ) %>%
      select(
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
        woody_wetland,
        total_area_hectares
      )
  }
