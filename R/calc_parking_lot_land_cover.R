#' Calculate Parking Lot Intervention Effect on Land Cover
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
#'      Default is `0.8`
#'
#' @param detail Default is `FALSE`, returns a table with more detailed fields. Recommended
#' for debugging.
#'
#' @return a Tibble.
#' @export
#'
#' @examples
calc_parking_lot_land_cover <-
  function(.parking_lot_reduction_percentage = 0.8,
           .luse_scen = "compact_dev_with_drs",
           detail = FALSE) {
    parking_lot_land_cover <-
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
        scaling_factor = (total_area_hectares + decreased_parking_lot) / total_area_hectares
      ) %>%
      dplyr::mutate(dplyr::across(
        .cols = c(
          impervious,
          grass,
          trees,
          barren,
          forest,
          shrub,
          grassland,
          agriculture,
          woody_wetland,
          wetland
        ),
        ~ .x * scaling_factor
      )) %>%
      dplyr::mutate(parking_lot = parking_lot_2)

    parking_lot_land_cover_short <-
      parking_lot_land_cover %>%
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

    return(if (detail == TRUE) {
      parking_lot_land_cover
    } else {
      parking_lot_land_cover_short
    })
  }
