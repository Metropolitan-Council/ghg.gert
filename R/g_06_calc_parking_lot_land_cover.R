#' @title Calculate parking lot intervention effect on land cover
#' @family land use
#'
#' @description Recalculates the change in land cover types
#'      by city when implementing a
#'      parking lot reduction intervention, the percent reduction of parking lot area is
#'      defined in the argument `.parking_lot_reduction_percentage`
#'
#' @inheritParams calc_tree_planting_land_cover
#' @param .parking_lot_reduction_percentage numeric, value between `0` and `1`.
#'      The percentage reduction of parking lot
#'      area to be explored under the current scenario.
#'      Default is `0.8`.
#' @param detail logical,
#'      If `detail == TRUE` the function
#'      returns a tibble with more detailed fields. Recommended
#'      for debugging.
#'      Default is `FALSE`.
#'
#' @return [tibble::tibble()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_parking_lot_land_cover(
#'   tb = land_use_data,
#'   .urban_form_scenario = "bau",
#'   .parking_lot_reduction_percentage = 0.8,
#'   .tree_planting_intervention = "tree_planting_on_all_pervious",
#'   .tree_planting_per_capita = 0.26,
#'   .tree_planting_per_hectare = 247,
#'   detail = FALSE
#' )
#' }
calc_parking_lot_land_cover <- function(tb,
                                        .urban_form_scenario,
                                        .parking_lot_reduction_percentage,
                                        .tree_planting_intervention,
                                        .tree_planting_per_capita,
                                        .tree_planting_per_hectare,
                                        detail = FALSE) {
  # -------------------------------------------------------------------------
  tree_parking_land_cover <- calc_tree_planting_land_cover(
    tb = tb,
    .urban_form_scenario = .urban_form_scenario,
    .tree_planting_intervention = .tree_planting_intervention,
    .tree_planting_per_capita = .tree_planting_per_capita,
    .tree_planting_per_hectare = .tree_planting_per_hectare,
    detail = FALSE
  )


  # -------------------------------------------------------------------------
  parking_lot_land_cover <-
    tree_parking_land_cover %>%
    dplyr::mutate(
      parking_lot_2 =
        case_when(
          year == 2016 ~ parking_lot,
          year == 2040 &
            .urban_form_scenario == "compact_dev_with_drs" ~
            (parking_lot * (1 - .parking_lot_reduction_percentage)),
          year == 2040 &
            .urban_form_scenario != "compact_dev_with_drs" ~
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


  # -------------------------------------------------------------------------
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


  # -------------------------------------------------------------------------

  return(if (detail == TRUE) {
    parking_lot_land_cover
  } else {
    parking_lot_land_cover_short
  })
}
