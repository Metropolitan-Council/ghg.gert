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
#'   .selected_ctu = "all",
#'   .urban_form_scenario = "bau",
#'   .parking_lot_reduction_percentage = 0.8,
#'   .tree_planting_intervention = "tree_planting_on_all_pervious",
#'   .tree_planting_per_capita = 0.26,
#'   .tree_planting_per_hectare = 247,
#'   detail = FALSE
#' )
#' }
calc_parking_lot_land_cover <- function(tb,
                                        .selected_ctu,
                                        .urban_form_scenario,
                                        .parking_lot_reduction_percentage,
                                        .tree_planting_intervention,
                                        .tree_planting_per_capita,
                                        .tree_planting_per_hectare,
                                        detail = FALSE) {
  cli::cli_progress_message("*** calculating parking lot reduction strategy \n")

  # -------------------------------------------------------------------------
  tree_parking_land_cover <- calc_tree_planting_land_cover(
    tb = tb,
    .selected_ctu = .selected_ctu,
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

        dplyr::if_else(
          year == 2016, parking_lot,
          parking_lot * (1 - .parking_lot_reduction_percentage)
        ),
      decreased_parking_lot = parking_lot - parking_lot_2,
      scaling_factor = (total_area_hectares + decreased_parking_lot) / total_area_hectares
    ) %>%
    dplyr::mutate(
      impervious = impervious * scaling_factor,
      grass = grass * scaling_factor,
      trees = trees * scaling_factor,
      barren = barren * scaling_factor,
      forest = forest * scaling_factor,
      shrub = shrub * scaling_factor,
      grassland = grassland * scaling_factor,
      agriculture = agriculture * scaling_factor,
      woody_wetland = woody_wetland * scaling_factor,
      wetland = wetland * scaling_factor
    ) %>%
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
