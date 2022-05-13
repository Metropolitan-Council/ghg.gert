#' Calculate Total Plantable Area in Hectares by City
#'
#' @family land_use_module
#'
#' @return
#' @export
#'
#' @examples
calc_total_plantable_area <- function() {
  total_plantable_area <-
    calc_land_cover_by_city() %>%
    group_by(ctu_name, year) %>%
    pivot_wider(names_from = land_cover_description_2, values_from = land_cover_hectares) %>%
    mutate(
      plantable_area_hectares = (grass +  barren + shrub + grassland + agriculture),
      total_trees_hectares = (trees + forest + woody_wetland),
      total_area_hectares = (
        grass +  barren + shrub + grassland
        + agriculture + trees + forest + woody_wetland
        + impervious + water + wetland
      )
    )

  return(total_plantable_area)

}
