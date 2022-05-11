#' Calculate Total Plantable Area in Hectares by City
#'
#' @return
#' @export
#'
#' @examples
calc_total_plantable_area <- function() {
  calc_land_cover_by_city() %>%
    filter(
      land_cover_description_2 %in% c("grass",
                                      "barren",
                                      "shrub",
                                      "grassland",
                                      "agriculture")
    ) %>%
    group_by(ctu_name, year) %>%
    summarise(plantable_area_hectares = sum(land_cover_hectares))
}
