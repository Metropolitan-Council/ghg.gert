#' Calculate Land Cover by City/Township
#'
#' @description takes the output of the function 'calc_land_cover_by_land_use' and
#' calculates the total land cover by type for each community.
#'
#' @return
#' @export
#'
#' @examples
calc_land_cover_by_city <- function() {
  calc_land_cover_by_land_use() %>%
    group_by(ctu_name, year, land_cover_description_2) %>%
    summarise(land_cover_hectares = sum(land_cover_land_use_hectares))
}
