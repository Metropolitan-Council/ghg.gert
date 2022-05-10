#' Calculate Land Cover by Land Use
#'
#' @description  For the selected land use scenario, this function calculates the land cover
#' by land use type for each community, using the bridge table "land_cover_percentages_filled"
#'
#' @param tb the dataset to be used; defaults to "land_use_data.rda"
#'
#' @return A long table with the percent of land cover for each land use type for each city*/township
#' @export
#'
#' @examples
calc_land_cover_by_land_use <- function(tb = land_use_data) {
  dplyr::bind_rows(
    tb$land_cover_percentages_filled %>%
      dplyr::group_by(ctu_name,
                      description_2,
                      land_cover_description_2) %>%
      dplyr::transmute(land_cover_land_use_hectares = test * hectares) %>%
      dplyr::mutate(year = 2016),
    dplyr::right_join(
      tb$land_cover_percentages_filled %>%
        dplyr::select(ctu_name, description_2, land_cover_description_2, test),
      scen_land_use(),
      by = c("ctu_name", "description_2")
    ) %>%
      dplyr::group_by(ctu_name,
                      description_2,
                      land_cover_description_2) %>%
      dplyr::transmute(land_cover_land_use_hectares = test * scenario_hectares) %>%
      dplyr::mutate(year = 2040)
  )
}
