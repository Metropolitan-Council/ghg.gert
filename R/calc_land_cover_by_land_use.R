#' Title
#'
#' @param tb
#'
#' @return
#' @export
#'
#' @examples
p_land_cover_by_land_use <- function(tb) {
  dplyr::bind_rows(
    tb$land_cover_percentages_filled %>%
      dplyr::group_by(ctu_name, description_2, land_cover_description_2) %>%
      dplyr::transmute(land_cover_land_use_hectares = test * hectares) %>%
      dplyr::mutate(year = 2016),
    dplyr::right_join(
      tbland_cover_percentages_filled %>%
        dplyr::select(ctu_name, description_2, land_cover_description_2, test),
      p_summed_land_use_2040,
      by = c("ctu_name", "description_2")
    ) %>%
      dplyr::group_by(ctu_name, description_2, land_cover_description_2) %>%
      dplyr::transmute(land_cover_land_use_hectares = test * scenario_hectares) %>%
      dplyr::mutate(year = 2040)
  )
}
