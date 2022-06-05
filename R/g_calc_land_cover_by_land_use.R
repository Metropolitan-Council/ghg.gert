#' @title Calculate Land Cover by Land Use by City/Township
#' @family Land Use
#'
#' @description  `calc_land_cover_by_land_use` calculates the land cover by land use type for each community,
#'      using the bridge table "land_cover_percentages_filled"
#'      for the selected land use scenario.
#'
#' @inheritParams calc_scen_land_use
#'
#' @return **Tibble**.
#'      A long table with the percent of land cover for each land use type for each city*/township
#' @export
#'
#' @examples
#' \dontrun{
#' ghg.sp::calc_land_cover_by_land_use(
#'       tb = land_use_data,
#'       .scenario = "bau",
#'       .luse_scen = "compact_dev_with_drs")
#' }
calc_land_cover_by_land_use <- function(tb,
                                        .scenario,
                                        .luse_scen) {

  match.arg(arg = .scenario, choices = c("bau"))
  match.arg(arg = .luse_scen, choices = c("compact_dev_with_drs"))

  land_cover_by_land_use <-
    dplyr::bind_rows(
      tb$land_cover_percentages_filled %>%
        dplyr::group_by(ctu_name,
                        description_2,
                        land_cover_description_2) %>%
        dplyr::transmute(land_cover_land_use_hectares =
                           percent_land_cover_type  * hectares) %>%
        dplyr::mutate(year = 2016),
      dplyr::right_join(
        tb$land_cover_percentages_filled %>%
          dplyr::select(
            ctu_name,
            description_2,
            land_cover_description_2,
            percent_land_cover_type
          ),
        calc_scen_land_use(
          tb = tb,
          .scenario = .scenario,
          .luse_scen = .luse_scen
        ),
        by = c("ctu_name", "description_2")
      ) %>%
        dplyr::group_by(ctu_name,
                        description_2,
                        land_cover_description_2) %>%
        dplyr::transmute(land_cover_land_use_hectares =
                           percent_land_cover_type  * scenario_hectares) %>%
        dplyr::mutate(year = 2040)
    )

  return(land_cover_by_land_use)
}
