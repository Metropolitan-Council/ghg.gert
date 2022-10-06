#' @title Calculate land cover by land use and city/township
#' @family land use
#'
#' @description Calculates the land cover by land use type for each community,
#'      using the bridge table "land_cover_percentages_filled" for the selected land use scenario.
#'
#' @inheritParams calc_scen_land_use
#'
#' @return [tibble::tibble()].
#'      A long table with the percent of land cover for each land use type for each city*/township
#'
#' @export
#'
#' @examples
#' \dontrun{
#' calc_land_cover_by_land_use(
#'   tb = land_use_data,
#'   .urban_form_scenario = "bau"
#' )
#' }
calc_land_cover_by_land_use <- function(tb,
                                        .urban_form_scenario) {
  land_cover_by_land_use <-
    bind_rows(
      tb$land_cover_percentages_filled %>%
        dplyr::group_by(
          ctu_name,
          land_use_type,
          land_cover_type
        ) %>%
        dplyr::transmute(
          land_cover_land_use_hectares =
            percent_land_cover_type * hectares
        ) %>%
        dplyr::mutate(year = 2016),
      dplyr::right_join(
        tb$land_cover_percentages_filled %>%
          dplyr::select(
            ctu_name,
            land_use_type,
            land_cover_type,
            percent_land_cover_type
          ),
        calc_scen_land_use(
          tb = tb,
          .urban_form_scenario = .urban_form_scenario
        ) %>%
          dplyr::group_by(ctu_name, land_use_type) %>%
          dplyr::summarise(scenario_hectares = sum(scenario_hectares)),
        by = c("ctu_name", "land_use_type")
      ) %>%
        dplyr::group_by(
          ctu_name,
          land_use_type,
          land_cover_type
        ) %>%
        dplyr::mutate(
          land_cover_land_use_hectares =
            percent_land_cover_type * scenario_hectares
        ) %>%
        dplyr::mutate(year = 2040) %>%
        dplyr::select(
          ctu_name,
          year,
          land_use_type,
          land_cover_type,
          land_cover_land_use_hectares
        )
    )

  return(land_cover_by_land_use)
}
