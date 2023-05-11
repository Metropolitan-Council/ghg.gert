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
#'   .selected_ctu = "all"
#' )
#' }
calc_land_cover_by_land_use <- function(tb,
                                        .selected_ctu,
                                        .enviro_factors = enviro_factors) {
  # -------------------------------------------------------------------------
  land_cover_percentages <- calc_land_cover_percentages(
    tb = tb,
    .selected_ctu = .selected_ctu
  )

  # -------------------------------------------------------------------------
  scen_land_use <-
    if (.selected_ctu == "all") {
      calc_scen_land_use(
        tb = tb,
        .selected_ctu
      )
    } else {
      calc_scen_land_use(
        tb = tb,
        .selected_ctu
      ) %>% filter(ctu_name == .selected_ctu)
    }

  # -------------------------------------------------------------------------
  land_cover_by_land_use <-
    dplyr::bind_rows(
      land_cover_percentages %>%
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
        land_cover_percentages %>%
          dplyr::select(
            ctu_name,
            land_use_type,
            land_cover_type,
            percent_land_cover_type
          ),
        scen_land_use %>%
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

  # -------------------------------------------------------------------------
  return(land_cover_by_land_use)
}
