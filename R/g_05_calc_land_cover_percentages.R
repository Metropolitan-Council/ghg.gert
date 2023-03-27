#' @title Calculate Baseline Land Cover Land Use Percentages by CTU
#'
#' @description This function calculates the baseline land cover land use percentages
#'      for each city/township (CTU) based on the 2016 data. It computes the
#'      percentage of each land use type and land cover type within the CTUs, considering
#'      both the total hectares of each land use type and the land cover distribution.
#'      The function returns a tibble containing the land cover land use percentages
#'      for each CTU, land use type, and land cover type.
#'
#' @param tb [tibble::tibble()].
#' The input dataset to be used. Default is `land_use_data`.
#' @param .selected_ctu character,
#' The selected city/township  (CTU) for which to calculate land cover land use percentages.
#'
#' @return [tibble::tibble()].
#'      A tibble containing the land cover land use percentages for each CTU,
#'      land use type, and land cover type based on 2016 data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_land_cover_percentages(
#'   tb = land_use_data,
#'   .selected_ctu = "all"
#' )
#' }
calc_land_cover_percentages <- function(tb = land_use_data,
                                        .selected_ctu) {

  # -------------------------------------------------------------------------
  ctu_land_use_hectares <- filter_ctu(tb$ctu_land_use_hectares, .selected_ctu)
  ctu_land_use_2016_land_cover <- filter_ctu(tb$ctu_land_use_2016_land_cover, .selected_ctu)
  ctu_forecast <- filter_ctu(tb$ctu_forecast, .selected_ctu)

  # -------------------------------------------------------------------------
  get_hectares_by_land_use_baseline_year <- (
    ctu_land_use_hectares %>%
      dplyr::filter(year == 2016) %>%
      dplyr::group_by(ctu_name, land_use_type) %>%
      dplyr::summarise(hectares = sum(hectares),
                       .groups = "drop") %>%
      dplyr::ungroup()
  )

  # -------------------------------------------------------------------------
  get_land_use_by_land_cover_baseline_year <-
    ctu_land_use_2016_land_cover %>%
    dplyr::group_by(ctu_name, land_use_type) %>%
    dplyr::mutate(total_hectares = sum(hectares)) %>%
    dplyr::group_by(ctu_name,
                    land_use_type,
                    land_cover_type) %>%
    dplyr::transmute(land_cover_percent =
                       hectares / total_hectares) %>%
    dplyr::ungroup()


  # -------------------------------------------------------------------------
  get_percent_of_land_use_by_land_cover_baseline_year <-
    tb$land_use_by_cover_type %>%
    dplyr::group_by(land_use_type) %>%
    dplyr::mutate(percent_of_total_area =
                    (total_area_m2 /
                       sum(total_area_m2))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(total_area_m2))


  # -------------------------------------------------------------------------
  land_cover_percentages_filled <-
    get_hectares_by_land_use_baseline_year %>%
    dplyr::left_join(
      .,
      ctu_forecast %>%
        dplyr::distinct(ctu_name) %>%
        dplyr::cross_join(.,
                          tb$land_use_2016_types %>%
                            dplyr::select(land_use_type)) %>%
        dplyr::cross_join(.,
                          tb$land_cover_types %>%
                            dplyr::select(land_cover_type)) %>%
        dplyr::left_join(
          .,
          (get_land_use_by_land_cover_baseline_year),
          by = c("ctu_name",
                 "land_use_type",
                 "land_cover_type")
        ) %>% dplyr::left_join(
          .,
          (get_percent_of_land_use_by_land_cover_baseline_year),
          by = c("land_use_type",
                 "land_cover_type")
        )
      %>%
        dplyr::select(
          ctu_name,
          land_cover_type,
          land_use_type,
          land_cover_percent,
          percent_of_total_area
        ),
      by = c("ctu_name", "land_use_type")
    ) %>%
    dplyr::mutate(
      percent_land_cover_type = dplyr::if_else(
        hectares > 50,
        land_cover_percent,
        dplyr::if_else(is.na(percent_of_total_area),
                       0,
                       percent_of_total_area)
      )
    )

  # -------------------------------------------------------------------------
  return(land_cover_percentages_filled)

  }
