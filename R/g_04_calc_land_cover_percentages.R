#' @title Calculate Baseline Land Cover Land Use Percentages by CTU
#' @export
#'
calc_land_cover_percentages <- function(tb = land_use_data,
                                        .selected_ctu) {

  cli::cli_progress_message("**** calculating land cover percentages \n")

  # -------------------------------------------------------------------------

  get_hectares_by_land_use_baseline_year <- (
    tb$ctu_land_use_hectares %>%
      dplyr::filter(year == 2016) %>%
      dplyr::group_by(ctu_name, land_use_type) %>%
      dplyr::summarise(
        hectares = sum(hectares),
        .groups = "drop"
      ) %>%
      dplyr::ungroup()
  )

  # -------------------------------------------------------------------------

  get_land_use_by_land_cover_baseline_year <-
    tb$ctu_land_use_2016_land_cover %>%
    dplyr::group_by(ctu_name, land_use_type) %>%
    dplyr::mutate(total_hectares = sum(hectares)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(ctu_name, land_use_type, land_cover_type) %>%
    dplyr::transmute(land_cover_percent = hectares / total_hectares) %>%
    dplyr::ungroup()


  # -------------------------------------------------------------------------
  # looks like I could get rid of this
  get_percent_of_land_use_bt_land_cover_baseline_year <-
    tb$land_use_by_cover_type %>%
    dplyr::group_by(land_use_type) %>%
    dplyr::mutate(
      percent_of_total_area =
        (total_area_m2 /
          sum(total_area_m2))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(total_area_m2))


  # -------------------------------------------------------------------------

  merge_data_sets <-
    get_hectares_by_land_use_baseline_year %>%
    dplyr::left_join(
      .,
      (
        tb$ctu_forecast %>%
          dplyr::distinct(ctu_name) %>%
          base::merge(
            .,
            tb$land_use_2016_types %>%
              dplyr::select(land_use_type)
          ) %>%
          base::merge(
            .,
            tb$land_cover_types %>%
              dplyr::select(land_cover_type)
          ) %>% left_join(
            .,
            (get_land_use_by_land_cover_baseline_year),
            by = c(
              "ctu_name",
              "land_use_type",
              "land_cover_type"
            )
          ) %>% dplyr::left_join(
            .,
            (get_percent_of_land_use_bt_land_cover_baseline_year),
            by = c(
              "land_use_type",
              "land_cover_type"
            )
          )
      ) %>%
        dplyr::select(
          ctu_name,
          land_cover_type,
          land_use_type,
          land_cover_percent,
          percent_of_total_area
        ),
      by = c("ctu_name", "land_use_type")
    )

  # -------------------------------------------------------------------------
  land_cover_percentages_filled <-
    merge_data_sets %>%
    dplyr::mutate(
      percent_land_cover_type = dplyr::if_else(
        hectares > 50,
        land_cover_percent,
        dplyr::if_else(is.na(percent_of_total_area),
          0,
          percent_of_total_area
        )
      )
    )

  # -------------------------------------------------------------------------

  return(land_cover_percentages_filled)
}
