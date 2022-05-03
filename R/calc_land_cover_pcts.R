p_land_cover_percentages_filled <-
  p_summed_land_use_2016 %>%
  dplyr::left_join(
    .,
    (
      t_ctu_forecast %>%
        dplyr::distinct(ctu_name) %>%
        base::merge(.,
                    t_land_use_2016_types %>%
                      dplyr::select(description_2)) %>%
        base::merge(
          .,
          t_land_cover_types %>%
            dplyr::select(land_cover_description_2)
        ) %>% left_join(
          .,
          p_land_cover_percentages,
          by = c("ctu_name",
                 "description_2",
                 "land_cover_description_2")
        ) %>% dplyr::left_join(
          .,
          p_land_use_by_cover_type_percent,
          by = c("description_2",
                 "land_cover_description_2")
        ) %>%
        dplyr::mutate(
          land_cover_percent2 =
            dplyr::case_when(
              land_cover_percent %>% is.na() ~ percent_of_total_area,
              land_cover_percent >= 0 ~ land_cover_percent
            )
        )
    ) %>%
      dplyr::select(
        ctu_name,
        land_cover_description_2,
        description_2,
        land_cover_percent,
        percent_of_total_area
      ),

    by = c("ctu_name", "description_2")
  ) %>%
  dplyr::mutate(test = dplyr::if_else(
    hectares > 50,
    land_cover_percent,
    dplyr::if_else(is.na(percent_of_total_area),
                   0,
                   percent_of_total_area)
  ))
