calc_land_cover_percentages <- function(){

  p_land_cover_percentages_filled <-
    (
      t_ctu_land_use_hectares %>%
        dplyr::filter(year == 2016) %>%
        dplyr::group_by(ctu_name, land_use_type) %>%
        dplyr::summarise(hectares = sum(hectares),
                         .groups = "drop") %>%
        dplyr::ungroup()
    ) %>%
    dplyr::left_join(
      .,
      (
        t_ctu_forecast %>%
          dplyr::distinct(ctu_name) %>%
          base::merge(.,
                      t_land_use_2016_types %>%
                        dplyr::select(land_use_type)) %>%
          base::merge(.,
                      t_land_cover_types %>%
                        dplyr::select(land_cover_type)) %>% left_join(
                          .,
                          (
                            t_ctu_land_use_2016_land_cover %>%
                              dplyr::group_by(ctu_name, land_use_type) %>%
                              dplyr::mutate(total_hectares = sum(hectares)) %>%
                              dplyr::ungroup() %>%
                              dplyr::group_by(ctu_name, land_use_type, land_cover_type) %>%
                              dplyr::transmute(land_cover_percent = hectares / total_hectares) %>%
                              dplyr::ungroup()
                          ),
                          by = c("ctu_name",
                                 "land_use_type",
                                 "land_cover_type")
                        ) %>% dplyr::left_join(
                          .,
                          (
                            t_land_use_by_cover_type %>%
                              dplyr::group_by(land_use_type) %>%
                              dplyr::mutate(percent_of_total_area =
                                              (total_area_m2 /
                                                 sum(total_area_m2))) %>%
                              dplyr::ungroup() %>%
                              dplyr::select(-c(total_area_m2))
                          ),
                          by = c("land_use_type",
                                 "land_cover_type")
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

  return(land_cover_percentages_filled)

}
