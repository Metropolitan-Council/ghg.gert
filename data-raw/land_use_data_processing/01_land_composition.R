## ------------------------------------------------------------------------------------------------------------
p_land_use_by_cover_type_percent <-
  t_land_use_by_cover_type %>%
  dplyr::group_by(land_use_type) %>%
  dplyr::mutate(percent_of_total_area =
                  (total_area_m2 /
                     sum(total_area_m2))) %>%
  dplyr::ungroup()


## ------------------------------------------------------------------------------------------------------------
p_carbon_stock_by_cover_type <-
  p_land_use_by_cover_type_percent %>%
  base::merge(.,
              t_general_carbon_values,
              by = "land_cover_type") %>%
  dplyr::mutate(
    carbon_stock_by_cover_type_mg_c_per_ha =
      (percent_of_total_area *
         stock_mg_c_per_ha)
  )


## ------------------------------------------------------------------------------------------------------------
p_land_composition_ctu <-
  t_ctu_land_use_hectares %>%
  group_by(ctu_name, development_name, year, description, land_use_type) %>%
  summarise(hectares = sum(hectares, na.rm = TRUE),
            .groups = "drop") %>%
  base::merge(
    .,
    (
      t_ctu_land_use_hectares %>%
        dplyr::group_by(ctu_name, development_name, year) %>%
        dplyr::summarise(
          total_hectares = sum(hectares, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::ungroup()
    ),
    by = c("ctu_name", "development_name", "year")
  ) %>%
  dplyr::mutate(percent =
                  hectares /
                  total_hectares)

## ------------------------------------------------------------------------------------------------------------
p_land_by_development_type_sum_bau <-
  t_ctu_land_use_hectares %>%
  dplyr::filter(year == 2040) %>%
  dplyr::group_by(ctu_name) %>%
  dplyr::summarise(total_hectares_bau = sum(hectares)) %>%
  dplyr::ungroup()


## ------------------------------------------------------------------------------------------------------------
p_land_cover_percentages <-
  t_ctu_land_use_2016_land_cover %>%
  dplyr::group_by(ctu_name, land_use_type) %>%
  dplyr::mutate(total_hectares = sum(hectares)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ctu_name, land_use_type, land_cover_type) %>%
  dplyr::transmute(land_cover_percent = hectares / total_hectares) %>%
  dplyr::ungroup()


## ------------------------------------------------------------------------------------------------------------
p_summed_land_use_2016 <-
  t_ctu_land_use_hectares %>%
  dplyr::filter(year == 2016) %>%
  dplyr::group_by(ctu_name, land_use_type) %>%
  dplyr::summarise(hectares = sum(hectares),
                   .groups = "drop") %>%
  dplyr::ungroup()


## ------------------------------------------------------------------------------------------------------------
p_land_cover_percentages_filled <-
  p_summed_land_use_2016 %>%
  dplyr::left_join(
    .,
    (
      t_ctu_forecast %>%
        dplyr::distinct(ctu_name) %>%
        base::merge(.,
                    t_land_use_2016_types %>%
                      dplyr::select(land_use_type)) %>%
        base::merge(
          .,
          t_land_cover_types %>%
            dplyr::select(land_cover_type)
        ) %>% left_join(
          .,
          p_land_cover_percentages,
          by = c("ctu_name",
                 "land_use_type",
                 "land_cover_type")
        ) %>% dplyr::left_join(
          .,
          p_land_use_by_cover_type_percent,
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


## ------------------------------------------------------------------------------------------------------------
p_multifamily_mixed_area <-
  t_ctu_land_use_hectares %>%
  dplyr::group_by(ctu_name, development_name) %>%
  dplyr::filter(year == 2040) %>%
  dplyr::filter(
    land_use_type %in% c(
      "park_recreational_or_preserve",
      "mixed_use_commercial",
      "mixed_use_industrial",
      "mixed_use_residential",
      "multifamily"
    )
  ) %>%
  dplyr::summarise(hectares = sum(hectares), .groups = "drop") %>%
  dplyr::ungroup()


## ------------------------------------------------------------------------------------------------------------
p_land_by_development_type <- c()

# BAU Total
p_land_by_development_type$bau_total <-
  t_ctu_land_use_hectares %>%
  dplyr::filter(year == 2040) %>%
  dplyr::group_by(ctu_name, development_name) %>%
  dplyr::summarise(hectares = sum(hectares), .groups = "drop") %>%
  dplyr::mutate(scenario = "bau")

# BAU Mixed Use/Compact Zoning/Park
p_land_by_development_type$bau_mixed_use_compact_zoning_park <-
  p_multifamily_mixed_area %>%
  dplyr::mutate(scenario = "bau_mixed_use_compact_zoning")


## ------------------------------------------------------------------------------------------------------------
p_land_cover_percentages <-
  t_ctu_land_use_2016_land_cover %>%
  dplyr::group_by(ctu_name, land_use_type) %>%
  dplyr::mutate(total_hectares = sum(hectares)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ctu_name, land_use_type, land_cover_type) %>%
  dplyr::transmute(land_cover_percent = hectares / total_hectares) %>%
  dplyr::ungroup()
