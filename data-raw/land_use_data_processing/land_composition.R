# import tables
## -------------------------------------------------------------------------------------------
t_ctu_land_use_hectares <-
  import_from_emissions("metro_sp_mod_1.vw_ctu_land_use_hectares")

## ------------------------------------------------------------------------------------------------------------
p_land_composition_ctu <-
  t_ctu_land_use_hectares %>%
  base::merge(
    .,
    (
      t_ctu_land_use_hectares %>%
        dplyr::group_by(ctu_name, development_name, year) %>%
        dplyr::summarise(total_hectares = sum(hectares), .groups = 'drop') %>%
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

# import tables
## -------------------------------------------------------------------------------------------
t_ctu_land_use_2016_land_cover <- import_from_emissions("metro_sp_mod_1.vw_ctu_land_use_2016_land_cover")

## ------------------------------------------------------------------------------------------------------------
p_land_cover_percentages <-
  t_ctu_land_use_2016_land_cover %>%
  dplyr::group_by(ctu_name, description_2) %>%
  dplyr::mutate(total_hectares = sum(hectares)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ctu_name, description_2, land_cover_description_2) %>%
  dplyr::transmute(land_cover_percent = hectares / total_hectares) %>%
  dplyr::ungroup()
