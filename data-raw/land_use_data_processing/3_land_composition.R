## ----setup, include=FALSE------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


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

