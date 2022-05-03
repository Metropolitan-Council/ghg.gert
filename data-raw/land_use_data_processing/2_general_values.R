## ----setup, include=FALSE------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------------------------------------------
p_land_use_by_cover_type_percent <-
  t_land_use_by_cover_type %>%
  dplyr::group_by(description_2) %>%
  dplyr::mutate(percent_of_total_area =
                  (total_area_m2 /
                     sum(total_area_m2))) %>%
  dplyr::ungroup()


## ------------------------------------------------------------------------------------------------------------
p_carbon_stock_by_cover_type <-
  p_land_use_by_cover_type_percent %>%
  base::merge(.,
              t_general_carbon_values,
              by = "land_cover_description_2") %>%
  dplyr::mutate(
    carbon_stock_by_cover_type_mg_c_per_ha =
      (percent_of_total_area *
         stock_mg_c_per_ha)
  )

