## ----setup, include=FALSE------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------------------------------------------
p_summed_land_use_2040 <-
  p_scenario_land_use_2040 %>%
  dplyr::group_by(ctu_name, description_2) %>%
  dplyr::summarise(scenario_hectares = sum(scenario_hectares),
                   .groups = 'drop') %>%
  dplyr::group_by(ctu_name) %>%
  dplyr::mutate(total_scenario_hectares = sum(scenario_hectares)) %>%
  dplyr::ungroup()

p_summed_land_use_2016 <-
  t_ctu_land_use_hectares %>%
  dplyr::filter(year == 2016) %>%
  dplyr::group_by(ctu_name, description_2) %>%
  dplyr::summarise(hectares = sum(hectares),
                   .groups = 'drop') %>%
  dplyr::ungroup()

