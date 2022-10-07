land_use_data <- c()

land_use_data$ctu_county <- t_ctu_county

land_use_data$ctu_forecast <- t_ctu_forecast

land_use_data$scenario_parameters <- t_scenario_parameters

land_use_data$land_cover_percentages_filled <-
  p_land_cover_percentages_filled

land_use_data$land_composition_ctu <- p_land_composition_ctu

land_use_data$ctu_land_use_hectares <- t_ctu_land_use_hectares

land_use_data$current_conservation_tillage_county <- t_current_conservation_tillage_county

land_use_data$land_by_development_type <- p_land_by_development_type

land_use_data$multifamily_mixed_area <- p_multifamily_mixed_area

usethis::use_data(land_use_data, overwrite = T)
