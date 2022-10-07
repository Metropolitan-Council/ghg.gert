land_use_data <- c()

land_use_data$ctu_county <- t_ctu_county
land_use_data$ctu_forecast <- t_ctu_forecast
land_use_data$scenario_parameters <- t_scenario_parameters
land_use_data$ctu_land_use_hectares <- t_ctu_land_use_hectares
land_use_data$current_conservation_tillage_county <- t_current_conservation_tillage_county
land_use_data$land_use_types <- t_land_use_2016_types
land_use_data$land_cover_types <- t_land_cover_types
land_use_data$land_use_by_cover_type <- t_land_use_by_cover_type

usethis::use_data(land_use_data, overwrite = T)
