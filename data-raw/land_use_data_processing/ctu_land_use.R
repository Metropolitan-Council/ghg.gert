land_use_data <- c()

land_use_data$land_use_by_cover_type_percent <-
  p_land_use_by_cover_type_percent

land_use_data$carbon_stock_by_cover_type <-
  p_carbon_stock_by_cover_type

land_use_data$land_cover_percentages_filled <-
  p_land_cover_percentages_filled

land_use_data$summed_land_use_2016 <- p_summed_land_use_2016

land_use_data$land_composition_ctu <- p_land_composition_ctu

land_use_data$ctu_land_use_hectares <- t_ctu_land_use_hectares

land_use_data$ctu_forecast <- t_ctu_forecast

land_use_data$scenario_parameters <- t_scenario_parameters

usethis::use_data(land_use_data, overwrite = T)
