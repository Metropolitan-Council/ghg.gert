## ----setup, include=FALSE------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------------------------------------------
v_scenario <-
  t_scenario_parameters %>%
  dplyr::filter(scenario_description_2 ==  params$scenario)


## ------------------------------------------------------------------------------------------------------------
v_urban_expansion_relative_to_bau <-
  v_scenario$urban_expansion_relative_to_bau
v_urban_infill <- v_scenario$urban_infill


## ------------------------------------------------------------------------------------------------------------
v_parking_lot_reduction_percentage <- 0.8
v_agricultural_land_carbon_stock_mg_c_per_hectare <- 41
v_maximum_soc_accumulation_percent <- 1.54
v_w2w_diesel_emission_factor_kg_co2e_per_gal <- 12.50
v_avoided_emissions_tractor_use_mg_co2e_per_hectare <- 0.102
v_LA_tree_per_capita_planting_factor <- 0.26
v_tree_planting_per_hectare <- 247 

