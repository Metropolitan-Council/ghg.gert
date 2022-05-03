## ------------------------------------------------------------------------------------------------------------
v_scenario <-
  t_scenario_parameters %>%
  dplyr::filter(scenario_description_2 ==  params$scenario)


## ------------------------------------------------------------------------------------------------------------
v_urban_expansion_relative_to_bau <-
  v_scenario$urban_expansion_relative_to_bau
v_urban_infill <- v_scenario$urban_infill


## ------------------------------------------------------------------------------------------------------------
v_maximum_soc_accumulation_percent <- 1.54
v_LA_tree_per_capita_planting_factor <- 0.26
v_tree_planting_per_hectare <- 247

