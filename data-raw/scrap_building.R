pkgload::load_all()

building_data$residential %>%
  calc_ghg_floor_area()

scen_building_residential(.home_behavior_change_pct = 0.8) %>%
  calc_ghg_floor_area()



scen_building_residential(
  tb = building_data$residential,
  .new_homes_to_multifamily_pct = 0.5,
  .single_family_floor_area_growth_pct = 0.15,
  .new_homes_affected_pct = 0,
  .new_homes_leed_gold_pct = 0,
  .existing_home_retrofit_pct = 0,
  .existing_home_ultra_retrofit_pct = 0,
  .home_behavior_change_pct = 0,
  # .homes_electric_heating_pct = 0.59,
  .enviro_factors = enviro_factors
) %>%
  calc_ghg_floor_area()
