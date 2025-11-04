c(
  .selected_ctu = "all",
  run_land_use = TRUE,
  run_buildings = TRUE,
  run_residential = TRUE,
  run_non_residential = TRUE,
  run_transportation = TRUE,
  tb = land_use_data,
  non_res_tb = building_data$non_residential,
  res_tb = building_data$residential,
  res_tb_bau = building_data$residential,
  non_res_tb_bau = building_data$non_residential,
  pass_tb = transportation_data$passenger,
  freight_tb = transportation_data$freight,
  detail = FALSE,
  .renewable_ng_res = FALSE,
  .urban_form_scenario = "bau", # should be env factor
  .conservation_tillage_intervention = "current_conservation_tillage",
  .tree_planting_intervention = "match_la_million_trees_goal",
  .tree_planting_per_capita = 0.0, # may be env factor
  .tree_planting_per_hectare = 0, # should be env factor
  .parking_lot_reduction_percentage = 0,
  .electrified_buildings_pct = 0.0,
  .renewable_ng_nonres = FALSE,
  # .non_res_natural_gas_for_water_heating_pct = 0.20, # should be env factor
  # .non_res_natural_gas_for_space_heating_pct = 0.69, # should be env factor
  # .commercial_smart_grid_pct = 1.00, # env factor
  # .industrial_smart_grid_pct = 1.00, # env factor
  .smart_grid_energy_reduction_pct = 0.0,
  .new_homes_to_multifamily_pct = 0.0,
  .existing_high_efficiency_buildings_pct = 0.0,
  .home_behavior_change_pct = 0.0,
  .single_family_floor_area_growth_pct = 0.05,
  .new_homes_affected_pct = 0.0,
  .new_homes_leed_gold_pct = 0.0,
  .existing_home_retrofit_pct = 0.0,
  .existing_home_ultra_retrofit_pct = 0.0,
  # .res_natural_gas_for_space_heating_pct = 0.71, # env factor
  # .res_natural_gas_for_water_heating_pct = 0.24, # env factor
  .additional_electrified_residential_buildings_pct = 0,
  .grid_decarbonization_pct = 0.6,
  .scenario = "BAU",
  .electric_scenario = "ER",
  .aeo_scenario = "REF",
  .transit_avo_pct = 0,
  .pldv_avo_pct = 0,
  .transit_service_pct = 0,
  .vmt_fee = 0,
  .payd_fee = 0,
  .gas_tax = 0,
  .parking_price = 0,
  .freight_parking_price = 0,
  .cong_price = 0,
  .freight_vmt_fee = 0,
  .drs_pct = 0,
  .av_pct = 0,
  .drs_fuel_type = "",
  .av_fuel_type = "",
  .pop_dens_pct_change = 0,
  .emp_dens_pct_change = 0,
  .land_use_diversity_pct_change = 0,
  .intersection_design_pct_change = 0,
  .job_access_pct_change = 0,
  .transit_dist_pct_change = 0,
  .comb_5d_impact_pct_change = 0,
  .telework_pct = 0,
  .bev_pct_sales = 0,
  .hev_pct_sales = 0,
  .enviro_factors = enviro_factors,
  .elast = elast,
  .elast_5d = elast_5d
)



bau <- ghg.ccap::run_all_modules(
  .selected_ctu = .selected_ctu,

  ## land use module parameters
  # make sure this is *always* "bau"
  .urban_form_scenario = "bau",
  .conservation_tillage_intervention = "current_conservation_tillage", # small use case, correct
  # trees
  .tree_planting_intervention = "match_la_million_trees_goal",
  .tree_planting_per_capita = 0,
  .tree_planting_per_hectare = 0,
  # for the purpose of planting trees
  .parking_lot_reduction_percentage = 0,

  ## building energy module parameters

  # 38% in 2018
  .grid_decarbonization_pct = 0.6, # user can modify
  ## non residential energy parameters
  .renewable_ng_nonres = FALSE,
  # switch to electric heating
  .electrified_buildings_pct = 0,
  # how effective is smart grid
  .smart_grid_energy_reduction_pct = 0,
  ## residential energy parameters
  .renewable_ng_res = FALSE,
  .new_homes_to_multifamily_pct = 0,
  .existing_high_efficiency_buildings_pct = 0,
  .home_behavior_change_pct = 0,
  .single_family_floor_area_growth_pct = 0.05,
  # pct of new homes that are affected by differnce in floor area
  .new_homes_affected_pct = 0,
  .new_homes_leed_gold_pct = 0,
  .existing_home_retrofit_pct = 0,
  .existing_home_ultra_retrofit_pct = 0,
  # % increase in residential that are electrical heating
  .additional_electrified_residential_buildings_pct = 0,

  ## transportation parameters
  .scenario = "bau",
  .electric_scenario = "ER",
  .aeo_scenario = "REF",
  .transit_avo_pct = 0,
  .pldv_avo_pct = 0,
  .transit_service_pct = 0,
  .vmt_fee = 0,
  .payd_fee = 0,
  .gas_tax = 0,
  .parking_price = 0,
  .freight_parking_price = 0,
  .cong_price = 0,
  .freight_vmt_fee = 0,
  .drs_pct = 0,
  .av_pct = 0,
  .pop_dens_pct_change = 0,
  .emp_dens_pct_change = 0,
  .land_use_diversity_pct_change = 0,
  .intersection_design_pct_change = 0,
  .job_access_pct_change = 0,
  .transit_dist_pct_change = 0,
  .comb_5d_impact_pct_change = 0,
  .telework_pct = 0,
  .bev_pct_sales = 0,
  .hev_pct_sales = 0
)
