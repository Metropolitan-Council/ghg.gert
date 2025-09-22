res_tb = building_data$residential
non_res_tb = building_energy_data$jobs
res_tb_bau = building_data$residential
non_res_tb_bau = building_energy_data$jobs
run_residential = TRUE
run_non_residential = FALSE
.baseline_year = 2022
# run_non_residential = TRUE
# selected CTU
.selected_ctu = "Minneapolis"
.scenario = "alt"
# non-residential
# electrification
.electrified_buildings_pct = 0.0
# smartgrid
.smart_grid_energy_reduction_pct = 0.0
# residential
# .renewable_ng_res = FALSE
# .renewable_ng_nonres = FALSE
# housing
.new_homes_to_multifamily_pct = 0.0
.existing_jobs_retrofit_pct = 0.0
# .home_behavior_change_pct = 0.0
# .single_family_floor_area_growth_pct = 0.05
# .new_homes_affected_pct = 0.0
.leed_start_year = 2028
.new_sf_homes_leed_gold_pct = 0.0
.new_mf_homes_leed_gold_pct = 0.0
.retrofit_start_year = 2028
.retrofit_end_year = 2050
.existing_sf_retrofit_pct = 0.0
.existing_mf_retrofit_pct = 0.0
# electrification
.heatpump_start_year = 2028
.heatpump_end_year = 2050
.sf_heat_pump_pct = 0.0
.mf_heat_pump_pct = 0.0
.jobs_heatpump_pct = 0.0
.app_elec_start_year = 2028
.app_elec_end_year = 2050
.sf_app_elec_pct = 0.0
.mf_app_elec_pct = 0.0
.grid_emissions = ghg.ccap::grid_emissions
.enviro_factors = ghg.ccap::enviro_factors
# .sf_elec_appliance_pct = 0.0
# # .mf_elec_appliance_pct = 0.0
# .additional_electrified_residential_buildings_pct = 0.0
