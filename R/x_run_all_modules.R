#' @title run all modules
#'
#' @param run_land_use logical. Default is `TRUE`
#' @param run_buildings logical. Default is `TRUE`
#' @param run_transportation logical. Default is `TRUE`
#'
#' @inheritParams run_scenario_land_use
#' @inheritParams run_scenario_building
#' @inheritParams scen_building_residential
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams scen_building_non_residential
#' @inheritParams filter_ctu
#' @inheritParams vmt_annual_energy_outlook
#' @inheritParams vmt_land_use_change
#' @inheritParams vmt_parking_policy
#' @inheritParams vmt_road_policy
#' @inheritParams vmt_telework
#' @inheritParams vmt_stock_proportion
#' @inheritParams vmt_transit_service
#' @inheritParams vmt_vehicle_occupancy
#' @inheritParams adj_unit_counts
#' @inheritParams calc_floor_area_leed
#' @inheritParams calc_floor_area_growth
#' @inheritParams calc_floor_area_retrofit
#' @inheritParams calc_electrify_residential_heating
#' @inheritParams calc_floor_area_behavior_change
#' @inheritParams calc_ghg_residential
#' @inheritParams calc_ghg_non_residential
#' @inheritParams calc_residential_renewable_ng
#' @inheritParams calc_existing_comm_building_efficiency
#' @inheritParams calc_carbon_sequestration_per_ctu
#' @inheritParams calc_carbon_stock_per_ctu
#' @inheritParams calc_parking_lot_land_cover
#' @inheritParams calc_land_cover_by_land_use
#' @inheritParams calc_tree_planting_land_cover
#' @inheritParams calc_scen_land_use
#' @inheritParams calc_land_by_development_type
#' @inheritParams calc_electrify_commercial_heating
#'
#' @return list, list with the outputs of the three modules.
#' @export
#'
run_all_modules <- function(.selected_ctu = "all",
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
                            .parking_cost = parking_cost,
                            .calc_transp_cost = FALSE,
                            .calc_transp_fuel_cost_mile = FALSE,
                            .calc_transp_fuel_use = FALSE,
                            .calc_transp_ghg_embodied = FALSE,
                            detail = FALSE,
                            .renewable_ng_res = FALSE,
                            .conservation_tillage_intervention = "current_conservation_tillage",
                            .tree_planting_intervention = "none",
                            .tree_planting_per_capita = 0.26,
                            .tree_planting_per_hectare = 247,
                            .parking_lot_reduction_percentage = 0.0,
                            # electrification
                            .electrified_buildings_pct = 0.0,
                            # smartgrid
                            .smart_grid_energy_reduction_pct = 0.0,
                            # residential
                            # .renewable_ng_res = FALSE,
                            # .renewable_ng_nonres = FALSE,
                            # housing
                            # .new_homes_to_multifamily_pct = 0.0,
                            .existing_high_efficiency_buildings_pct = 0.0,
                            # .home_behavior_change_pct = 0.0,
                            # .single_family_floor_area_growth_pct = 0.05,
                            # .new_homes_affected_pct = 0.0,
                            .new_sf_homes_leed_gold_pct = 0.0,
                            .new_mf_homes_leed_gold_pct = 0.0,
                            .existing_sf_retrofit_pct = 0.0,
                            .existing_mf_retrofit_pct = 0.0,
                            # electrification
                            .sf_heat_pump_pct = 0.0,
                            .mf_heat_pump_pct = 0.0,
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
                            .pop_dens_pct_change = 0,
                            .emp_dens_pct_change = 0,
                            .land_use_diversity_pct_change = 0,
                            .intersection_design_pct_change = 0,
                            .job_access_pct_change = 0,
                            .transit_dist_pct_change = 0,
                            .comb_5d_impact_pct_change = 0,
                            .telework_pct = 0,
                            .bev_pct_sales = 0,
                            .phev_pct_sales = 0,
                            .hev_pct_sales = 0,
                            .enviro_factors = ghg.ccap::enviro_factors,
                            .factor_values = ghg.ccap::factor_values,
                            .elast = ghg.ccap::elast,
                            .fuel_economy = ghg.ccap::fuel_economy,
                            .grid_emissions = ghg.ccap::grid_emissions,
                            .elast_5d = elast_5d) {
  output <- c()

  if (run_buildings == TRUE) {
    output$buildings <- run_scenario_building(
      res_tb = res_tb,
      non_res_tb = non_res_tb,
      res_tb_bau = res_tb_bau,
      non_res_tb_bau = non_res_tb_bau,
      run_residential = run_residential,
      run_non_residential = run_non_residential,
      .selected_ctu = .selected_ctu,
      .scenario = .scenario,
      .density_output = run_scenario_land_use(
        tb = planned_land_use$ctu_planned_land_use_parcel,
        tb_strategy = NULL,
        .selected_ctu = .selected_ctu,
        .scenario = .scenario
      ),
      .electrified_buildings_pct = .electrified_buildings_pct,
      .smart_grid_energy_reduction_pct = .smart_grid_energy_reduction_pct,
      # .new_homes_to_multifamily_pct = .new_homes_to_multifamily_pct,
      .existing_high_efficiency_buildings_pct = .existing_high_efficiency_buildings_pct,
      # .home_behavior_change_pct = .home_behavior_change_pct,
      # .single_family_floor_area_growth_pct = .single_family_floor_area_growth_pct,
      # .new_homes_to_multifamily_pct = .new_homes_to_multifamily_pct,
      .new_sf_homes_leed_gold_pct = .new_sf_homes_leed_gold_pct,
      .new_mf_homes_leed_gold_pct = .new_mf_homes_leed_gold_pct,
      .existing_sf_retrofit_pct = .existing_sf_retrofit_pct,
      .existing_mf_retrofit_pct = .existing_mf_retrofit_pct,
      .sf_heat_pump_pct = .sf_heat_pump_pct,
      .mf_heat_pump_pct = .mf_heat_pump_pct,

      # .additional_electrified_residential_buildings_pct = .additional_electrified_residential_buildings_pct,
      # .grid_decarbonization_pct = .grid_decarbonization_pct,
      .enviro_factors = .enviro_factors,
      .grid_emissions = .grid_emissions
    )
  }
  if (run_transportation == TRUE) {
    output$transp <- run_module_transportation(
      pass_tb = pass_tb,
      freight_tb = freight_tb,
      .selected_ctu = .selected_ctu,
      .calc_transp_cost = .calc_transp_cost,
      .calc_transp_fuel_cost_mile = .calc_transp_fuel_cost_mile,
      .calc_transp_fuel_use = .calc_transp_fuel_use,
      .calc_transp_ghg_embodied = .calc_transp_ghg_embodied,
      .scenario = .scenario,
      .aeo_scenario = .aeo_scenario,
      .electric_scenario = .electric_scenario,
      .transit_avo_pct = .transit_avo_pct,
      .pldv_avo_pct = .pldv_avo_pct,
      .transit_service_pct = .transit_service_pct,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .parking_price = .parking_price,
      .freight_parking_price = .freight_parking_price,
      .cong_price = .cong_price,
      .freight_vmt_fee = .freight_vmt_fee,
      .pop_dens_pct_change = .pop_dens_pct_change,
      .emp_dens_pct_change = .emp_dens_pct_change,
      .land_use_diversity_pct_change = .land_use_diversity_pct_change,
      .intersection_design_pct_change = .intersection_design_pct_change,
      .job_access_pct_change = .job_access_pct_change,
      .transit_dist_pct_change = .transit_dist_pct_change,
      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
      .telework_pct = .telework_pct,
      .bev_pct_sales = .bev_pct_sales,
      .phev_pct_sales = .phev_pct_sales,
      .hev_pct_sales = .hev_pct_sales,
      .elast = .elast,
      .elast_5d = .elast_5d,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values,
      .grid_decarbonization_pct = .grid_decarbonization_pct
    )
  }

  return(output)
}
