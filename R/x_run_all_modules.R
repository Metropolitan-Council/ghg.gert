#' @title run all modules
#'
#' @param run_land_use logical. Default is `TRUE`
#' @param run_buildings logical. Default is `TRUE`
#' @param run_transportation logical. Default is `TRUE`
#' @inheritParams run_scenario_land_use
#' @inheritParams run_scenario_building
#' @inheritParams run_scenario_transportation
#' @inheritParams filter_ctu
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
                            .calc_transp_cost = FALSE,
                            .calc_transp_fuel_cost_mile = FALSE,
                            .calc_transp_fuel_use = FALSE,
                            .calc_transp_ghg_embodied = FALSE,
                            detail = FALSE,
                            .renewable_ng_res = FALSE,
                            .conservation_tillage_intervention = "current_conservation_tillage",
                            .tree_planting_intervention = "none",
                            .tree_planting_per_capita = 0.0,
                            .tree_planting_per_hectare = 0,
                            .parking_lot_reduction_percentage = 0.0,
                            .electrified_buildings_pct = 0.00,
                            .renewable_ng_nonres = FALSE,
                            .smart_grid_energy_reduction_pct = 0,
                            .new_homes_to_multifamily_pct = 0,
                            .existing_high_efficiency_buildings_pct = 0.0,
                            .home_behavior_change_pct = 0,
                            .single_family_floor_area_growth_pct = 0.05,
                            .new_homes_affected_pct = 0.0,
                            .new_homes_leed_gold_pct = 0.0,
                            .existing_home_retrofit_pct = 0.0,
                            .existing_home_ultra_retrofit_pct = 0.0,
                            .additional_electrified_residential_buildings_pct = 0,
                            .grid_decarbonization_pct = enviro_factors$GRID_DECARBONIZATION_DEFAULT,
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
                            .mit_bau_summary = 0,
                            .enviro_factors = enviro_factors,
                            .factor_values = ghg.sp::factor_values,
                            .elast = elast,
                            .elast_5d = elast_5d) {
  output <- c()

  if (run_land_use == TRUE) {
    output$land_use <- run_scenario_land_use(
      tb = tb,
      .selected_ctu = .selected_ctu,
      .conservation_tillage_intervention = .conservation_tillage_intervention,
      .tree_planting_intervention = .tree_planting_intervention,
      .tree_planting_per_capita = .tree_planting_per_capita,
      .tree_planting_per_hectare = .tree_planting_per_hectare,
      .parking_lot_reduction_percentage = .parking_lot_reduction_percentage,
      detail = detail,
      .enviro_factors = .enviro_factors
    )
  }
  if (run_buildings == TRUE) {
    output$buildings <- run_scenario_building(
      res_tb = res_tb,
      non_res_tb = non_res_tb,
      res_tb_bau = res_tb_bau,
      non_res_tb_bau = non_res_tb_bau,
      run_residential = run_residential,
      run_non_residential = run_non_residential,
      .selected_ctu = .selected_ctu,
      .electrified_buildings_pct = .electrified_buildings_pct,
      .smart_grid_energy_reduction_pct = .smart_grid_energy_reduction_pct,
      .new_homes_to_multifamily_pct = .new_homes_to_multifamily_pct,
      .existing_high_efficiency_buildings_pct = .existing_high_efficiency_buildings_pct,
      .home_behavior_change_pct = .home_behavior_change_pct,
      .single_family_floor_area_growth_pct = .single_family_floor_area_growth_pct,
      .new_homes_affected_pct = .new_homes_affected_pct,
      .new_homes_leed_gold_pct = .new_homes_leed_gold_pct,
      .existing_home_retrofit_pct = .existing_home_retrofit_pct,
      .existing_home_ultra_retrofit_pct = .existing_home_ultra_retrofit_pct,
      .additional_electrified_residential_buildings_pct = .additional_electrified_residential_buildings_pct,
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .enviro_factors = .enviro_factors
    )
  }
  if (run_transportation == TRUE) {
    output$transp <- run_scenario_transportation(
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
      .mit_bau_summary = .mit_bau_summary,
      .elast = .elast,
      .elast_5d = .elast_5d,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values,
      .grid_decarbonization_pct = .grid_decarbonization_pct
    )
  }

  return(output)
}
