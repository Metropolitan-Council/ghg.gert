#'
#' @title Calculate scenario for freight multi-modal, air, and water transportation
#'
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#' @family transportation results, freight
#'
#' @export
#'
#' @importFrom cli cli_alert_success
mode_air_water_multi <- function(.freight_tb = transportation_data$freight,
                                 .selected_ctu = "all",
                                 .scenario = "BAU",
                                 .electric_scenario = "ER",
                                 .aeo_scenario = "REF",
                                 .parking_cost = parking_cost,
                                 .vehicle_occupancy = vehicle_occupancy,
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
                                 .grid_decarbonization_pct = 0.6,
                                 .enviro_factors = enviro_factors,
                                 .factor_values = factor_values,
                                 .elast = elast,
                                 .elast_5d = elast_5d,
                                 .fuel_economy = fuel_economy) {
  # cli::cli_progress_message("** calculating scenario for air and water travel \n")
  .freight_tb <- filter_ctu(.freight_tb, .selected_ctu)

  # Multimodal -----

  type <- "F"
  # For all freight, var = TMT
  var <- "TMT"

  mode <- "MM"

  ## CI -----
  # stock <- "CIStock"
  # mpg <- "CIMPG"
  # class <- "CI"
  message("Multimodal, diesel")

  fcm_mm_diesel <- calc_fuel_cost_mile(
    tb = .freight_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "CIMPG",
    .fuel_cost_gallon = .enviro_factors$CI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )


  mm_ci_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .freight_tb,
      .parking_cost = .parking_cost,
      .vehicle_occupancy = .vehicle_occupancy,
      .mode = "MM",
      .stock = "CIStock",
      .variable = var,
      .tb_fuel_cost_mile = fcm_mm_diesel,
      .aeo_scenario = .aeo_scenario,
      .transit_avo_pct = .transit_avo_pct,
      .pldv_avo_pct = .pldv_avo_pct,
      .transit_service_pct = .transit_service_pct,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .cong_price = .cong_price,
      .parking_price = .parking_price,
      .freight_parking_price = .freight_parking_price,
      .freight_vmt_fee = .freight_vmt_fee,
      .pop_dens_pct_change = .pop_dens_pct_change,
      .emp_dens_pct_change = .emp_dens_pct_change,
      .land_use_diversity_pct_change = .land_use_diversity_pct_change,
      .intersection_design_pct_change = .intersection_design_pct_change,
      .job_access_pct_change = .job_access_pct_change,
      .transit_dist_pct_change = .transit_dist_pct_change,
      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
      .telework_pct = .telework_pct,
      .phev_electric = .phev_electric,
      .enviro_factors = .enviro_factors,
      .elast = .elast,
      .elast_5d = .elast_5d,
      .factor_values = .factor_values
    ) %>%
    mutate(class = "CI")

  mm_ci_ghg <-
    calc_ghg_direct(
      tb_vmt = mm_ci_vmt,
      tb = .freight_tb,
      .mode = "MM",
      .fuel_type = "MMCI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "CIMPG",
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .fuel_economy = .fuel_economy,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values
    )



  ## battery electric-----
  # stock <- "BEVStock"
  # mpe <- "BEVElec"
  # class <- "BEV"
  message("Multimodal, battery electric")

  fcm_mm_bev <- calc_fuel_cost_mile(
    tb = .freight_tb,
    .mode = "MM",
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "BEVElec",
    .fuel_cost_gallon = .enviro_factors$ELEC_FUEL_COST_KWH,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )


  mm_bev_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .freight_tb,
      .mode = "MM",
      .stock = "BEVStock",
      .variable = var,
      .parking_cost = .parking_cost,
      .vehicle_occupancy = .vehicle_occupancy,
      .tb_fuel_cost_mile = fcm_mm_bev,
      .aeo_scenario = .aeo_scenario,
      .transit_avo_pct = .transit_avo_pct,
      .pldv_avo_pct = .pldv_avo_pct,
      .transit_service_pct = .transit_service_pct,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .cong_price = .cong_price,
      .parking_price = .parking_price,
      .freight_parking_price = .freight_parking_price,
      .freight_vmt_fee = .freight_vmt_fee,
      .pop_dens_pct_change = .pop_dens_pct_change,
      .emp_dens_pct_change = .emp_dens_pct_change,
      .land_use_diversity_pct_change = .land_use_diversity_pct_change,
      .intersection_design_pct_change = .intersection_design_pct_change,
      .job_access_pct_change = .job_access_pct_change,
      .transit_dist_pct_change = .transit_dist_pct_change,
      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
      .telework_pct = .telework_pct,
      .phev_electric = .phev_electric,
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .elast_5d = .elast_5d,
      .factor_values = .factor_values
    ) %>% mutate(class = "BEV")

  mm_bev_ghg <-
    calc_ghg_direct(
      tb_vmt = mm_bev_vmt,
      tb = .freight_tb,
      .mode = "MM",
      .fuel_type = .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "BEVElec",
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .fuel_economy = .fuel_economy,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values
    )


  # Air------
  # mode <- "AIR"

  ## SI
  # stock <- "SIStock"
  # mpg <- "SIMPG"
  # class <- "SI"

  message("Air, gasoline")

  fcm_air <- calc_fuel_cost_mile(
    tb = .freight_tb,
    .mode = "AIR",
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "SIMPG",
    .fuel_cost_gallon = .enviro_factors$SI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )


  air_si_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .freight_tb,
      .mode = "AIR",
      .stock = "SIStock",
      .parking_cost = .parking_cost,
      .vehicle_occupancy = .vehicle_occupancy,
      .variable = var,
      .tb_fuel_cost_mile = fcm_air,
      .aeo_scenario = .aeo_scenario,
      .transit_avo_pct = .transit_avo_pct,
      .pldv_avo_pct = .pldv_avo_pct,
      .transit_service_pct = .transit_service_pct,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .cong_price = .cong_price,
      .parking_price = .parking_price,
      .freight_parking_price = .freight_parking_price,
      .freight_vmt_fee = .freight_vmt_fee,
      .pop_dens_pct_change = .pop_dens_pct_change,
      .emp_dens_pct_change = .emp_dens_pct_change,
      .land_use_diversity_pct_change = .land_use_diversity_pct_change,
      .intersection_design_pct_change = .intersection_design_pct_change,
      .job_access_pct_change = .job_access_pct_change,
      .transit_dist_pct_change = .transit_dist_pct_change,
      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
      .telework_pct = .telework_pct,
      .phev_electric = .phev_electric,
      .enviro_factors = .enviro_factors,
      .elast = .elast,
      .elast_5d = .elast_5d,
      .factor_values = .factor_values
    ) %>% mutate(class = "SI")

  air_si_ghg <-
    calc_ghg_direct(
      tb_vmt = air_si_vmt,
      tb = .freight_tb,
      .mode = "AIR",
      .fuel_type = "ASI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "SIMPG",
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .fuel_economy = .fuel_economy,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values
    )


  # Water ------
  # mode <- "WAT"

  message("Water, diesel")

  ## CI -----
  # stock <- "CIStock"
  # mpg <- "CIMPG"
  # class <- "CI"

  fcm_wat <- calc_fuel_cost_mile(
    tb = .freight_tb,
    .mode = "WAT",
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "CIMPG",
    .fuel_cost_gallon = .enviro_factors$CI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )


  wat_ci_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .freight_tb,
      .mode = "WAT",
      .stock = "CIStock",
      .variable = var,
      .parking_cost = .parking_cost,
      .vehicle_occupancy = .vehicle_occupancy,
      .tb_fuel_cost_mile = fcm_wat,
      .aeo_scenario = .aeo_scenario,
      .transit_avo_pct = .transit_avo_pct,
      .pldv_avo_pct = .pldv_avo_pct,
      .transit_service_pct = .transit_service_pct,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .cong_price = .cong_price,
      .parking_price = .parking_price,
      .freight_parking_price = .freight_parking_price,
      .freight_vmt_fee = .freight_vmt_fee,
      .pop_dens_pct_change = .pop_dens_pct_change,
      .emp_dens_pct_change = .emp_dens_pct_change,
      .land_use_diversity_pct_change = .land_use_diversity_pct_change,
      .intersection_design_pct_change = .intersection_design_pct_change,
      .job_access_pct_change = .job_access_pct_change,
      .transit_dist_pct_change = .transit_dist_pct_change,
      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
      .telework_pct = .telework_pct,
      .phev_electric = .phev_electric,
      .enviro_factors = .enviro_factors,
      .elast = .elast,
      .elast_5d = .elast_5d,
      .factor_values = .factor_values
    ) %>% mutate(class = "CI")

  wat_ci_ghg <-
    calc_ghg_direct(
      tb_vmt = wat_ci_vmt,
      tb = .freight_tb,
      .mode = "WAT",
      .fuel_type = "WCI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "CIMPG",
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .fuel_economy = .fuel_economy,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values
    )



  # Finish up -----

  vmt_all <- dplyr::bind_rows(
    mm_ci_vmt,
    mm_bev_vmt,
    air_si_vmt,
    wat_ci_vmt
  )

  ghg_all <- dplyr::bind_rows(
    mm_ci_ghg,
    mm_bev_ghg,
    air_si_ghg,
    wat_ci_ghg
  )


  av_return <- list(
    "vmt" = vmt_all,
    "dir_ghg" = ghg_all
  )

  cli::cli_alert_success(paste(
    "Freight air, water, multimodal",
    "✈️",
    "🚢",
    "🐸"
  ))

  return(av_return)
}
