#'
#' @title Calculate scenario for freight multi-modal, air, and water transportation
#'
#' @inheritParams run_scenario_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#' @family transportation results, freight
#'
#' @export
#'
#' @importFrom emo ji
#' @importFrom cli cli_alert_success
scen_air_water_multi <- function(.freight_tb = transportation_data$freight,
                                 .selected_ctu = "all",
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
                                 .grid_decarbonization_pct = 0.6,
                                 .enviro_factors = enviro_factors,
                                 .factor_values = factor_values,
                                 .elast = elast,
                                 .elast_5d = elast_5d) {
  # cli::cli_progress_message("** calculating scenario for air and water travel \n")
  .freight_tb <- filter_ctu(.freight_tb, .selected_ctu)

  # Multimodal -----

  type <- "F"
  # For all freight, var = TMT
  var <- "TMT"
  # browser()
  mode <- "MM"

  ## CI -----
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  message("Multimodal, diesel")

  fcm <- calc_fuel_cost_mile(
    tb = .freight_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = mpg,
    .fuel_cost_gallon = .enviro_factors$CI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )


  mm_ci_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .freight_tb,
      .mode = mode,
      .stock = stock,
      .variable = var,
      .tb_fuel_cost_mile = fcm,
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
    mutate(class = class)

  mm_ci_ghg <-
    calc_ghg_direct(
      tb_vmt = mm_ci_vmt,
      tb =  .freight_tb,
      .mode =  mode,
      .fuel_type = "MMCI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg,
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values
    )



  ## battery electric-----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"
  message("Multimodal, battery electric")

  fcm <- calc_fuel_cost_mile(
    tb = .freight_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = mpe,
    .fuel_cost_gallon = .enviro_factors$ELEC_FUEL_COST_KWH,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )


  mm_bev_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .freight_tb,
      .mode = mode,
      .stock = stock,
      .variable = var,
      .tb_fuel_cost_mile = fcm,
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
    ) %>% mutate(class = class)

  mm_bev_ghg <-
    calc_ghg_direct(
      tb_vmt = mm_bev_vmt,
      tb = .freight_tb,
      .mode = mode,
      .fuel_type = .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpe,
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values
    )


  # Air------
  mode <- "AIR"

  ## SI
  stock <- "SIStock"
  mpg <- "SIMPG"
  class <- "SI"

  message("Air, gasoline")

  fcm <- calc_fuel_cost_mile(
    tb = .freight_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = mpg,
    .fuel_cost_gallon = .enviro_factors$SI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )


  air_si_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .freight_tb,
      .mode = mode,
      .stock = stock,
      .variable = var,
      .tb_fuel_cost_mile = fcm,
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
    ) %>% mutate(class = class)

  air_si_ghg <-
    calc_ghg_direct(
      tb_vmt = air_si_vmt,
      tb = .freight_tb,
      .mode = mode,
      .fuel_type = "ASI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg,
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values
    )


  # Water ------
  mode <- "WAT"

  message("Water, diesel")

  ## CI -----
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"

  fcm <- calc_fuel_cost_mile(
    tb = .freight_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = mpg,
    .fuel_cost_gallon = .enviro_factors$CI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )


  wat_ci_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .freight_tb,
      .mode = mode,
      .stock = stock,
      .variable = var,
      .tb_fuel_cost_mile = fcm,
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
    ) %>% mutate(class = class)

  wat_ci_ghg <-
    calc_ghg_direct(
      tb_vmt = wat_ci_vmt,
      tb = .freight_tb,
      .mode = mode,
      .fuel_type = "WCI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg,
      .grid_decarbonization_pct = .grid_decarbonization_pct,
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
    "ghg" = ghg_all
  )

  cli::cli_alert_success(paste(
    "Freight air, water, multimodal",
    emo::ji("airplane"),
    emo::ji("ship"),
    emo::ji("frog")
  ))

  return(av_return)
}
