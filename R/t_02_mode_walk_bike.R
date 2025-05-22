#' @title Scenario for walk and bike
#' @family passenger
#' @family transportation
#'
#' @description Calculates scenario for walk and bike.
#'
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#' @export
#'
#' @importFrom emo ji
#' @importFrom cli cli_alert_success

mode_walk_bike <- function(.pass_tb = transportation_data$passenger,
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
                           .elast = elast,
                           .fuel_economy = fuel_economy,
                           .factor_values = factor_values,
                           .elast_5d = elast_5d) {
  # cli::cli_progress_message("** calculating scenario walk and bike \n")
  .pass_tb <- filter_ctu(.pass_tb, .selected_ctu)

  fcm <- calc_fuel_cost_mile(
    .pass_tb,
    .mode = "PLDV",
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "SIMPG",
    .fuel_cost_gallon = .enviro_factors$SI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  type <- "P"
  # For all passenger modes, variable = PMT
  var <- "PMT"

  # browser()
  ## Walk -----
  # mode <- "WALK"
  # stock <- ""
  # class <- "WALK"

  walk_vmt <-
    calc_vmt_forecast(
      tb = .pass_tb,
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      .mode = "WALK",
      .stock = "",
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
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values,
      .elast_5d = .elast_5d
    ) %>%
    mutate(class = "WALK")

  ## Bike -----
  # mode <- "BIKE"
  # stock <- ""
  # class <- "BIKE"

  bike_vmt <-
    calc_vmt_forecast(
      tb = .pass_tb,
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      .mode = "BIKE",
      .stock = "",
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
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values,
      .elast_5d = .elast_5d
    ) %>%
    mutate(class = "BIKE")

  # Finish up -----
  # browser()
  vmt_all <- dplyr::bind_rows(
    bike_vmt,
    walk_vmt
  )
  # browser()
  dir_ghg_all <- vmt_all %>%
    dplyr::mutate(
      dir_ghg = 0,
      type = type,
      class = mode,
    ) %>%
    dplyr::select(
      type, class, scenario, mode, ctu, year, aeo_mode,
      dir_ghg
    ) %>%
    unique()

  wb_fin <- list(
    "vmt" = vmt_all,
    "dir_ghg" = dir_ghg_all
  )

  cli::cli_alert_success(paste("Walk and bike", emo::ji("walking"), emo::ji("bike")))

  return(wb_fin)
}
