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
#' @importFrom cli cli_alert_success
mode_walk_bike <- function(.pass_tb = transportation_data$passenger,
                           .selected_ctu = "all",
                           .scenario = "BAU",
                           .electric_scenario = "ER",
                           .aeo_scenario = "REF",
                           .parking_cost = parking_cost,
                           .vehicle_occupancy = vehicle_occupancy,
                           .transit_avo_pct = transportation_defaults$transit_avo_pct,
                           .pldv_avo_pct = transportation_defaults$pldv_avo_pct,
                           .transit_service_pct = transportation_defaults$transit_service_pct,
                           .vmt_fee = transportation_defaults$vmt_fee,
                           .payd_fee = transportation_defaults$payd_fee,
                           .gas_tax = transportation_defaults$gas_tax,
                           .parking_price = transportation_defaults$parking_price,
                           .freight_parking_price = transportation_defaults$freight_parking_price,
                           .vmt_reduction_pct = transportation_defaults$vmt_reduction_pct,
                           .cong_price = transportation_defaults$cong_price,
                           .freight_vmt_fee = transportation_defaults$freight_vmt_fee,
                           .pop_dens_pct_change = transportation_defaults$pop_dens_pct_change,
                           .emp_dens_pct_change = transportation_defaults$emp_dens_pct_change,
                           .land_use_diversity_pct_change = transportation_defaults$land_use_diversity_pct_change,
                           .intersection_design_pct_change = transportation_defaults$intersection_design_pct_change,
                           .job_access_pct_change = transportation_defaults$job_access_pct_change,
                           .transit_dist_pct_change = transportation_defaults$transit_dist_pct_change,
                           .comb_5d_impact_pct_change = transportation_defaults$comb_5d_impact_pct_change,
                           .telework_pct = transportation_defaults$telework_pct,
                           .cbtp_start_year = transportation_defaults$cbtp_start_year,
                           .cbtp_prop_targeted = transportation_defaults$cbtp_prop_targeted,
                           .enviro_factors = enviro_factors,
                           .elast = elast,
                           .fuel_economy = fuel_economy,
                           .factor_values = factor_values,
                           .elast_5d = elast_5d,
                           .calc_transp_cost = FALSE,
                           .calc_transp_fuel_use = FALSE,
                           .calc_transp_ghg_embodied = FALSE) {
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
      .parking_cost = .parking_cost,
      .vehicle_occupancy = .vehicle_occupancy,
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
      .parking_cost = .parking_cost,
      .vehicle_occupancy = .vehicle_occupancy,
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

  vmt_all <- dplyr::bind_rows(
    bike_vmt,
    walk_vmt
  )

  dir_ghg_all <- vmt_all %>%
    dplyr::mutate(
      dir_ghg = 0,
      type = type,
      class = mode,
    ) %>%
    dplyr::select(
      type, class, scenario, mode, geog_name, geog_id, year, aeo_mode,
      dir_ghg
    ) %>%
    dplyr::distinct()

  wb_fin <- list(
    "vmt" = vmt_all,
    "dir_ghg" = dir_ghg_all
  )

  cli::cli_alert_success("Walk and bike 🚶 🚴")

  return(wb_fin)
}
