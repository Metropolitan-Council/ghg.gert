#' @title Calculate scenario for freight rail
#' @family freight
#' @family transportation
#'
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#' @return [tibble::tibble()] with column names...
#' @export
#'
#' @importFrom cli cli_alert_success
mode_freight_rail <- function(.freight_tb = transportation_data$freight,
                              .selected_ctu,
                              .scenario = "BAU",
                              .electric_scenario = "ER",
                              .aeo_scenario = "REF",
                              .parking_cost = parking_cost,
                              .vehicle_occupancy = vehicle_occupancy,
                              .transit_avo_pct = ghg.ccap::transportation_defaults$transit_avo_pct,
                              .pldv_avo_pct = ghg.ccap::transportation_defaults$pldv_avo_pct,
                              .transit_service_pct = ghg.ccap::transportation_defaults$transit_service_pct,
                              .vmt_fee = ghg.ccap::transportation_defaults$vmt_fee,
                              .payd_fee = ghg.ccap::transportation_defaults$payd_fee,
                              .gas_tax = ghg.ccap::transportation_defaults$gas_tax,
                              .parking_price = ghg.ccap::transportation_defaults$parking_price,
                              .freight_parking_price = ghg.ccap::transportation_defaults$freight_parking_price,
                              .cong_price = ghg.ccap::transportation_defaults$cong_price,
                              .freight_vmt_fee = ghg.ccap::transportation_defaults$freight_vmt_fee,
                              .pop_dens_pct_change = ghg.ccap::transportation_defaults$pop_dens_pct_change,
                              .emp_dens_pct_change = ghg.ccap::transportation_defaults$emp_dens_pct_change,
                              .land_use_diversity_pct_change = ghg.ccap::transportation_defaults$land_use_diversity_pct_change,
                              .intersection_design_pct_change = ghg.ccap::transportation_defaults$intersection_design_pct_change,
                              .intersection_density_pct_change = ghg.ccap::transportation_defaults$intersection_density_pct_change,
                              .job_access_pct_change = ghg.ccap::transportation_defaults$job_access_pct_change,
                              .transit_dist_pct_change = ghg.ccap::transportation_defaults$transit_dist_pct_change,
                              .comb_5d_impact_pct_change = ghg.ccap::transportation_defaults$comb_5d_impact_pct_change,
                              .telework_pct = ghg.ccap::transportation_defaults$telework_pct,
                              .enviro_factors = enviro_factors,
                              .factor_values = factor_values,
                              .elast = elast,
                              .elast_5d = elast_5d,
                              .fuel_economy = fuel_economy) {
  # cli::cli_progress_message("** calculating freight rail scneario \n")
  .freight_tb <- filter_ctu(.freight_tb, .selected_ctu)

  mode <- "FR"

  type <- "F"
  # For all freight, var = TMT
  var <- "TMT"


  # diesel -----
  # stock <- "CIStock"
  # mpg <- "CIMPG"
  # class <- "CI"
  message("Freight rail, diesel")

  fcm_ci <- calc_fuel_cost_mile(
    tb = .freight_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "CIMPG",
    .fuel_cost_gallon = .enviro_factors$CI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )


  ci_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .freight_tb,
      .mode = mode,
      .stock = "CIStock",
      .parking_cost = .parking_cost,
      .vehicle_occupancy = .vehicle_occupancy,
      .variable = var,
      .tb_fuel_cost_mile = fcm_ci,
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
      .intersection_density_pct_change = .intersection_density_pct_change,
      .job_access_pct_change = .job_access_pct_change,
      .transit_dist_pct_change = .transit_dist_pct_change,
      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
      .telework_pct = .telework_pct,
      .enviro_factors = .enviro_factors,
      .elast = .elast,
      .elast_5d = .elast_5d,
      .factor_values = .factor_values
    ) %>% mutate(class = "CI")

  ci_ghg <-
    calc_ghg_direct(
      tb_vmt = ci_vmt,
      tb = .freight_tb,
      .mode = mode,
      .fuel_type = "RCI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "CIMPG",
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values,
      .fuel_economy = .fuel_economy
    )


  # battery electric ------
  # stock <- "EVStock"
  # mpe <- "EVElec"
  # class <- "EV"
  message("Freight rail, electric")

  fcm_ev <- calc_fuel_cost_mile(
    tb = .freight_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "EVElec",
    .fuel_cost_gallon = .enviro_factors$ELEC_FUEL_COST_KWH,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values,
    .fuel_economy = .fuel_economy
  )


  ev_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .freight_tb,
      .mode = mode,
      .stock = "EVStock",
      .parking_cost = .parking_cost,
      .vehicle_occupancy = .vehicle_occupancy,
      .variable = var,
      .tb_fuel_cost_mile = fcm_ev,
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
      .intersection_density_pct_change = .intersection_density_pct_change,
      .job_access_pct_change = .job_access_pct_change,
      .transit_dist_pct_change = .transit_dist_pct_change,
      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
      .telework_pct = .telework_pct,
      .enviro_factors = .enviro_factors,
      .elast = .elast,
      .elast_5d = .elast_5d,
      .factor_values = .factor_values
    ) %>% mutate(class = "EV")


  ev_ghg <-
    calc_ghg_direct(
      tb_vmt = ev_vmt,
      tb = .freight_tb,
      .mode = mode,
      .fuel_type = .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "EVElec",
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values,
      .fuel_economy = .fuel_economy
    )

  # Finish up -----


  vmt_all <- dplyr::bind_rows(
    ci_vmt,
    ev_vmt
  )


  dir_ghg_all <- dplyr::bind_rows(
    ci_ghg,
    ev_ghg
  )


  freight_rail <- list(
    "vmt" = vmt_all,
    "dir_ghg" = dir_ghg_all
  )

  cli::cli_alert_success("Freight rail 🚆")

  return(freight_rail)
}
