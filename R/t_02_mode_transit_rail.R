#' @title Calculate scenario for passenger rail
#' @family passenger
#' @family transportation
#'
#'
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#'
#' @export
#' @importFrom cli cli_alert_success
mode_transit_rail <- function(.pass_tb = transportation_data$passenger,
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
                              .vmt_reduction_pct = ghg.ccap::transportation_defaults$vmt_reduction_pct,
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
                              .cbtp_start_year = ghg.ccap::transportation_defaults$cbtp_start_year,
                              .cbtp_prop_targeted = ghg.ccap::transportation_defaults$cbtp_prop_targeted,
                              .ctr_employees_targeted = ghg.ccap::transportation_defaults$ctr_employees_targeted,
                              .ctr_voluntary = ghg.ccap::transportation_defaults$ctr_voluntary,
                              .ctr_start_year = ghg.ccap::transportation_defaults$ctr_start_year,
                              .commute_vmt_proportion = ghg.ccap::commute_vmt_proportion,
                              .enviro_factors = enviro_factors,
                              .elast = elast,
                              .elast_5d = elast_5d,
                              .factor_values = factor_values,
                              .fuel_economy = fuel_economy,
                              .calc_transp_cost = FALSE,
                              .calc_transp_fuel_use = FALSE,
                              .calc_transp_ghg_embodied = FALSE) {
  # cli::cli_progress_message("** calculating scenario transit rail \n")

  passenger_rail <- list()

  .pass_tb <- filter_ctu(.pass_tb, .selected_ctu)


  type <- "P"
  # For all passenger modes, variable = PMT
  var <- "PMT"

  # Rail Urban-----

  ## EV Rail -----
  # mode <- "RU"
  # stock <- "EVStock"
  # mpe <- "EVElec"
  # class <- "EV"

  # cli::cli_progress_message("**** Passenger urban rail, electric \n")

  fcm_ru_elec <- calc_fuel_cost_mile(
    .pass_tb,
    .mode = "RU",
    .aeo_scenario,
    .miles_per_gallon = "EVElec",
    .fuel_cost_gallon = .enviro_factors$ELEC_FUEL_COST_KWH,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  ru_ev_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .pass_tb,
      .mode = "RU",
      .stock = "EVStock",
      .parking_cost = .parking_cost,
      .vehicle_occupancy = .vehicle_occupancy,
      .variable = var,
      .tb_fuel_cost_mile = fcm_ru_elec,
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
      .cbtp_start_year = .cbtp_start_year,
      .cbtp_full_effect_year = .cbtp_full_effect_year,
      .cbtp_prop_targeted = .cbtp_prop_targeted,
      .ctr_employees_targeted = .ctr_employees_targeted,
      .ctr_voluntary = .ctr_voluntary,
      .ctr_start_year = .ctr_start_year,
      .ctr_full_effect_year = .ctr_full_effect_year,
      .telework_pct = .telework_pct,
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .elast_5d = .elast_5d,
      .factor_values = .factor_values
    ) %>%
    mutate(class = "EV")

  ru_ev_ghg <-
    calc_ghg_direct(
      tb_vmt = ru_ev_vmt,
      tb = .pass_tb,
      .mode = "RU",
      .fuel_type = .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "EVElec",
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values,
      .fuel_economy = .fuel_economy
    )


  # Rail Interurban-----
  # mode <- "RI"

  ## BCI Rail interurban -----
  # stock <- "BCIStock"
  # mpg <- "BCIMPG"
  # class <- "BCI"


  fcm_ri_ci <- calc_fuel_cost_mile(
    .pass_tb,
    .mode = "RI",
    .aeo_scenario,
    .miles_per_gallon = "BCIMPG",
    .fuel_cost_gallon = .enviro_factors$CI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  # cli::cli_progress_message("**** Passenger interurban rail, diesel \n")

  ci_ri_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .pass_tb,
      .mode = "RI",
      .stock = "BCIStock",
      .variable = var,
      .tb_fuel_cost_mile = fcm_ri_ci,
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
      .intersection_density_pct_change = .intersection_density_pct_change,
      .job_access_pct_change = .job_access_pct_change,
      .transit_dist_pct_change = .transit_dist_pct_change,
      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
      .telework_pct = .telework_pct,
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .elast_5d = .elast_5d,
      .factor_values = .factor_values
    ) %>%
    mutate(class = "BCI")


  ci_ri_ghg <-
    calc_ghg_direct(
      tb_vmt = ci_ri_vmt,
      tb = .pass_tb,
      .mode = "RI",
      .fuel_type = "RCI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "BCIMPG",
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values,
      .fuel_economy = .fuel_economy
    )

  ## EV Rail Inter -----
  # stock <- "EVStock"
  # mpe <- "EVElec"
  # class <- "EV"

  fcm_ri_ev <- calc_fuel_cost_mile(
    .pass_tb,
    .mode = "RI",
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "EVElec",
    .fuel_cost_gallon = .enviro_factors$ELEC_FUEL_COST_KWH,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )


  # cli::cli_progress_message("**** Passenger interurban rail, electric \n")
  ev_ri_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .pass_tb,
      .mode = "RI",
      .stock = "EVStock",
      .variable = var,
      .tb_fuel_cost_mile = fcm_ri_ev,
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
      .intersection_density_pct_change = .intersection_density_pct_change,
      .job_access_pct_change = .job_access_pct_change,
      .transit_dist_pct_change = .transit_dist_pct_change,
      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
      .telework_pct = .telework_pct,
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .elast_5d = .elast_5d,
      .factor_values = .factor_values
    ) %>%
    mutate(class = "EV")

  ev_ri_ghg <-
    calc_ghg_direct(
      tb_vmt = ev_ri_vmt,
      tb = .pass_tb,
      .mode = "RI",
      .fuel_type = .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "EVElec",
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values,
      .fuel_economy = .fuel_economy
    )

  vmt_all <- dplyr::bind_rows(
    ru_ev_vmt,
    ev_ri_vmt,
    ci_ri_vmt
  )

  dir_ghg_all <- dplyr::bind_rows(
    ru_ev_ghg,
    ev_ri_ghg,
    ci_ri_ghg
  )

  passenger_rail <- list("vmt" = vmt_all, "dir_ghg" = dir_ghg_all)


  if (.calc_transp_fuel_use == TRUE) {
    ev_fuel <-
      calc_fuel_use(
        tb_vmt = ru_ev_vmt,
        tb = .pass_tb,
        .mode = "RU",
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = "EVElec",
        .enviro_factors = .enviro_factors
      )

    ci_ri_fuel <-
      calc_fuel_use(
        tb_vmt = ci_ri_vmt,
        tb = .pass_tb,
        .mode = "RI",
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = "BCIMPG",
        .enviro_factors = .enviro_factors
      )

    ev_ri_fuel <-
      calc_fuel_use(
        tb_vmt = ev_ri_vmt,
        tb = .pass_tb,
        .mode = "RI",
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = "EVElec",
        .enviro_factors = .enviro_factors
      )

    passenger_rail$fuel_use_gallons_kwh <- dplyr::bind_rows(
      ev_fuel,
      ev_ri_fuel,
      ci_ri_fuel
    )
  }

  if (.calc_transp_cost == TRUE) {
    ru_ev_cost <-
      calc_cost(
        tb_vmt = ru_ev_vmt,
        .selected_ctu = .selected_ctu,
        .mode = "RU",
        .price = "EVPrice",
        .enviro_factors = .enviro_factors,
        .factor_values = .factor_values
      )

    ci_ri_cost <-
      calc_cost(ci_ri_vmt,
        .selected_ctu = .selected_ctu,
        .mode = "RI",
        .price = "BCIPrice",
        .enviro_factors = .enviro_factors,
        .factor_values = .factor_values
      )

    ev_ri_cost <-
      calc_cost(
        tb_vmt = ev_ri_vmt,
        .selected_ctu = .selected_ctu,
        .mode = "RI",
        .price = "EVPrice",
        .enviro_factors = .enviro_factors
      )

    passenger_rail$cost <- dplyr::bind_rows(
      ru_ev_cost,
      ev_ri_cost,
      ci_ri_cost
    )
  }

  # Finish up -----


  if (.calc_transp_ghg_embodied == TRUE) {
    emb_ghg_all <- dir_ghg_all %>%
      dplyr::mutate(
        ghg_embodied_source = NA,
        ghg_embodied = NA,
        type = type
      ) %>%
      dplyr::select(
        type, ghg_embodied_source, ghg_embodied,
        mode, class, geog_name, year, aeo_mode
      ) %>%
      dplyr::distinct()

    passenger_rail$emb_ghg <- emb_ghg_all
  }

  cli::cli_alert_success("Urban and interurban rail 🚆")

  return(passenger_rail)
}
