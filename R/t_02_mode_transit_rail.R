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
#' @importFrom emo ji
mode_transit_rail <- function(.pass_tb = transportation_data$passenger,
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
    .enviro_factors$ELEC_FUEL_COST_KWH,
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
      .job_access_pct_change = .job_access_pct_change,
      .transit_dist_pct_change = .transit_dist_pct_change,
      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
      .telework_pct = .telework_pct,
      .phev_electric = .phev_electric,
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
      .grid_decarbonization_pct = .grid_decarbonization_pct,
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
      .grid_decarbonization_pct = .grid_decarbonization_pct,
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
      .grid_decarbonization_pct = .grid_decarbonization_pct,
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
  # browser()

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
      unique()

    passenger_rail$emb_ghg <- emb_ghg_all
  }

  cli::cli_alert_success(paste("Urban and interurban rail", emo::ji("train")))

  return(passenger_rail)
}
