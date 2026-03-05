#' @title Calculate scenario for transit buses
#'
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#'
#' @family transportation
#' @family passenger
#'
#' @export
#'
#' @importFrom cli cli_alert_success
mode_transit_bus <- function(.pass_tb = transportation_data$passenger,
                             .selected_ctu = "all",
                             .scenario = "BAU",
                             .electric_scenario = "ER",
                             .aeo_scenario = "REF",
                             .transit_avo_pct = transportation_defaults$transit_avo_pct,
                             .parking_cost = parking_cost,
                             .vehicle_occupancy = vehicle_occupancy,
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
                             .enviro_factors = enviro_factors,
                             .elast = elast,
                             .elast_5d = elast_5d,
                             .factor_values = factor_values,
                             .fuel_economy = fuel_economy,
                             .calc_transp_cost = FALSE,
                             .calc_transp_fuel_use = FALSE,
                             .calc_transp_ghg_embodied = FALSE) {
  # cli::cli_progress_message("** calculating scenario transit bus \n")

  .pass_tb <- filter_ctu(.pass_tb, .selected_ctu)


  type <- "P"
  # For all passenger modes, variable = PMT
  var <- "PMT"
  mode <- "BU"

  ## Bus transit -----

  ### CI Bus -----

  message("Transit bus, diesel")

  # stock <- "BCIStock"
  # mpg <- "BCIMPG"
  # class <- "BCI"

  fcm_ci <- calc_fuel_cost_mile(
    tb = .pass_tb,
    .mode = "BU",
    .miles_per_gallon = "BCIMPG",
    .aeo_scenario = .aeo_scenario,
    .fuel_cost_gallon = .enviro_factors$CI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  ci_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .pass_tb,
      .mode = "BU",
      .stock = "BCIStock",
      .variable = var,
      .parking_cost = .parking_cost,
      .vehicle_occupancy = .vehicle_occupancy,
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

  ci_dir_ghg <-
    calc_ghg_direct(
      tb_vmt = ci_vmt,
      tb = .pass_tb,
      .mode = "BU",
      .fuel_type = "BCI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "BCIMPG",
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values,
      .fuel_economy = .fuel_economy
    )

  bus_scenario <- list("vmt" = ci_vmt, "dir_ghg" = ci_dir_ghg)

  if (.calc_transp_cost == TRUE) {
    ci_fuel <-
      calc_fuel_use(
        tb_vmt = ci_vmt,
        tb = .pass_tb,
        .mode = mode,
        # "CI",
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = "BCIMPG",
        .enviro_factors = .enviro_factors
      )

    bus_scenario$fuel_use_gallons_kwh <- ci_fuel
  }

  if (.calc_transp_ghg_embodied == TRUE) {
    ci_emb_ghg <-
      calc_ghg_embodied(
        tb = .pass_tb,
        .mode = "BU",
        .sales_mode = "BCISales",
        .fuel_type = "BU-BCI-EMB",
        .class = "BCI",
        .transit_avo_pct = .transit_avo_pct,
        .enviro_factors = .enviro_factors
      )

    bus_scenario$emb_ghg <- ci_emb_ghg
  }

  if (.calc_transp_cost == TRUE) {
    ci_cost <-
      calc_cost(
        tb_vmt = ci_vmt,
        .selected_ctu = .selected_ctu,
        .mode = "BU",
        .price = "BCIPrice",
        .enviro_factors = .enviro_factors
      )

    bus_scenario$cost <- ci_cost
  }

  ### HEV Bus ------
  # stock <- "HEVStock"
  # mpg <- "HEVMPG"
  # class <- "HEV"
  # message("Transit bus, hybrid")
  #
  # hev_vmt <-
  #   calc_vmt_forecast(
  #     .scenario = .scenario,
  #     tb = .pass_tb,
  #     .mode = mode,
  #     .stock = stock,
  #     .variable = var,
  #     .tb_fuel_cost_mile = fcm,
  #     .aeo_scenario = .aeo_scenario,
  #     .transit_avo_pct = .transit_avo_pct,
  #     .transit_service_pct = .transit_service_pct,
  #     .vmt_fee = .vmt_fee,
  #     .payd_fee = .payd_fee,
  #     .gas_tax = .gas_tax,
  #     .cong_price = .cong_price,
  #     .parking_price = .parking_price,
  #     .freight_vmt_fee = .freight_vmt_fee,
  #     .pop_dens_pct_change = .pop_dens_pct_change,
  #     .emp_dens_pct_change = .emp_dens_pct_change,
  #     .land_use_diversity_pct_change = .land_use_diversity_pct_change,
  #     .intersection_design_pct_change = .intersection_design_pct_change,
  #     .job_access_pct_change = .job_access_pct_change,
  #     .transit_dist_pct_change = .transit_dist_pct_change,
  #     .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
  #     .telework_pct = .telework_pct,
  #     .elast = .elast,
  #     .enviro_factors = .enviro_factors,
  #     .elast_5d = .elast_5d
  #   ) %>%
  #   mutate(class = class)
  #
  #
  # hev_dir_ghg <- calc_ghg_direct(
  #   tb_vmt = hev_vmt,
  #   tb = .pass_tb,
  #   .mode = mode,
  #   .fuel_type = "CI",
  #   .aeo_scenario = .aeo_scenario,
  #   .miles_per_gallon = mpg,
  #   .enviro_factors = .enviro_factors
  # )
  #
  #
  # hev_fuel <-
  #   calc_fuel_use(
  #     tb_vmt = hev_vmt,
  #     tb = .pass_tb,
  #     .mode = mode,
  #     # "CI",
  #     .aeo_scenario = .aeo_scenario,
  #     .miles_per_gallon = mpg,
  #     .enviro_factors = .enviro_factors
  #   )
  #
  #
  # hev_emb_ghg <-
  #   calc_ghg_embodied(
  #     tb = .pass_tb,
  #     .mode = mode,
  #     .sales_mode = "HEVSales",
  #     .fuel_type = "BU-HEV-EMB",
  #     .class = class,
  #     .transit_avo_pct = .transit_avo_pct,
  #     hev_vmt,
  #     .enviro_factors = .enviro_factors
  #   )
  #
  #
  # hev_cost <-
  #   calc_cost(hev_vmt, mode, "HEVPrice")
  #
  #
  # ### BEV Bus -----
  # stock <- "BEVStock"
  # mpe <- "BEVElec"
  # class <- "BEV"
  # message("Transit bus, battery electric")
  #
  #
  # bev_vmt <-
  #   calc_vmt_forecast(
  #     .scenario = .scenario,
  #     tb = .pass_tb,
  #     .mode = mode,
  #     .stock = stock,
  #     .variable = var,
  #     .tb_fuel_cost_mile = fcm,
  #     .aeo_scenario = .aeo_scenario,
  #     .transit_avo_pct = .transit_avo_pct,
  #     .transit_service_pct = .transit_service_pct,
  #     .vmt_fee = .vmt_fee,
  #     .payd_fee = .payd_fee,
  #     .gas_tax = .gas_tax,
  #     .cong_price = .cong_price,
  #     .parking_price = .parking_price,
  #     .freight_parking_price = .freight_parking_price,
  #     .freight_vmt_fee = .freight_vmt_fee,
  #     .pop_dens_pct_change = .pop_dens_pct_change,
  #     .emp_dens_pct_change = .emp_dens_pct_change,
  #     .land_use_diversity_pct_change = .land_use_diversity_pct_change,
  #     .intersection_design_pct_change = .intersection_design_pct_change,
  #     .job_access_pct_change = .job_access_pct_change,
  #     .transit_dist_pct_change = .transit_dist_pct_change,
  #     .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
  #     .telework_pct = .telework_pct,
  #     .elast = .elast,
  #     .enviro_factors = .enviro_factors,
  #     .elast_5d = .elast_5d
  #   ) %>%
  #   mutate(class = class)
  #
  #
  # bev_dir_ghg <-
  #   calc_ghg_direct(
  #     tb_vmt = bev_vmt,
  #     tb = .pass_tb,
  #     .mode = mode,
  #     .fuel_type = .electric_scenario,
  #     .aeo_scenario = .aeo_scenario,
  #     .miles_per_gallon = mpe,
  #     .enviro_factors = .enviro_factors
  #   )
  #
  # bev_fuel <-
  #   calc_fuel_use(
  #     tb_vmt = bev_vmt,
  #     tb = .pass_tb,
  #     .mode = mode,
  #     # .electric_scenario,
  #     .aeo_scenario = .aeo_scenario,
  #     .miles_per_gallon = mpe,
  #     .enviro_factors = .enviro_factors
  #   )
  #
  # bev_emb_ghg <-
  #   calc_ghg_embodied(
  #     tb = .pass_tb,
  #     .mode = mode,
  #     .sales_mode = "BEVSales",
  #     .fuel_type = "BU-BEV-EMB",
  #     .class = class,
  #     .transit_avo_pct = .transit_avo_pct,
  #     bev_vmt,
  #     .mit_bau_summary,
  #     .enviro_factors = .enviro_factors
  #   )
  #
  # bev_cost <-
  #   calc_cost(
  #     tb_vmt = bev_vmt,
  #     .mode = mode,
  #     .price = "BEVPrice",
  #     .enviro_factors = .enviro_factors
  #   )


  # Bus Rapid Transit----


  # ### CI BRT -----
  # mode <- "BRT"
  # stock <- "BCIStock"
  # mpg <- "BCIMPG"
  # class <- "BCI"
  # message("Bus rapid transit, diesel")
  #
  # ci_brt_vmt <-
  #   calc_vmt_forecast(
  #     .scenario = .scenario,
  #     tb = .pass_tb,
  #     .mode = mode,
  #     .stock = stock,
  #     .variable = var,
  #     .tb_fuel_cost_mile = fcm,
  #     .aeo_scenario = .aeo_scenario,
  #     .transit_avo_pct = .transit_avo_pct,
  #     .transit_service_pct = .transit_service_pct,
  #     .vmt_fee = .vmt_fee,
  #     .payd_fee = .payd_fee,
  #     .gas_tax = .gas_tax,
  #     .cong_price = .cong_price,
  #     .parking_price = .parking_price,
  #     .freight_parking_price = .freight_parking_price,
  #     .freight_vmt_fee = .freight_vmt_fee,
  #     .pop_dens_pct_change = .pop_dens_pct_change,
  #     .emp_dens_pct_change = .emp_dens_pct_change,
  #     .land_use_diversity_pct_change = .land_use_diversity_pct_change,
  #     .intersection_design_pct_change = .intersection_design_pct_change,
  #     .job_access_pct_change = .job_access_pct_change,
  #     .transit_dist_pct_change = .transit_dist_pct_change,
  #     .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
  #     .telework_pct = .telework_pct,
  #     .elast = .elast,
  #     .enviro_factors = .enviro_factors,
  #     .elast_5d = .elast_5d
  #   ) %>%
  #   mutate(class = class)
  #
  # ci_brt_ghg <-
  #   calc_ghg_direct(
  #     tb_vmt = ci_brt_vmt,
  #     tb = .pass_tb,
  #     .mode = mode,
  #     .fuel_type = "BCI",
  #     .aeo_scenario = .aeo_scenario,
  #     .miles_per_gallon = mpg,
  #     .enviro_factors = .enviro_factors
  #   )
  #
  # ci_brt_fuel <-
  #   calc_fuel_use(
  #     tb_vmt = ci_brt_vmt,
  #     tb = .pass_tb,
  #     .mode = mode,
  #     # "BCI",
  #     .aeo_scenario = .aeo_scenario,
  #     .miles_per_gallon = mpg,
  #     .enviro_factors = .enviro_factors
  #   )
  #
  # ci_brt_emb_ghg <-
  #   calc_ghg_embodied(
  #     tb = .pass_tb,
  #     .mode = mode,
  #     .sales_mode = "BCISales",
  #     .fuel_type = "BU-BCI-EMB",
  #     .class = class,
  #     .transit_avo_pct = .transit_avo_pct
  #   )
  #
  # ci_brt_cost <-
  #   calc_cost(
  #     tb_vmt = ci_brt_vmt,
  #     .mode = mode,
  #     .price = "BCIPrice"
  #   )
  #
  # #
  # ### HEV BRT -----
  # stock <- "HEVStock"
  # mpg <- "HEVMPG"
  # class <- "HEV"
  #
  # message("Bus rapid transit, hybrid")
  #
  # hev_brt_vmt <-
  #   calc_vmt_forecast(
  #     .scenario = .scenario,
  #     tb = .pass_tb,
  #     .mode = mode,
  #     .stock = stock,
  #     .variable = var,
  #     .tb_fuel_cost_mile = fcm,
  #     .aeo_scenario = .aeo_scenario,
  #     .transit_avo_pct = .transit_avo_pct,
  #     .transit_service_pct = .transit_service_pct,
  #     .vmt_fee = .vmt_fee,
  #     .payd_fee = .payd_fee,
  #     .gas_tax = .gas_tax,
  #     .cong_price = .cong_price,
  #     .parking_price = .parking_price,
  #     .freight_parking_price = .freight_parking_price,
  #     .freight_vmt_fee = .freight_vmt_fee,
  #     .pop_dens_pct_change = .pop_dens_pct_change,
  #     .emp_dens_pct_change = .emp_dens_pct_change,
  #     .land_use_diversity_pct_change = .land_use_diversity_pct_change,
  #     .intersection_design_pct_change = .intersection_design_pct_change,
  #     .job_access_pct_change = .job_access_pct_change,
  #     .transit_dist_pct_change = .transit_dist_pct_change,
  #     .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
  #     .telework_pct = .telework_pct,
  #     .elast = .elast,
  #     .enviro_factors = .enviro_factors,
  #     .elast_5d = .elast_5d
  #   ) %>%
  #   mutate(class = class)
  #
  #
  # hev_brt_ghg <-
  #   calc_ghg_direct(
  #     tb_vmt = hev_brt_vmt,
  #     tb = .pass_tb,
  #     .mode = mode,
  #     .fuel_type = "HEV",
  #     .aeo_scenario = .aeo_scenario,
  #     .miles_per_gallon = mpg
  #   )
  #
  # hev_brt_fuel <-
  #   calc_fuel_use(
  #     tb_vmt = hev_brt_vmt,
  #     tb = .pass_tb,
  #     .mode = mode,
  #     # .fuel_type = "HEV",
  #     .aeo_scenario = .aeo_scenario,
  #     .miles_per_gallon = mpg,
  #     .enviro_factors = .enviro_factors
  #   )
  #
  # hev_brt_emb_ghg <-
  #   calc_ghg_embodied(
  #     tb =  .pass_tb,
  #     .mode =  mode,
  #     .sales_mode =  "HEVSales",
  #     .fuel_type = "BU-HEV-EMB",
  #     .class = class,
  #     .transit_avo_pct, hev_brt_vmt,
  #     .enviro_factors = .enviro_factors
  #   )
  #
  # hev_brt_cost <-
  #   calc_cost(
  #     tb_vmt = hev_brt_vmt,
  #     .mode = mode,
  #     .price = "HEVPrice",
  #     .enviro_factors = .enviro_factors
  #   )
  #
  # ### BEV BRT -----
  # stock <- "BEVStock"
  # mpe <- "BEVElec"
  # class <- "BEV"
  # message("Bus rapid transit, battery")
  #
  # bev_brt_vmt <- calc_vmt_forecast(
  #   .scenario = .scenario,
  #   tb = .pass_tb,
  #   .mode = mode,
  #   .stock = stock,
  #   .variable = var,
  #   .tb_fuel_cost_mile = fcm,
  #   .aeo_scenario = .aeo_scenario,
  #   .transit_avo_pct = .transit_avo_pct,
  #   .transit_service_pct = .transit_service_pct,
  #   .vmt_fee = .vmt_fee,
  #   .payd_fee = .payd_fee,
  #   .gas_tax = .gas_tax,
  #   .cong_price = .cong_price,
  #   .parking_price = .parking_price,
  #   .freight_parking_price = .freight_parking_price,
  #   .freight_vmt_fee = .freight_vmt_fee,
  #   .pop_dens_pct_change = .pop_dens_pct_change,
  #   .emp_dens_pct_change = .emp_dens_pct_change,
  #   .land_use_diversity_pct_change = .land_use_diversity_pct_change,
  #   .intersection_design_pct_change = .intersection_design_pct_change,
  #   .job_access_pct_change = .job_access_pct_change,
  #   .transit_dist_pct_change = .transit_dist_pct_change,
  #   .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
  #   .telework_pct = .telework_pct,
  #   .elast = .elast,
  #   .enviro_factors = .enviro_factors,
  #   .elast_5d = .elast_5d
  # ) %>%
  #   mutate(class = class)
  #
  #
  # bev_brt_ghg <-
  #   calc_ghg_direct(
  #     tb_vmt =  bev_brt_vmt,
  #     tb = .pass_tb,
  #     .mode =   mode,
  #     .electric_scenario,
  #     .aeo_scenario = .aeo_scenario,
  #     .miles_per_gallon = mpe,
  #     .enviro_factors = .enviro_factors
  #   )
  #
  # bev_brt_fuel <-
  #   calc_fuel_use(
  #     tb_vmt = bev_brt_vmt,
  #     tb = .pass_tb,
  #     .mode = mode,
  #     # .fuel_type = .electric_scenario,
  #     .aeo_scenario = .aeo_scenario,
  #     .miles_per_gallon = mpe,
  #     .enviro_factors = .enviro_factors
  #   )
  #
  # bev_brt_emb_ghg <-
  #   calc_ghg_embodied(
  #     tb = .pass_tb,
  #     .mode = mode,
  #     .sales_mode = "BEVSales",
  #     .fuel_type = "BU-BEV-EMB",
  #     .class = class,
  #     .transit_avo_pct = .transit_avo_pct,
  #     .enviro_factors = .enviro_factors
  #   )
  #
  # bev_brt_cost <-
  #   calc_cost(
  #     tb_vmt = bev_brt_vmt,
  #     .mode = mode,
  #     .price = "BEVPrice",
  #     .enviro_factors = .enviro_factors
  #   )
  #

  # Finish up -----

  cli::cli_alert_success("Transit buses and bus rapid transit 🚌")

  return(bus_scenario)
}
