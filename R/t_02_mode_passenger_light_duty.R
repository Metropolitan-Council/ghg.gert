#' @title Calculate scenario for passenger light-duty vehicles
#' @family passenger
#' @family transportation
#'
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#' @export
#'
#' @importFrom cli cli_alert_success
#' @importFrom purrr pmap
#' @importFrom stringr str_to_lower
mode_passenger_light_duty <- function(.pass_tb,
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
                                      .vmt_reduction_pct = 0,
                                      .freight_vmt_fee = 0,
                                      .pop_dens_pct_change = 0,
                                      .emp_dens_pct_change = 0,
                                      .land_use_diversity_pct_change = 0,
                                      .intersection_design_pct_change = 0,
                                      .job_access_pct_change = 0,
                                      .transit_dist_pct_change = 0,
                                      .comb_5d_impact_pct_change = 0,
                                      .telework_pct = 0,
                                      .enviro_factors = ghg.ccap::enviro_factors,
                                      .elast = ghg.ccap::elast,
                                      .elast_5d = ghg.ccap::elast_5d,
                                      .factor_values = ghg.ccap::factor_values,
                                      .fuel_economy = ghg.ccap::fuel_economy,
                                      .calc_transp_cost = FALSE,
                                      .calc_transp_fuel_use = FALSE,
                                      .calc_transp_ghg_embodied = FALSE) {
  # cli::cli_progress_message("** calculating scenario for passenger light duty vehicles \n")
  .pass_tb <- filter_ctu(.pass_tb, .selected_ctu)

  # Sequence for each
  # 1. Establish `type`, `var`, `mode`
  # 2. Establish `stock`, `mpg`, `class`
  # 3. Calculate fuel cost per mile with `calc_fuel_cost_mile()`
  # 4. Calculate VMT with `calc`

  # Passenger ------------------------------------------------------------

  type <- "P"
  # For all passenger modes, variable = PMT
  var <- "PMT"
  # PLDV by fuel and CTU
  mode <- "PLDV"

  # browser()

  # establish commonly passed parameters
  fcm_common <- list(
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values,
    .fuel_economy = .fuel_economy
  )

  vmt_common <- list(
    tb = .pass_tb,
    .mode = mode,
    .variable = var,
    .scenario = .scenario,
    .selected_ctu = .selected_ctu,
    .parking_cost = .parking_cost,
    .vehicle_occupancy = .vehicle_occupancy,
    .aeo_scenario = .aeo_scenario,
    .transit_avo_pct = .transit_avo_pct,
    .pldv_avo_pct = .pldv_avo_pct,
    .transit_service_pct = .transit_service_pct,
    .vmt_fee = .vmt_fee,
    .vmt_reduction_pct = .vmt_reduction_pct,
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
  )

  dir_ghg_common <- list(
    tb = .pass_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values,
    .fuel_economy = .fuel_economy
  )

  # create parameter table
  vehicle_params <- tibble::tribble(
    ~fuel_label, ~mpg_name, ~stock_name, ~fuel_label2, ~fuel_cost_var,
    "SI", "SIMPG", "SIStock", "SI", .enviro_factors$SI_FUEL_COST_GAL,
    "CI", "CIMPG", "CIStock", "CI", .enviro_factors$CI_FUEL_COST_GAL,
    "HEV", "HEVMPG", "HEVStock", "SI", .enviro_factors$SI_FUEL_COST_GAL,
    "BEV", "BEVElec", "BEVStock", .electric_scenario, .enviro_factors$ELEC_FUEL_COST_KWH
  )

  # run for each vehicle fuel type
  results_list <- purrr::pmap(vehicle_params, run_vehicle_calculations, fcm_common, vmt_common, dir_ghg_common)

  # move results to environment---
  purrr::map(
    results_list,
    list2env,
    envir = environment()
  )

  # compile -----
  vmt_all <- dplyr::bind_rows(
    ci_vmt,
    si_vmt,
    hev_vmt,
    bev_vmt
  )


  dir_ghg_all <- dplyr::bind_rows(
    ci_dir_ghg,
    si_dir_ghg,
    hev_dir_ghg,
    bev_dir_ghg
  )

  # do the total number of VMT match up?

  # browser()
  # .pass_tb %>%
  #   filter(mode == "PLDV",
  #          var == "PMT") %>%
  #   mutate(value = value * 1.6) %>%
  #   left_join(
  #
  #     vmt_all %>%
  #       filter(mode == "PLDV") %>%
  #       group_by(mode, geog_name, geog_id, year, aeo_mode, type) %>%
  #       summarize(vmt = sum(vmt))) %>%
  #   mutate(diff = value - vmt)


  pldv_scenario <- list(
    "dir_ghg" = dir_ghg_all,
    "vmt" = vmt_all
  )




  # if calculate fuel use -----
  if (.calc_transp_fuel_use) {
    fuel_params <- list(
      tb_vmt            = list(si_vmt, ci_vmt, hev_vmt, bev_vmt),
      .miles_per_gallon = c("SIMPG", "CIMPG", "HEVMPG", "BEVElec")
    )

    pldv_scenario$fuel_use_gallons_kwh <- purrr::pmap(fuel_params, function(tb_vmt, .miles_per_gallon) {
      calc_fuel_use(
        tb_vmt            = tb_vmt,
        tb                = .pass_tb,
        .mode             = mode,
        .aeo_scenario     = .aeo_scenario,
        .miles_per_gallon = .miles_per_gallon
      )
    }) %>%
      dplyr::bind_rows()
  }


  # if calculate cost ----
  if (.calc_transp_cost) {
    cost_params <- list(
      tb_vmt = list(si_vmt, ci_vmt, hev_vmt, bev_vmt),
      .price = c("SIPrice", "CIPrice", "HEVPrice", "BEVPrice")
    )

    pldv_scenario$cost <- purrr::pmap(cost_params, function(tb_vmt, .price) {
      calc_cost(
        tb_vmt          = tb_vmt,
        .price          = .price,
        .mode           = mode,
        .selected_ctu   = .selected_ctu,
        .enviro_factors = .enviro_factors,
        .factor_values  = .factor_values
      )
    }) %>%
      dplyr::bind_rows()
  }
  # if calculate embodied -----

  if (.calc_transp_ghg_embodied) {
    emb_ghg_params <- list(
      .class      = c("SI", "CI", "HEV", "BEV"),
      .sales_mode = c("SISales", "CISales", "HEVSales", "BEVSales"),
      .fuel_type  = c("SI-EMB", "CI-EMB", "HEV-EMB", "BEV-EMB")
    )

    pldv_scenario$emb_ghg <- purrr::pmap(emb_ghg_params, function(.class, .sales_mode, .fuel_type) {
      calc_ghg_embodied(
        tb               = .pass_tb,
        .mode            = mode,
        .class           = .class,
        .sales_mode      = .sales_mode,
        .fuel_type       = .fuel_type,
        .enviro_factors  = .enviro_factors,
        .transit_avo_pct = .transit_avo_pct
      )
    }) %>%
      dplyr::bind_rows()
  }

  cli::cli_alert_success(
    "Passenger light-duty vehicles 🚗"
  )

  # return ------
  return(pldv_scenario)
}



#' @keywords internal
#' @importFrom stringr str_to_lower
run_vehicle_calculations <- function(
    fuel_label,
    mpg_name,
    stock_name,
    fuel_label2,
    fuel_cost_var,
    fcm_common,
    vmt_common,
    dir_ghg_common) {
  cli::cli_alert_info("Passenger vehicles, {fuel_label}")

  fcm <- do.call(
    calc_fuel_cost_mile,
    c(.miles_per_gallon = mpg_name, .fuel_cost_gallon = fuel_cost_var, fcm_common)
  )

  vmt <- do.call(
    calc_vmt_forecast,
    c(.stock = stock_name, append(vmt_common, list(.tb_fuel_cost_mile = fcm)))
  ) %>%
    dplyr::mutate(class = fuel_label)

  dir_ghg <- do.call(
    calc_ghg_direct,
    c(.fuel_type = fuel_label2, .miles_per_gallon = mpg_name, append(dir_ghg_common, list(tb_vmt = vmt)))
  )

  fuel_label_lower <- tolower(fuel_label)

  list(fcm, vmt, dir_ghg) %>%
    setNames(paste0(fuel_label_lower, c("_fcm", "_vmt", "_dir_ghg"))) %>%
    return()
}
