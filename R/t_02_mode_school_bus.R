#' @title Calculate scenario for school buses
#' @family passenger
#' @family transportation
#'
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#' @return [tibble::tibble()] with column names...
#' @export
#' @importFrom cli cli_alert_success
mode_school_bus <- function(.pass_tb = transportation_data$passenger,
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
  # cli::cli_progress_message("** calculating scenario school bus \n")
  .pass_tb <- filter_ctu(.pass_tb, .selected_ctu = .selected_ctu)


  school_bus <- list()

  type <- "P"
  # For all passenger modes, variable = PMT
  var <- "PMT"


  # School Bus-----
  mode <- "BS"

  # VMT Calculation

  ## CI School bus -----
  # stock <- "CIStock"
  # mpg <- "CIMPG"
  # class <- "CI"
  message("School bus, diesel")

  fcm_ci <- calc_fuel_cost_mile(
    tb = .pass_tb,
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
      tb = .pass_tb,
      .mode = mode,
      .stock = "CIStock",
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
      .job_access_pct_change = .job_access_pct_change,
      .transit_dist_pct_change = .transit_dist_pct_change,
      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
      .telework_pct = .telework_pct,
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .elast_5d = .elast_5d,
      .factor_values = .factor_values,
    ) %>%
    mutate(class = "CI")

  ## BEV school bus -----
  # stock <- "BEVStock"
  # mpe <- "BEVElec"
  # class <- "BEV"

  fcm_ev <- calc_fuel_cost_mile(
    tb = .pass_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "BEVElec",
    .fuel_cost_gallon = .enviro_factors$ELEC_FUEL_COST_KWH,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  message("School bus, electric")
  bev_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .pass_tb,
      .mode = mode,
      .stock = "BEVStock",
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
      .job_access_pct_change = .job_access_pct_change,
      .transit_dist_pct_change = .transit_dist_pct_change,
      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
      .telework_pct = .telework_pct,
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .elast_5d = .elast_5d,
      .factor_values = .factor_values
    ) %>%
    mutate(class = "BEV")

  # GHG Calculation
  ci_ghg <-
    calc_ghg_direct(
      tb_vmt = ci_vmt,
      tb = .pass_tb,
      .mode = mode,
      .fuel_type = "CI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "CIMPG",
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values,
      .fuel_economy = .fuel_economy
    )

  bev_ghg <-
    calc_ghg_direct(
      tb_vmt = bev_vmt,
      tb = .pass_tb,
      .mode = mode,
      .fuel_type = .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "BEVElec",
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values,
      .fuel_economy = .fuel_economy
    )

  vmt_all <- dplyr::bind_rows(
    ci_vmt,
    bev_vmt
  )

  dir_ghg_all <- dplyr::bind_rows(
    ci_ghg,
    bev_ghg
  )

  school_bus <- list("vmt" = vmt_all, "dir_ghg" = dir_ghg_all)



  if (.calc_transp_fuel_use == TRUE) {
    ci_fuel <-
      calc_fuel_use(
        tb_vmt = ci_vmt,
        tb = .pass_tb,
        .mode = mode,
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = "CIMPG"
      )

    bev_fuel <-
      calc_fuel_use(
        tb_vmt = bev_vmt,
        tb = .pass_tb,
        .mode = mode,
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = "BEVElec"
      )

    school_bus$fuel_use_gallons_kwh <- dplyr::bind_rows(
      ci_fuel,
      bev_fuel
    )
  }

  if (.calc_transp_cost == TRUE) {
    ci_cost <- calc_cost(ci_vmt, .selected_ctu,
      .mode = mode,
      .price = "CIPrice",
      .factor_values = .factor_values,
      .enviro_factors = .enviro_factors
    )

    bev_cost <- calc_cost(bev_vmt, .selected_ctu, mode, "BEVPrice",
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values
    )

    school_bus$cost <- dplyr::bind_rows(
      ci_cost,
      bev_cost
    )
  }

  # Finish up -----

  if (.calc_transp_ghg_embodied == TRUE) {
    emb_ghg_all <- dir_ghg_all %>%
      dplyr::mutate(
        ghg_embodied_source = NA,
        type = type
      ) %>%
      dplyr::select(type, scenario, mode, geog_name, year, aeo_mode,
        ghg_embodied_source,
        ghg_embodied = dir_ghg
      )

    school_bus$emb_ghg <- emb_ghg_all
  }

  cli::cli_alert_success(paste("School bus", "🏫"))

  return(school_bus)
}
