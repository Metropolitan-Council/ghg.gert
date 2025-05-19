#' @title Calculate scenario for passenger light-duty vehicles
#' @family passenger
#' @family transportation
#'
#' @inheritParams run_scenario_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#' @export
#'
#' @importFrom emo ji
#' @importFrom cli cli_alert_success
mode_passenger_light_duty <- function(.pass_tb,
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
                                      .factor_values = ghg.ccap::factor_values,
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



  ## Gasoline (SI) -----
  # Calculate aggregate GHG in kt CO2 by year
  # 1. Calculate fuel cost per mile (FCM)
  # 2. Calculate VMT (requires FCM, )
  # 3. Calculate direct emissions (requires VMT)
  # 4. Calculate fuel use (requires VMT)
  # 5. Calculate embodied GHG

  # stock <- "SIStock"
  # mpg <- "SIMPG"
  # class <- "SI"

  # browser()
  message("Passenger vehicles, gasoline")

  # Calculate a fuel cost per mile rather than per gallon
  si_fcm <- calc_fuel_cost_mile(
    tb = .pass_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "SIMPG",
    .fuel_cost_gallon = .enviro_factors$SI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  si_vmt <- calc_vmt_forecast(
    .scenario = .scenario,
    .selected_ctu = .selected_ctu,
    tb = .pass_tb,
    .mode = mode,
    .stock = "SIStock",
    .variable = var,
    .tb_fuel_cost_mile = si_fcm,
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
    .phev_electric = NA,
    .elast = .elast,
    .enviro_factors = .enviro_factors,
    .elast_5d = .elast_5d,
    .factor_values = .factor_values
  ) %>%
    dplyr::mutate(class = "SI")

  si_dir_ghg <- calc_ghg_direct(
    tb_vmt = si_vmt,
    tb = .pass_tb,
    .mode = mode,
    .fuel_type = "SI",
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "SIMPG",
    .grid_decarbonization_pct = .grid_decarbonization_pct,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  # complete gasoline table

  ## Diesel  (CI)----
  # stock <- "CIStock"
  # mpg <- "CIMPG"
  # class <- "CI"

  message("Passenger vehicles, diesel")
  # Calculate a fuel cost per mile rather than per gallon
  ci_fcm <- calc_fuel_cost_mile(
    tb = .pass_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "CIMPG",
    .fuel_cost_gallon = .enviro_factors$CI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  ci_vmt <- calc_vmt_forecast(
    .scenario = .scenario,
    .selected_ctu = .selected_ctu,
    tb = .pass_tb,
    .mode = mode,
    .stock = "CIStock",
    .variable = var,
    .tb_fuel_cost_mile = ci_fcm,
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
    dplyr::mutate(class = "CI")



  ci_dir_ghg <-
    calc_ghg_direct(
      tb_vmt = ci_vmt,
      tb = .pass_tb,
      .mode = mode,
      .fuel_type = "CI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "CIMPG",
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values
    )


  ## HEV (Hybrid electric vehicle)----
  # stock <- "HEVStock"
  # mpg <- "HEVMPG"
  # class <- "HEV"

  message("Passenger vehicles, hybrid")

  # Calculate a fuel cost per mile rather than per gallon

  fcm_hev <- calc_fuel_cost_mile(
    tb = .pass_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "HEVMPG",
    .fuel_cost_gallon = .enviro_factors$SI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  hev_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .pass_tb,
      .mode = mode,
      .stock = "HEVStock",
      .variable = var,
      .tb_fuel_cost_mile = fcm_hev,
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
    dplyr::mutate(class = "HEV")

  hev_dir_ghg <-
    calc_ghg_direct(
      tb_vmt = hev_vmt,
      tb = .pass_tb,
      .mode = mode,
      .fuel_type = "SI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "HEVMPG",
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values
    )


  ## PHEV (Plug-in hybrid) -----
  ### Eqn: (PMT in 1000 mi) x Pr(stock by fuel) x ((Pr(Elec) x
  ### elec consumption (MWh per 1000 mi) x GHG per elec) +
  ###  ((1 - Pr(Elec)) x fuel consumption (per 1000 mi) x GHG per fuel)

  # stock <- "PHEVStock"
  # mpg <- "PHEVMPG"
  # mpe <- "PHEVElec"
  # class <- "PHEV"

  message("Passenger vehicles, plug-in hybrid")

  # Calculate a fuel cost per mile rather than per gallon
  # browser()

  fcm_phev <- calc_fuel_cost_mile(
    tb = .pass_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "PHEVMPG",
    .fuel_cost_gallon = .enviro_factors$SI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  ### VMT gas ----
  phev_vmt_gas <- calc_vmt_forecast(
    .scenario = .scenario,
    .selected_ctu = .selected_ctu,
    tb = .pass_tb,
    .phev_electric = FALSE,
    .mode = mode,
    .stock = "PHEVStock",
    .variable = var,
    .tb_fuel_cost_mile = fcm_phev,
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
    dplyr::mutate(class = "PHEV")


  ### VMT electric ------
  fcm_electric <- calc_fuel_cost_mile(
    tb = .pass_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "PHEVElec",
    .fuel_cost_gallon = .enviro_factors$ELEC_FUEL_COST_KWH,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  phev_vmt_electric <- calc_vmt_forecast(
    .scenario = .scenario,
    tb = .pass_tb,
    .selected_ctu = .selected_ctu,
    .phev_electric = TRUE,
    .mode = mode,
    .stock = "PHEVStock",
    .variable = var,
    .tb_fuel_cost_mile = fcm_electric,
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
    dplyr::mutate(class = "PHEV")

  ### PHEV VMT all -----
  phev_vmt <- dplyr::left_join(
    phev_vmt_electric %>%
      dplyr::select(everything(),
                    vmt_electric = vmt
      ),
    phev_vmt_gas %>%
      dplyr::select(everything(),
                    vmt_gas = vmt
      ),
    c(
      "type",
      "stock",
      "class",
      "scenario",
      "mode",
      "ctu",
      "year",
      "aeo_mode"
    )
  ) %>%
    rowwise() %>%
    dplyr::mutate(
      vmt = vmt_electric + vmt_gas,
      class = "PHEV"
    ) %>%
    dplyr::select(-vmt_electric, -vmt_gas)

  ### gas ghg direct -----
  phev_ghg_gas <- calc_ghg_direct(
    tb_vmt = phev_vmt_gas,
    tb = .pass_tb,
    .mode = mode,
    .fuel_type = "SI",
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "PHEVMPG",
    .grid_decarbonization_pct = .grid_decarbonization_pct,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  ) %>%
    dplyr::select(everything(),
                  dir_ghg_gas = dir_ghg
    )

  ### electric ghg direct -----
  phev_ghg_electric <- calc_ghg_direct(
    tb_vmt = phev_vmt_electric,
    tb = .pass_tb,
    .mode = mode,
    .fuel_type = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "PHEVElec",
    .grid_decarbonization_pct = .grid_decarbonization_pct,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  ) %>%
    dplyr::select(everything(),
                  dir_ghg_electric = dir_ghg
    )

  phev_dir_ghg <- dplyr::left_join(
    phev_ghg_gas,
    phev_ghg_electric,
    c(
      "type", "scenario", "mode", "ctu",
      "year", "aeo_mode", "class"
    )
  ) %>%
    dplyr::mutate(dir_ghg = dir_ghg_electric + dir_ghg_gas) %>%
    dplyr::select(-dir_ghg_electric, -dir_ghg_gas)

  ## BEV (Battery electric vehicle) -----
  # stock <- "BEVStock"
  # mpe <- "BEVElec"
  # class <- "BEV"

  message("Passenger vehicles, battery electric")

  # browser()
  fcm_bev <- calc_fuel_cost_mile(
    tb = .pass_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "BEVElec",
    .fuel_cost_gallon = .enviro_factors$ELEC_FUEL_COST_KWH,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  bev_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .pass_tb,
      .mode = mode,
      .stock = "BEVStock",
      .variable = var,
      .tb_fuel_cost_mile = fcm_bev,
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
      .factor_values = .factor_values,
      .elast_5d = .elast_5d
    ) %>%
    dplyr::mutate(class ="BEV")

  bev_dir_ghg <-
    calc_ghg_direct(
      tb_vmt = bev_vmt,
      tb = .pass_tb,
      .mode = mode,
      .fuel_type = .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "BEVElec",
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values
    )


  vmt_all <- dplyr::bind_rows(
    ci_vmt,
    si_vmt,
    hev_vmt,
    phev_vmt,
    bev_vmt
  )


  dir_ghg_all <- dplyr::bind_rows(
    ci_dir_ghg,
    si_dir_ghg,
    hev_dir_ghg,
    phev_dir_ghg,
    bev_dir_ghg
  )


  pldv_scenario <- list(
    "dir_ghg" = dir_ghg_all,
    "vmt" = vmt_all
  )


  # if calculate fuel use -----
  if (.calc_transp_fuel_use == TRUE) {
    si_fuel <- calc_fuel_use(
      tb_vmt = si_vmt,
      tb = .pass_tb,
      .mode = mode,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "SIMPG"
    )

    ci_fuel <-
      calc_fuel_use(
        tb_vmt = ci_vmt,
        tb = .pass_tb,
        .mode = mode,
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = "CIMPG"
      )

    phev_fuel_electric <-
      calc_fuel_use(
        tb_vmt = phev_vmt_electric,
        tb = .pass_tb,
        .mode = mode,
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = "PHEVElec"
      )

    phev_fuel_gas <- calc_fuel_use(
      tb_vmt = phev_vmt_gas,
      tb = .pass_tb,
      .mode = mode,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "PHEVMPG",
      .enviro_factors = .enviro_factors
    )

    phev_fuel <- dplyr::left_join(
      phev_fuel_electric %>%
        dplyr::select(everything(),
                      fuel_use_electric = fuel_use_gallons_kwh
        ),
      phev_fuel_gas %>%
        dplyr::select(everything(),
                      fuel_use_gas = fuel_use_gallons_kwh
        ),
      c(
        "type",
        "scenario", "mode", "ctu", "year",
        "aeo_mode"
      )
    ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(fuel_use_gallons_kwh = fuel_use_gas + fuel_use_electric) %>%
      dplyr::select(-fuel_use_gas, -fuel_use_electric)

    hev_fuel <-
      calc_fuel_use(
        tb_vmt = hev_vmt,
        tb = .pass_tb,
        .mode = mode,
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = "HEVMPG"
      )

    bev_fuel <-
      calc_fuel_use(
        tb_vmt = bev_vmt,
        tb = .pass_tb,
        .mode = mode,
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = "BEVElec"
      )

    pldv_scenario$fuel_use_gallons_kwh <- dplyr::bind_rows(
      ci_fuel,
      si_fuel,
      hev_fuel,
      phev_fuel,
      bev_fuel
    )
  }

  # if calculate cost ----
  if (.calc_transp_cost == TRUE) {
    si_cost <-
      calc_cost(
        .selected_ctu = .selected_ctu,
        tb_vmt = si_vmt,
        .mode = mode,
        .price = "SIPrice",
        .enviro_factors = .enviro_factors,
        .factor_values = .factor_values
      )

    ci_cost <-
      calc_cost(
        tb_vmt = ci_vmt,
        .selected_ctu = .selected_ctu,
        .mode = mode,
        .price = "CIPrice",
        .enviro_factors = .enviro_factors,
        .factor_values = .factor_values
      )

    hev_cost <-
      calc_cost(hev_vmt, mode,
                .selected_ctu = .selected_ctu,
                .price = "HEVPrice",
                .enviro_factors = .enviro_factors,
                .factor_values = .factor_values
      )

    phev_cost <-
      calc_cost(phev_vmt,
                .selected_ctu = .selected_ctu,
                .mode = mode,
                .price = "PHEVPrice",
                .enviro_factors = .enviro_factors,
                .factor_values = .factor_values
      )

    bev_cost <-
      calc_cost(
        tb_vmt = bev_vmt,
        .selected_ctu = .selected_ctu,
        .mode = mode,
        .price = "BEVPrice",
        .enviro_factors = .enviro_factors,
        .factor_values = .factor_values      )

    pldv_scenario$cost <- dplyr::bind_rows(
      si_cost,
      ci_cost,
      hev_cost,
      phev_cost,
      bev_cost
    )
  }

  # if calculate embodied -----

  if (.calc_transp_ghg_embodied == TRUE) {
    si_emb_ghg <-
      calc_ghg_embodied(.pass_tb, mode,
                        .class = "SI",
                        "SISales",
                        "SI-EMB"
      )

    ci_emb_ghg <-
      calc_ghg_embodied(
        tb = .pass_tb,
        .mode = mode,
        .sales_mode = "CISales",
        .fuel_type = "CI-EMB",
        .class = "CI"
      )

    hev_emb_ghg <-
      calc_ghg_embodied(
        tb = .pass_tb,
        .class = "HEV",
        .mode = mode,
        .sales_mode = "HEVSales",
        .fuel_type = "HEV-EMB",
        .enviro_factors = .enviro_factors
      )

    phev_emb_ghg <-
      calc_ghg_embodied(
        tb = .pass_tb,
        .mode = mode,
        .class = "PHEV",
        .sales_mode = "PHEVSales",
        .fuel_type = "PHEV-EMB",
        .enviro_factors = .enviro_factors,
        .transit_avo_pct = .transit_avo_pct
      )

    bev_emb_ghg <-
      calc_ghg_embodied(
        .pass_tb,
        .mode = mode,
        .class = "BEV",
        .sales_mode = "BEVSales",
        .fuel_type = "BEV-EMB",
        .enviro_factors = .enviro_factors
      )

    pldv_scenario$emb_ghg <- dplyr::bind_rows(
      ci_emb_ghg,
      si_emb_ghg,
      hev_emb_ghg,
      phev_emb_ghg,
      bev_emb_ghg
    )
  }

  cli::cli_alert_success(paste(
    "Passenger light-duty vehicles",
    emo::ji("automobile")
  ))

  # return ------
  return(pldv_scenario)
}
