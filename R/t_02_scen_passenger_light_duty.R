#' @title Calculate scenario for passenger light-duty vehicles
#' @family passenger
#' @family transportation
#'
#' @inheritParams run_scenario_transportation
#' @inheritParams calc_vmt_forecast
#'
#' @export
#'
#' @importFrom emo ji
#' @importFrom usethis ui_done
scen_passenger_light_duty <- function(.pass_tb = transportation_data$passenger,
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
                                      .mit_bau_summary = 0,
                                      .enviro_factors = enviro_factors,
                                      .elast = elast,
                                      .elast_5d = elast_5d,
                                      .calc_transp_cost = FALSE,
                                      .calc_transp_fuel_use = FALSE,
                                      .calc_transp_ghg_embodied = FALSE) {
  #cli::cli_progress_message("** calculating scenario for passenger light duty vehicles \n")

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



  ## Gasoline -----
  # Calculate aggregate GHG in kt CO2 by year
  # 1. Calculate fuel cost per mile (FCM)
  # 2. Calculate VMT (requires FCM, )
  # 3. Calculate direct emissions (requires VMT)
  # 4. Calculate fuel use (requires VMT)
  # 5. Calculate embodied GHG

  stock <- "SIStock"
  mpg <- "SIMPG"
  class <- "SI"

  # browser()
  message("Passenger vehicles, gasoline")

  # Calculate a fuel cost per mile rather than per gallon
  si_fcm <- calc_fuel_cost_mile(
    tb = .pass_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "SIMPG",
    .fuel_cost_gallon = .enviro_factors$SI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors
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
    .elast_5d = .elast_5d
  ) %>%
    dplyr::mutate(class = class)

  si_dir_ghg <- calc_ghg_direct(
    tb_vmt = si_vmt,
    tb = .pass_tb,
    .mode = mode,
    .fuel_type = "SI",
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = mpg
  )

  # complete gasoline table

  ## Diesel----
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"

  message("Passenger vehicles, diesel")
  # Calculate a fuel cost per mile rather than per gallon
  ci_fcm <- calc_fuel_cost_mile(
    tb = .pass_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "CIMPG",
    .fuel_cost_gallon = .enviro_factors$CI_FUEL_COST_GAL
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
    .elast_5d = .elast_5d
  ) %>%
    dplyr::mutate(class = class)



  ci_dir_ghg <-
    calc_ghg_direct(
      tb_vmt = ci_vmt,
      tb = .pass_tb,
      .mode = mode,
      .fuel_type = "CI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "CIMPG"
    )


  ## HEV (Hybrid electric vehicle)----
  stock <- "HEVStock"
  mpg <- "HEVMPG"
  class <- "HEV"

  message("Passenger vehicles, hybrid")

  # Calculate a fuel cost per mile rather than per gallon

  fcm <- calc_fuel_cost_mile(.pass_tb,
                             mode,
                             .aeo_scenario,
                             mpg,
                             .enviro_factors$SI_FUEL_COST_GAL)

  hev_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .pass_tb,
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
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .elast_5d = .elast_5d
    ) %>%
    dplyr::mutate(class = class)

  hev_dir_ghg <-
    calc_ghg_direct(hev_vmt, .pass_tb,
                    mode, "SI", .aeo_scenario, mpg)


  ## PHEV (Plug-in hybrid) -----
  ### Eqn: (PMT in 1000 mi) x Pr(stock by fuel) x ((Pr(Elec) x elec consumption (MWh per 1000 mi) x GHG per elec) + ((1 - Pr(Elec)) x fuel consumption (per 1000 mi) x GHG per fuel)

  stock <- "PHEVStock"
  mpg <- "PHEVMPG"
  mpe <- "PHEVElec"
  class <- "PHEV"

  message("Passenger vehicles, plug-in hybrid")

  # Calculate a fuel cost per mile rather than per gallon
  # browser()

  fcm <- calc_fuel_cost_mile(.pass_tb,
                             mode,
                             .aeo_scenario,
                             mpg,
                             .enviro_factors$SI_FUEL_COST_GAL)

  ### VMT gas ----
  phev_vmt_gas <- calc_vmt_forecast(
    .scenario = .scenario,
    .selected_ctu = .selected_ctu,
    tb = .pass_tb,
    .phev_electric = FALSE,
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
    .elast = .elast,
    .enviro_factors = .enviro_factors,
    .elast_5d = .elast_5d
  ) %>%
    dplyr::mutate(class = class)


  ### VMT electric ------
  fcm_electric <- calc_fuel_cost_mile(.pass_tb,
                                      mode,
                                      .aeo_scenario,
                                      mpe,
                                      .enviro_factors$ELEC_FUEL_COST_KWH)

  phev_vmt_electric <- calc_vmt_forecast(
    .scenario = .scenario,
    tb = .pass_tb,
    .selected_ctu = .selected_ctu,
    .phev_electric = TRUE,
    .mode = mode,
    .stock = stock,
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
    .elast_5d = .elast_5d
  ) %>%
    dplyr::mutate(class = class)

  ### VMT all -----
  phev_vmt <- left_join(
    phev_vmt_electric %>%
      select(everything(),
             vmt_electric = vmt),
    phev_vmt_gas %>%
      select(everything(),
             vmt_gas = vmt),
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
    dplyr::mutate(vmt = vmt_electric + vmt_gas,
           class = class) %>%
    select(-vmt_electric,-vmt_gas)

  ### gas ghg direct -----
  phev_ghg_gas <- calc_ghg_direct(
    tb_vmt = phev_vmt_gas,
    tb = .pass_tb,
    .mode = mode,
    .fuel_type = "SI",
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = mpg,
    .is_av = FALSE,
    .enviro_factors = .enviro_factors
  ) %>%
    select(everything(),
           dir_ghg_gas = dir_ghg)

  ### electric ghg direct -----
  phev_ghg_electric <- calc_ghg_direct(
    tb_vmt = phev_vmt_electric,
    tb = .pass_tb,
    .mode = mode,
    .fuel_type = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = mpe
  ) %>%
    select(everything(),
           dir_ghg_electric = dir_ghg)

  phev_dir_ghg <- left_join(
    phev_ghg_gas,
    phev_ghg_electric,
    c("type", "scenario", "mode", "ctu",
      "year", "aeo_mode", "class")
  ) %>%
    dplyr::mutate(dir_ghg = dir_ghg_electric + dir_ghg_gas) %>%
    select(-dir_ghg_electric,-dir_ghg_gas)

  ## BEV (Battery electric vehicle) -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"

  message("Passenger vehicles, battery electric")

  fcm <- calc_fuel_cost_mile(.pass_tb,
                             mode,
                             .aeo_scenario,
                             mpe,
                             .enviro_factors$ELEC_FUEL_COST_KWH)

  bev_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .pass_tb,
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
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .elast_5d = .elast_5d
    ) %>%
    dplyr::mutate(class = class)

  bev_dir_ghg <-
    calc_ghg_direct(bev_vmt, .pass_tb,
                    mode, .electric_scenario, .aeo_scenario, mpe)


  vmt_all <- dplyr::bind_rows(ci_vmt,
                              si_vmt,
                              hev_vmt,
                              phev_vmt,
                              bev_vmt)


  dir_ghg_all <- dplyr::bind_rows(ci_dir_ghg,
                                  si_dir_ghg,
                                  hev_dir_ghg,
                                  phev_dir_ghg,
                                  bev_dir_ghg)


  pldv_scenario <- list("dir_ghg" = dir_ghg_all,
                        "vmt" = vmt_all)


  if (.calc_transp_fuel_use == TRUE) {
    si_fuel <- calc_fuel_use(
      tb_vmt = si_vmt,
      tb = .pass_tb,
      .mode = mode,
      # .fuel_type = "SI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg
    )

    ci_fuel <-
      calc_fuel_use(
        tb_vmt = ci_vmt,
        tb = .pass_tb,
        .mode = mode,
        # .fuel_type = "CI",
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = "CIMPG"
      )

    phev_fuel_electric <-
      calc_fuel_use(
        tb_vmt = phev_vmt_electric,
        tb = .pass_tb,
        .mode = mode,
        # .electric_scenario,
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = mpe
      )

    hev_fuel <-
      calc_fuel_use(
        tb_vmt = hev_vmt,
        tb = .pass_tb,
        .mode = mode,
        # .fuel_type = "SI",
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = mpg
        # .is_av = .is_av
      )

    phev_fuel <- left_join(
      phev_fuel_electric %>%
        select(everything(),
               fuel_use_electric = fuel_use),
      phev_fuel_gas %>%
        select(everything(),
               fuel_use_gas = fuel_use),
      c("type",
        "scenario", "mode", "ctu", "year",
        "aeo_mode")
    ) %>%
      rowwise() %>%
      dplyr::mutate(fuel_use = fuel_use_gas + fuel_use_electric) %>%
      select(-fuel_use_gas,-fuel_use_electric)

    phev_fuel_gas <- calc_fuel_use(
      tb_vmt = phev_vmt_gas,
      tb = .pass_tb,
      .mode = mode,
      # .fuel_type = "SI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

    bev_fuel <-
      calc_fuel_use(bev_vmt, .pass_tb,
                    mode,
                    # .electric_scenario,
                    .aeo_scenario, mpe)

    pldv_scenario$fuel_use <- dplyr::bind_rows(ci_fuel,
                                               si_fuel,
                                               hev_fuel,
                                               phev_fuel,
                                               bev_fuel)

  }


  if (.calc_transp_cost == TRUE) {
    si_cost <-
      calc_cost(.selected_ctu = .selected_ctu,
                si_vmt, mode,
                "SIPrice")

    ci_cost <-
      calc_cost(
        tb_vmt = ci_vmt,
        .selected_ctu = .selected_ctu,
        .mode = mode,
        .price = "CIPrice"
      )

    hev_cost <-
      calc_cost(hev_vmt, mode,
                .selected_ctu = .selected_ctu,
                "HEVPrice")

    phev_cost <-
      calc_cost(phev_vmt,
                .selected_ctu = .selected_ctu,
                mode,
                "PHEVPrice")

    bev_cost <-
      calc_cost(
        tb_vmt = bev_vmt,
        .selected_ctu = .selected_ctu,
        .mode = mode,
        .price = "BEVPrice",
        .is_av = FALSE,
        .enviro_factors = .enviro_factors
      )

    pldv_scenario$cost <- dplyr::bind_rows(si_cost,
                                           ci_cost,
                                           hev_cost,
                                           phev_cost,
                                           bev_cost)

  }

  if (.calc_transp_ghg_embodied == TRUE) {

    si_emb_ghg <-
      calc_ghg_embodied(.pass_tb, mode,
                        .class = class,
                        "SISales",
                        "SI-EMB")

    ci_emb_ghg <-
      calc_ghg_embodied(
        tb = .pass_tb,
        .mode = mode,
        .sales_mode = "CISales",
        .fuel_type = "CI-EMB",
        .class = class
      )

    hev_emb_ghg <-
      calc_ghg_embodied(
        tb = .pass_tb,
        .class = class,
        .mode = mode,
        .sales_mode = "HEVSales",
        .fuel_type = "HEV-EMB",
        .enviro_factors = .enviro_factors
      )

    phev_emb_ghg <-
      calc_ghg_embodied(
        tb = .pass_tb,
        .mode = mode,
        .class = class,
        .sales_mode = "PHEVSales",
        .fuel_type = "PHEV-EMB",
        .enviro_factors = .enviro_factors,
        .transit_avo_pct = .transit_avo_pct
      )

    bev_emb_ghg <-
      calc_ghg_embodied(
        .pass_tb,
        .mode = mode,
        .class = class,
        .sales_mode = "BEVSales",
        .fuel_type = "BEV-EMB",
        .enviro_factors = .enviro_factors
      )

    pldv_scenario$emb_ghg  <- dplyr::bind_rows(ci_emb_ghg,
                                    si_emb_ghg,
                                    hev_emb_ghg,
                                    phev_emb_ghg,
                                    bev_emb_ghg)

  }

  usethis::ui_done(paste("Passenger light-duty vehicles",
                         emo::ji("automobile")))

  return(pldv_scenario)
}
