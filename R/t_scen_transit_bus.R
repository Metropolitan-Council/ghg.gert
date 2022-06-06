#' @title Calculate Scenario for Transit Buses
#' @family Transit
#' @family Transportation
#'
#' @inheritParams run_scenario_transportation
#' @inheritParams calc_vmt_forecast
#'
#'
#' @return
#' @export
#'
#' @importFrom emo ji
#' @importFrom usethis ui_done
scen_transit_bus <- function(.pass_tb = transportation_data$passenger,
                             .scenario = "BAU",
                             .electric_scenario = "ER",
                             .aeo_scenario = "REF",
                             .transit_avo_pct = 0,
                             .transit_rider_pct = 0,
                             .vmt_fee = 0,
                             .payd_fee = 0,
                             .gas_tax = 0,
                             .parking_price = 0,
                             .freight_parking_price = 0,
                             .cong_price = 0,
                             .freight_vmt_fee = 0,
                             .drs_pct = 0,
                             .av_pct = 0,
                             .drs_fuel_type = "",
                             .av_fuel_type = "",
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
                             .elast_5d = elast_5d) {
  # browser()
  type <- "P"
  # For all passenger modes, variable = PMT
  var <- "PMT"
  mode <- "BU"

  ## Bus transit -----

  ### CI Bus -----

  message("Transit bus, diesel")

  stock <- "BCIStock"
  mpg <- "BCIMPG"
  class <- "BCI"

  fcm <- calc_fuel_cost_mile(
    tb = .pass_tb,
    .mode = "PLDV",
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "SIMPG",
    .fuel_cost_gallon = .enviro_factors$SI_FUEL_COST_GAL,
    .av_pct = .av_pct,
    .enviro_factors = .enviro_factors
  )

  ci_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      tb = .pass_tb,
      .mode = mode,
      .stock = stock,
      .variable = var,
      .tb_fuel_cost_mile = fcm,
      .aeo_scenario = .aeo_scenario,
      .transit_avo_pct = .transit_avo_pct,
      .transit_rider_pct = .transit_rider_pct,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .cong_price = .cong_price,
      .parking_price = .parking_price,
      .freight_parking_price = .freight_parking_price,
      .drs_pct = .drs_pct,
      .av_pct = .av_pct,
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
    mutate(class = class)

  ci_dir_ghg <-
    calc_ghg_direct(
      tb_vmt = ci_vmt,
      tb = .pass_tb,
      .mode = mode,
      .fuel_type = "CI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  ci_fuel <-
    calc_fuel_use(
      tb_vmt = ci_vmt,
      tb = .pass_tb,
      .mode = mode,
      # "CI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  ci_emb_ghg <-
    calc_ghg_embodied(
      tb = .pass_tb,
      .mode =   mode,
      .sales_mode =  "BCISales",
      .fuel_type = "BU-BCI-EMB",
      .class = class,
      .transit_avo_pct = .transit_avo_pct,
      .mit_bau_summary,
      .enviro_factors = .enviro_factors
    )

  ci_cost <-
    calc_cost(
      tb_vmt = ci_vmt,
      .mode = mode,
      .price = "BCIPrice",
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  ### HEV Bus ------
  stock <- "HEVStock"
  mpg <- "HEVMPG"
  class <- "HEV"
  message("Transit bus, hybrid")

  hev_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      tb = .pass_tb,
      .mode = mode,
      .stock = stock,
      .variable = var,
      .tb_fuel_cost_mile = fcm,
      .aeo_scenario = .aeo_scenario,
      .transit_avo_pct = .transit_avo_pct,
      .transit_rider_pct = .transit_rider_pct,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .cong_price = .cong_price,
      .parking_price = .parking_price,
      .drs_pct = .drs_pct,
      .av_pct = .av_pct,
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
    mutate(class = class)


  hev_dir_ghg <- calc_ghg_direct(
    tb_vmt = hev_vmt,
    tb = .pass_tb,
    .mode = mode,
    .fuel_type = "CI",
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = mpg,
    .is_av = FALSE,
    .enviro_factors = .enviro_factors
  )


  hev_fuel <-
    calc_fuel_use(
      tb_vmt = hev_vmt,
      tb = .pass_tb,
      .mode = mode,
      # "CI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )


  hev_emb_ghg <-
    calc_ghg_embodied(
      tb = .pass_tb,
      .mode = mode,
      .sales_mode = "HEVSales",
      .fuel_type = "BU-HEV-EMB",
      .class = class,
      .transit_avo_pct = .transit_avo_pct,
      hev_vmt,
      .mit_bau_summary,
      .enviro_factors = .enviro_factors
    )


  hev_cost <-
    calc_cost(hev_vmt, mode, "HEVPrice")


  ### BEV Bus -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"
  message("Transit bus, battery electric")


  bev_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      tb = .pass_tb,
      .mode = mode,
      .stock = stock,
      .variable = var,
      .tb_fuel_cost_mile = fcm,
      .aeo_scenario = .aeo_scenario,
      .transit_avo_pct = .transit_avo_pct,
      .transit_rider_pct = .transit_rider_pct,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .cong_price = .cong_price,
      .parking_price = .parking_price,
      .freight_parking_price = .freight_parking_price,
      .drs_pct = .drs_pct,
      .av_pct = .av_pct,
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
    mutate(class = class)


  bev_dir_ghg <-
    calc_ghg_direct(
      tb_vmt = bev_vmt,
      tb = .pass_tb,
      .mode = mode,
      .fuel_type = .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpe,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  bev_fuel <-
    calc_fuel_use(
      tb_vmt = bev_vmt,
      tb = .pass_tb,
      .mode = mode,
      # .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpe,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  bev_emb_ghg <-
    calc_ghg_embodied(
      tb = .pass_tb,
      .mode = mode,
      .sales_mode = "BEVSales",
      .fuel_type = "BU-BEV-EMB",
      .class = class,
      .transit_avo_pct = .transit_avo_pct,
      bev_vmt,
      .mit_bau_summary,
      .enviro_factors = .enviro_factors
    )

  bev_cost <-
    calc_cost(
      tb_vmt = bev_vmt,
      .mode = mode,
      .price = "BEVPrice",
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )


  # Bus Rapid Transit----

  # browser()

  ### CI BRT -----
  mode <- "BRT"
  stock <- "BCIStock"
  mpg <- "BCIMPG"
  class <- "BCI"
  message("Bus rapid transit, diesel")

  ci_brt_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      tb = .pass_tb,
      .mode = mode,
      .stock = stock,
      .variable = var,
      .tb_fuel_cost_mile = fcm,
      .aeo_scenario = .aeo_scenario,
      .transit_avo_pct = .transit_avo_pct,
      .transit_rider_pct = .transit_rider_pct,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .cong_price = .cong_price,
      .parking_price = .parking_price,
      .freight_parking_price = .freight_parking_price,
      .drs_pct = .drs_pct,
      .av_pct = .av_pct,
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
    mutate(class = class)

  ci_brt_ghg <-
    calc_ghg_direct(
      tb_vmt = ci_brt_vmt,
      tb = .pass_tb,
      .mode = mode,
      .fuel_type = "BCI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  ci_brt_fuel <-
    calc_fuel_use(
      tb_vmt = ci_brt_vmt,
      tb = .pass_tb,
      .mode = mode,
      # "BCI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  ci_brt_emb_ghg <-
    calc_ghg_embodied(
      tb = .pass_tb,
      .mode = mode,
      .sales_mode = "BCISales",
      .fuel_type = "BU-BCI-EMB",
      .class = class,
      .transit_avo_pct = .transit_avo_pct
    )

  ci_brt_cost <-
    calc_cost(
      tb_vmt = ci_brt_vmt,
      .mode = mode,
      .price = "BCIPrice",
      .is_av = FALSE
    )


  ### HEV BRT -----
  stock <- "HEVStock"
  mpg <- "HEVMPG"
  class <- "HEV"

  message("Bus rapid transit, hybrid")

  hev_brt_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      tb = .pass_tb,
      .mode = mode,
      .stock = stock,
      .variable = var,
      .tb_fuel_cost_mile = fcm,
      .aeo_scenario = .aeo_scenario,
      .transit_avo_pct = .transit_avo_pct,
      .transit_rider_pct = .transit_rider_pct,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .cong_price = .cong_price,
      .parking_price = .parking_price,
      .freight_parking_price = .freight_parking_price,
      .drs_pct = .drs_pct,
      .av_pct = .av_pct,
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
    mutate(class = class)


  hev_brt_ghg <-
    calc_ghg_direct(
      tb_vmt = hev_brt_vmt,
      tb = .pass_tb,
      .mode = mode,
      .fuel_type = "HEV",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg
    )

  hev_brt_fuel <-
    calc_fuel_use(
      tb_vmt = hev_brt_vmt,
      tb = .pass_tb,
      .mode = mode,
      # .fuel_type = "HEV",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  hev_brt_emb_ghg <-
    calc_ghg_embodied(
      tb =  .pass_tb,
      .mode =  mode,
      .sales_mode =  "HEVSales",
      .fuel_type = "BU-HEV-EMB",
      .class = class,
      .transit_avo_pct, hev_brt_vmt,
      .mit_bau_summary,
      .enviro_factors = .enviro_factors
    )

  hev_brt_cost <-
    calc_cost(
      tb_vmt = hev_brt_vmt,
      .mode = mode,
      .price = "HEVPrice",
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  ### BEV BRT -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"
  message("Bus rapid transit, battery")

  bev_brt_vmt <- calc_vmt_forecast(
    .scenario = .scenario,
    tb = .pass_tb,
    .mode = mode,
    .stock = stock,
    .variable = var,
    .tb_fuel_cost_mile = fcm,
    .aeo_scenario = .aeo_scenario,
    .transit_avo_pct = .transit_avo_pct,
    .transit_rider_pct = .transit_rider_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .cong_price = .cong_price,
    .parking_price = .parking_price,
    .freight_parking_price = .freight_parking_price,
    .drs_pct = .drs_pct,
    .av_pct = .av_pct,
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
    mutate(class = class)


  bev_brt_ghg <-
    calc_ghg_direct(
      tb_vmt =  bev_brt_vmt,
      tb = .pass_tb,
      .mode =   mode,
      .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpe,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  bev_brt_fuel <-
    calc_fuel_use(
      tb_vmt = bev_brt_vmt,
      tb = .pass_tb,
      .mode = mode,
      # .fuel_type = .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpe,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  bev_brt_emb_ghg <-
    calc_ghg_embodied(
      tb = .pass_tb,
      .mode = mode,
      .sales_mode = "BEVSales",
      .fuel_type = "BU-BEV-EMB",
      .class = class,
      .transit_avo_pct = .transit_avo_pct,
      .mit_bau_summary,
      .enviro_factors = .enviro_factors
    )

  bev_brt_cost <-
    calc_cost(
      tb_vmt = bev_brt_vmt,
      .mode = mode,
      .price = "BEVPrice",
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  # Finish up -----

  # browser()
  fuel_use_all <- dplyr::bind_rows(
    ci_fuel,
    hev_fuel,
    bev_fuel,
    ci_brt_fuel,
    hev_brt_fuel,
    bev_brt_fuel
  )


  vmt_all <- dplyr::bind_rows(
    ci_vmt,
    hev_vmt,
    bev_vmt,
    ci_brt_vmt,
    hev_brt_vmt,
    bev_brt_vmt
  )

  emb_ghg_all <- dplyr::bind_rows(
    ci_emb_ghg,
    hev_emb_ghg,
    bev_emb_ghg,
    ci_brt_emb_ghg,
    hev_brt_emb_ghg,
    bev_brt_emb_ghg
  )

  dir_ghg_all <- dplyr::bind_rows(
    ci_dir_ghg,
    hev_dir_ghg,
    bev_dir_ghg,
    ci_brt_ghg,
    hev_brt_ghg,
    bev_brt_ghg
  )

  cost_all <- dplyr::bind_rows(
    ci_cost,
    hev_cost,
    bev_cost,
    ci_brt_cost,
    hev_brt_cost,
    bev_brt_cost
  )

  bus_scenario <- list(
    "vmt" = vmt_all,
    "dir_ghg" = dir_ghg_all,
    "emb_ghg" = emb_ghg_all,
    "fuel_use" = fuel_use_all,
    "cost" = cost_all
  )

  usethis::ui_done(paste("Transit buses and bus rapid transit", emo::ji("bus")))

  return(bus_scenario)
}
