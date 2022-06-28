#' @title Calculate Scenario for Passenger Rail
#' @family Passenger
#' @family Transportation
#'
#'
#' @inheritParams run_scenario_transportation
#' @inheritParams calc_vmt_forecast
#'
#' @export
#' @importFrom usethis ui_done
#' @importFrom emo ji
scen_transit_rail <- function(.pass_tb = transportation_data$passenger,
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
  # Rail Urban-----
  fcm <- calc_fuel_cost_mile(
    .pass_tb,
    .mode = "PLDV",
    .aeo_scenario,
    .miles_per_gallon = "SIMPG",
    .enviro_factors$SI_FUEL_COST_GAL
  )

  # browser()

  type <- "P"
  # For all passenger modes, variable = PMT
  var <- "PMT"

  ## EV Rail -----
  mode <- "RU"
  stock <- "EVStock"
  mpe <- "EVElec"
  class <- "EV"


  message("Passenger urban rail, electric")

  ev_vmt <-
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
      .drs_fuel_type = .drs_fuel_type,
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
      .phev_electric = .phev_electric,
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .elast_5d = .elast_5d
    ) %>%
    mutate(class = class)



  ev_ghg <-
    calc_ghg_direct(
      tb_vmt = ev_vmt,
      tb = .pass_tb,
      .mode = mode,
      .fuel_type = .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpe,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  ev_fuel <-
    calc_fuel_use(
      tb_vmt = ev_vmt,
      tb = .pass_tb,
      .mode = mode,
      # .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpe,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  ev_cost <-
    calc_cost(
      tb_vmt = ev_vmt,
      .mode = mode,
      .price = "EVPrice",
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  # Rail Interurban-----
  mode <- "RI"

  ## BCI Rail interurban -----
  stock <- "BCIStock"
  mpg <- "BCIMPG"
  class <- "BCI"

  message("Passenger interurban rail, diesel")

  ci_ri_vmt <-
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
      .drs_fuel_type = .drs_fuel_type,
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
      .phev_electric = .phev_electric,
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .elast_5d = .elast_5d
    ) %>%
    mutate(class = class)


  ci_ri_ghg <-
    calc_ghg_direct(
      tb_vmt = ci_ri_vmt,
      tb = .pass_tb,
      .mode = mode,
      .fuel_type = "BCI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  ci_ri_fuel <-
    calc_fuel_use(
      tb_vmt = ci_ri_vmt,
      tb = .pass_tb,
      .mode = mode,
      # "BCI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  ci_ri_cost <-
    calc_cost(ci_ri_vmt, mode, "BCIPrice")


  ## EV Rail Inter -----
  stock <- "EVStock"
  mpe <- "EVElec"
  class <- "EV"

  message("Passenger interurban rail, electric")
  ev_ri_vmt <-
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
      .drs_fuel_type = .drs_fuel_type,
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
      .phev_electric = .phev_electric,
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .elast_5d = .elast_5d
    ) %>%
    mutate(class = class)

  ev_ri_ghg <-
    calc_ghg_direct(
      tb_vmt = ev_ri_vmt,
      tb = .pass_tb,
      .mode = mode,
      .fuel_type = .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpe,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  ev_ri_fuel <-
    calc_fuel_use(
      tb_vmt = ev_ri_vmt,
      tb = .pass_tb,
      .mode = mode,
      # .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpe,
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )

  ev_ri_cost <-
    calc_cost(
      tb_vmt = ev_ri_vmt,
      .mode = mode,
      .price = "EVPrice",
      .is_av = FALSE,
      .enviro_factors = .enviro_factors
    )


  # Finish up -----
  # browser()
  vmt_all <- dplyr::bind_rows(
    ev_vmt,
    ev_ri_vmt,
    ci_ri_vmt
  )



  fuel_use_all <- dplyr::bind_rows(
    ev_fuel,
    ev_ri_fuel,
    ci_ri_fuel
  )

  dir_ghg_all <- dplyr::bind_rows(
    ev_ghg,
    ev_ri_ghg,
    ci_ri_ghg
  )


  emb_ghg_all <- dir_ghg_all %>%
    mutate(
      ghg_embodied_source = NA,
      ghg_embodied = NA,
      type = type
    ) %>%
    select(
      type, ghg_embodied_source, ghg_embodied,
      mode, class, ctu, year, aeo_mode
    ) %>%
    unique()

  cost_all <- dplyr::bind_rows(
    ev_cost,
    ev_ri_cost,
    ci_ri_cost
  )

  passenger_rail <- list(
    "vmt" = vmt_all,
    "dir_ghg" = dir_ghg_all,
    "emb_ghg" = emb_ghg_all,
    "fuel_use" = fuel_use_all,
    "cost" = cost_all
  )

  usethis::ui_done(paste("Urban and interurban rail", emo::ji("train")))

  return(passenger_rail)
}
