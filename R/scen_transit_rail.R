#' Calculate scenario for passenger rail
#'
#' @inheritParams scenario_results
#' @inheritParams calc_vmt_forecast
#'
#' @family transportation results, passenger
#'
#' @return
#' @export
#' @importFrom usethis ui_done
#' @importFrom emo ji
scen_transit_rail <- function(.pass_tb = transportation_data$passenger,
                              .scenario = "BAU",
                              .electric_scenario = "ER",
                              .aeo_scenario = "REF",
                              .transit_avo = 0,
                              .transit_rider_pct = 0,
                              .vmt_fee = 0,
                              .payd_fee = 0,
                              .gas_tax = 0,
                              .parking_price = 0,
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
                              .enviro_factors = enviro_factors) {
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
      .scenario,
      tb = .pass_tb,
      mode,
      stock, var, fcm, .aeo_scenario,
      .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee,
      .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_diversity_pct_change, .intersection_design_pct_change,
      .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>%
    mutate(class = class)



  ev_ghg <-
    calc_ghg_direct(
      ev_vmt,
      .pass_tb,
      mode,
      .electric_scenario,
      .aeo_scenario,
      mpe
    )

  ev_fuel <-
    calc_fuel_use(
      ev_vmt,
      .pass_tb,
      mode,
      .electric_scenario,
      .aeo_scenario,
      mpe
    )

  ev_cost <-
    calc_cost(ev_vmt, mode, "EVPrice")

  # Rail Interurban-----
  mode <- "RI"

  ## BCI Rail interurban -----
  stock <- "BCIStock"
  mpg <- "BCIMPG"
  class <- "BCI"

  message("Passenger interurban rail, diesel")

  ci_ri_vmt <-
    calc_vmt_forecast(
      .scenario, .pass_tb,
      mode, stock, var, fcm,
      .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price,
      .parking_price, .drs_pct, .av_pct, .freight_vmt_fee,
      .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_diversity_pct_change,
      .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>%
    mutate(class = class)


  ci_ri_ghg <-
    calc_ghg_direct(
      ci_ri_vmt,
      .pass_tb, mode, "BCI", .aeo_scenario, mpg
    )

  ci_ri_fuel <-
    calc_fuel_use(
      ci_ri_vmt, .pass_tb,
      mode, "BCI", .aeo_scenario, mpg
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
      .scenario, .pass_tb, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change, .land_use_diversity_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>%
    mutate(class = class)

  ev_ri_ghg <-
    calc_ghg_direct(
      ev_ri_vmt, .pass_tb,
      mode, .electric_scenario,
      .aeo_scenario, mpe
    )

  ev_ri_fuel <-
    calc_fuel_use(
      ev_ri_vmt, .pass_tb,
      mode, .electric_scenario,
      .aeo_scenario, mpe
    )

  ev_ri_cost <-
    calc_cost(ev_ri_vmt, mode, "EVPrice")


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
