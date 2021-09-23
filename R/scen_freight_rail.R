#'
#' Calculate scenario for freight rail
#'
#' @inheritParams scenario_results
#' @inheritParams calc_vmt_forecast
#'
#' @family transportation results, freight
#'
#' @return
#' @export
#'
#' @importFrom emo ji
scen_freight_rail <- function(.scenario = "BAU",
                              .electric_scenario = "ER",
                              .aeo_scenario = "REF",
                              .ctu = "",
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
                              .land_use_pct_change = 0,
                              .intersection_design_pct_change = 0,
                              .job_access_pct_change = 0,
                              .transit_dist_pct_change = 0,
                              .comb_5d_impact_pct_change = 0,
                              .telework_pct = 0,
                              .mit_bau_summary = 0,
                              .enviro_factors = enviro_factors) {
  mode <- "FR"

  type <- "F"
  # For all freight, var = TMT
  var <- "TMT"

  # browser()
  # diesel -----
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  message("Freight rail, diesel")

  # browser()
  ci_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$freight,
      mode, stock, var, fcm,
      .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price,
      .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change,
      .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>% mutate(class = class)

  ci_ghg <-
    calc_ghg_direct(
      ci_vmt,
      transportation_data$freight,
      mode, "RCI", .aeo_scenario, mpg
    )


  # battery electric ------
  stock <- "EVStock"
  mpe <- "EVElec"
  class <- "EV"
  message("Freight rail, electric")

  ev_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$freight,
      mode, stock, var, fcm,
      .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price,
      .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change,
      .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change
    ) %>% mutate(class = class)


  ev_ghg <-
    calc_ghg_direct(
      ev_vmt,
      transportation_data$freight, mode,
      .electric_scenario, .aeo_scenario, mpe
    )

  # Finish up -----


  vmt_all <- dplyr::bind_rows(
    ci_vmt,
    ev_vmt
  )


  dir_ghg_all <- dplyr::bind_rows(
    ci_ghg,
    ev_ghg
  )


  freight_rail <- list(
    "vmt" = vmt_all,
    "dir_ghg" = dir_ghg_all
  )

  usethis::ui_done(paste("Freight rail", emo::ji("train")))

  return(freight_rail)
}
