#' Title
#'
#' Calculate scenario for walk and bike
#'
#' @inheritParams scenario_results
#' @inheritParams calc_vmt_forecast
#'
#' @family transportation results
#' @keywords passenger
#'
#' @return
#' @export
#'
#' @importFrom emo ji
calc_walk_bike <- function(.scenario = "BAU",
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
                              .mit_bau_summary = 0) {
  fcm <- calc_fuel_cost_mile(
    transportation_data$passenger,
    .mode = "PLDV",
    .aeo_scenario,
    .miles_per_gallon = "SIMPG",
    SI_FUEL_COST_GAL
  )

  type <- "P"
  # For all passenger modes, variable = PMT
  var <- "PMT"
  mode <- "PLDV"

  # browser()
  ## Walk -----
  mode <- "WALK"
  stock <- ""
  class <- "WALK"

  walk_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$passenger, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct,
      .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>%
    mutate(class = class)

  ## Bike -----
  mode <- "BIKE"
  stock <- ""
  class <- "BIKE"

  bike_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$passenger, mode,
      stock, var, fcm, .aeo_scenario,
      .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee,
      .cong_price, .parking_price, .drs_pct,
      .av_pct, .freight_vmt_fee,
      .pop_dens_pct_change, .emp_dens_pct_change, .land_use_pct_change,
      .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>%
    mutate(class = class)



  # Finish up -----
  vmt_all <- dplyr::bind_rows(
    bike_vmt,
    walk_vmt
  )


  wb_fin <- list(
   "vmt" = vmt_all)

  usethis::ui_done(paste("Walk and bike", emo::ji("walking"), emo::ji("bike")))

  return(wb_fin)
}
