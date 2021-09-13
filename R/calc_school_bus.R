#' Calculate scenario for school buses
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
calc_school_bus <- function(.scenario = "BAU",
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


  # School Bus-----
  mode <- "BS"

  ## CI School bus -----
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"

  message("School bus, diesel")

  ci_vmt <-
    calc_vmt_forecast(
      .scenario,
      tb = transportation_data$passenger,
      mode, stock, var, fcm,
      .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price,
      .parking_price, .drs_pct, .av_pct, .freight_vmt_fee,
      .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change
    ) %>%
    mutate(class = class)


  ci_ghg <-
    calc_ghg_direct(
      ci_vmt, transportation_data$passenger,
      mode, "CI", .aeo_scenario, mpg
    )

  ci_fuel <-
    calc_fuel_use(
      ci_vmt, transportation_data$passenger, mode,
      "CI", .aeo_scenario, mpg
    )

  ci_cost <-
    calc_cost(
      ci_vmt,
      mode, "CIPrice"
    )


  ## BEV school bus -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"

  message("School bus, electric")
  bev_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$passenger, mode, stock, var, fcm,
      .aeo_scenario, .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee, .gas_tax, .cong_price,
      .parking_price, .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change
    ) %>%
    mutate(class = class)

  bev_ghg <-
    calc_ghg_direct(
      bev_vmt,
      transportation_data$passenger, mode, .electric_scenario,
      .aeo_scenario, mpe
    )

  bev_fuel <-
    calc_fuel_use(
      bev_vmt, transportation_data$passenger, mode,
      .electric_scenario, .aeo_scenario, mpe
    )

  bev_cost <-
    calc_cost(
      bev_vmt,
      mode, "BEVPrice"
    )


  vmt_all <- dplyr::bind_rows(
    ci_vmt,
    bev_vmt
  )

  dir_ghg_all <- dplyr::bind_rows(
    ci_ghg,
    bev_ghg
  )

  cost_all <- dplyr::bind_rows(
    ci_cost,
    bev_cost,
  )

  fuel_use_all <- dplyr::bind_rows(
    ci_fuel,
    bev_fuel
  )


  school_bus <- list(
    "vmt" =  vmt_all,
    "dir_ghg" =  dir_ghg_all,
    "fuel_use" = fuel_use_all,
    "cost"  = cost_all
  )

  usethis::ui_done(paste("School bus", emo::ji("school")))

  return(school_bus)
}
