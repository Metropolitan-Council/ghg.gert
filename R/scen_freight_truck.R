#' Calculate scenario for freight trucks
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
#' @importFrom usethis ui_done
scen_freight_truck <- function(.freight_tb = transportation_data$freight,
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
                               .land_use_pct_change = 0,
                               .intersection_design_pct_change = 0,
                               .job_access_pct_change = 0,
                               .transit_dist_pct_change = 0,
                               .comb_5d_impact_pct_change = 0,
                               .telework_pct = 0,
                               .mit_bau_summary = 0,
                               .enviro_factors = enviro_factors) {
  mode <- "FR"
  # (measured in ton-miles NOT miles)
  type <- "F"
  # For all freight, var = TMT
  var <- "TMT"



  ## Heavy truck (CUT) ----
  mode <- "CUT"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"

  message("Combined truck freight, diesel")

  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(
    .freight_tb, mode,
    .aeo_scenario, mpg, .enviro_factors$CI_FUEL_COST_GAL
  )

  cut_ci_vmt <-
    calc_vmt_forecast(
      .scenario,
      .freight_tb,
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


  cut_ci_ghg <-
    calc_ghg_direct(
      cut_ci_vmt, .freight_tb,
      mode, "CUTCI", .aeo_scenario, mpg
    )


  ### Heavy  battery electric -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"
  message("Combined truck freight, battery electric")

  cut_bev_vmt <-
    calc_vmt_forecast(
      .scenario, .freight_tb, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>%
    mutate(class = class)

  cut_bev_ghg <-
    calc_ghg_direct(
      cut_bev_vmt,
      .freight_tb, mode,
      .electric_scenario, .aeo_scenario, mpe
    )


  ## Medium truck (SUT) -----
  mode <- "SUT"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  message("Single truck freight, diesel")

  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(
    .freight_tb,
    mode,
    .aeo_scenario,
    mpg,
    .enviro_factors$CI_FUEL_COST_GAL
  )

  sut_ci_vmt <-
    calc_vmt_forecast(
      .scenario, .freight_tb, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct,
      .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>% mutate(class = class)

  sut_ci_ghg <-
    calc_ghg_direct(
      sut_ci_vmt,
      .freight_tb, mode,
      "SUTCI", .aeo_scenario, mpg
    )


  ### Medium truck battery electric -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"
  message("Combined truck freight, battery electric")

  sut_bev_vmt <-
    calc_vmt_forecast(
      .scenario, .freight_tb, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo,
      .transit_rider_pct, .vmt_fee, .payd_fee, .gas_tax,
      .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>%
    mutate(class = class)



  sut_bev_ghg <-
    calc_ghg_direct(
      sut_bev_vmt, .freight_tb,
      mode, .electric_scenario, .aeo_scenario, mpe
    )


  # Finish up -----


  vmt_all <- dplyr::bind_rows(
    sut_ci_vmt,
    sut_bev_vmt,
    cut_ci_vmt,
    cut_bev_vmt
  )


  dir_ghg_all <- dplyr::bind_rows(
    sut_ci_ghg,
    sut_bev_ghg,
    cut_ci_ghg,
    cut_bev_ghg
  )


  freight_truck <- list(
    "vmt" = vmt_all,
    "dir_ghg" = dir_ghg_all
  )

  usethis::ui_done(paste("Freight trucks", emo::ji("truck")))

  return(freight_truck)
}
