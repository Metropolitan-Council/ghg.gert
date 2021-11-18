#'
#' Calculate scenario for freight multi-modal, air, and water transportation
#'
#' @inheritParams run_scenario
#' @inheritParams calc_vmt_forecast
#'
#' @family transportation results, freight
#'
#' @return
#' @export
#'
#' @importFrom emo ji
#' @importFrom usethis ui_done
scen_air_water_multi <- function(.freight_tb = transportation_data$freight,
                                 .scenario = "BAU",
                                 .electric_scenario = "ER",
                                 .aeo_scenario = "REF",
                                 .transit_avo_pct = 0,
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

  # Multimodal -----

  type <- "F"
  # For all freight, var = TMT
  var <- "TMT"
  # browser()
  mode <- "MM"

  ## CI -----
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  message("Multimodal, diesel")

  mm_ci_vmt <-
    calc_vmt_forecast(
      .scenario, .freight_tb, mode,
      stock, var, fcm, .aeo_scenario,
      .transit_avo_pct, .transit_rider_pct, .vmt_fee, .payd_fee,
      .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_diversity_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>% mutate(class = class)

  mm_ci_ghg <-
    calc_ghg_direct(
      mm_ci_vmt,
      .freight_tb,
      mode, "MMCI", .aeo_scenario, mpg
    )



  ## battery electric-----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"
  message("Multimodal, battery electric")

  mm_bev_vmt <-
    calc_vmt_forecast(
      .scenario, .freight_tb, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo_pct, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct,
      .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_diversity_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>% mutate(class = class)

  mm_bev_ghg <-
    calc_ghg_direct(
      mm_bev_vmt,
      .freight_tb, mode,
      .electric_scenario, .aeo_scenario, mpe
    )


  # Air------
  mode <- "AIR"

  ## SI
  stock <- "SIStock"
  mpg <- "SIMPG"
  class <- "SI"

  message("Air, gasoline")


  air_si_vmt <-
    calc_vmt_forecast(
      .scenario, .freight_tb, mode,
      stock, var, fcm, .aeo_scenario, .transit_avo_pct,
      .transit_rider_pct, .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price,
      .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_diversity_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>% mutate(class = class)

  air_si_ghg <-
    calc_ghg_direct(
      air_si_vmt, .freight_tb, mode,
      "ASI",
      .aeo_scenario, mpg
    )


  # Water ------
  mode <- "WAT"

  message("Water, diesel")

  ## CI -----
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"

  wat_ci_vmt <-
    calc_vmt_forecast(
      .scenario, .freight_tb, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo_pct, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change, .land_use_diversity_pct_change,
      .intersection_design_pct_change, .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change
    ) %>% mutate(class = class)

  wat_ci_ghg <-
    calc_ghg_direct(
      wat_ci_vmt, .freight_tb,
      mode, "WCI", .aeo_scenario, mpg
    )



  # Finish up -----

  vmt_all <- dplyr::bind_rows(
    mm_ci_vmt,
    mm_bev_vmt,
    air_si_vmt,
    wat_ci_vmt
  )

  ghg_all <- dplyr::bind_rows(
    mm_ci_ghg,
    mm_bev_ghg,
    air_si_ghg,
    wat_ci_ghg
  )


  av_return <- list(
    "vmt" = vmt_all,
    "ghg" = ghg_all
  )

  usethis::ui_done(paste(
    "Freight air, water, multimodal",
    emo::ji("airplane"),
    emo::ji("ship"),
    emo::ji("frog")
  ))

  return(av_return)
}
