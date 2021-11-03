#' Title
#'
#' Calculate scenario for walk and bike
#'
#' @inheritParams scenario_results
#' @inheritParams calc_vmt_forecast
#'
#' @family transportation results, passenger
#'
#' @return
#' @export
#'
#' @importFrom emo ji
scen_walk_bike <- function(.pass_tb = transportation_data$passenger,
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
  fcm <- calc_fuel_cost_mile(
    .pass_tb,
    .mode = "PLDV",
    .aeo_scenario,
    .miles_per_gallon = "SIMPG",
    .enviro_factors$SI_FUEL_COST_GAL
  )

  type <- "P"
  # For all passenger modes, variable = PMT
  var <- "PMT"

  # browser()
  ## Walk -----
  mode <- "WALK"
  stock <- ""
  class <- "WALK"

  walk_vmt <-
    calc_vmt_forecast(
      .scenario, .pass_tb, mode, stock,
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
      .scenario, .pass_tb, mode,
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
  # browser()
  vmt_all <- dplyr::bind_rows(
    bike_vmt,
    walk_vmt
  )
  # browser()
  dir_ghg_all <- vmt_all %>%
    dplyr::mutate(
      dir_ghg = NA,
      type = type
    ) %>%
    dplyr::select(
      type, scenario, mode, ctu, year, aeo_mode,
      dir_ghg
    ) %>%
    unique()

  emb_ghg_all <- dir_ghg_all %>%
    dplyr::mutate(ghg_embodied_source = NA) %>%
    dplyr::select(type, scenario, mode, ctu, year, aeo_mode,
      ghg_embodied_source,
      ghg_embodied = dir_ghg
    )

  fuel_use_all <- dir_ghg_all %>%
    dplyr::select(type, scenario, mode, ctu, year, aeo_mode,
      fuel_use = dir_ghg
    )

  cost_all <- dir_ghg_all %>%
    dplyr::select(type,
      scenario, mode, ctu, year, aeo_mode,
      cost = dir_ghg
    )


  wb_fin <- list(
    "vmt" = vmt_all,
    "dir_ghg" = dir_ghg_all,
    "emb_ghg" = emb_ghg_all,
    "fuel_use" = fuel_use_all,
    "cost" = cost_all
  )

  usethis::ui_done(paste("Walk and bike", emo::ji("walking"), emo::ji("bike")))

  return(wb_fin)
}
