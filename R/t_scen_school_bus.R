#' @title Calculate scenario for school buses
#' @family passenger
#' @family transportation
#'
#' @inheritParams run_scenario_transportation
#' @inheritParams calc_vmt_forecast
#'
#' @return [tibble::tibble()] with column names...
#' @export
#' @importFrom usethis ui_done
#' @importFrom emo ji
scen_school_bus <- function(.pass_tb = transportation_data$passenger,
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
      tb = .pass_tb,
      mode, stock, var, fcm,
      .aeo_scenario, .transit_avo_pct, .transit_service_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price,
      .parking_price, .drs_pct, .av_pct, .freight_vmt_fee,
      .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_diversity_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change,
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .elast_5d = .elast_5d
    ) %>%
    mutate(class = class)


  ci_ghg <-
    calc_ghg_direct(
      ci_vmt, .pass_tb,
      mode, "CI", .aeo_scenario, mpg
    )

  ci_fuel <-
    calc_fuel_use(
      tb_vmt = ci_vmt,
      tb = .pass_tb,
      .mode = mode,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg
      # .is_av = .is_av
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
      .scenario, .pass_tb, mode, stock, var, fcm,
      .aeo_scenario, .transit_avo_pct, .transit_service_pct, .vmt_fee, .payd_fee, .gas_tax, .cong_price,
      .parking_price, .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_diversity_pct_change, .intersection_design_pct_change, .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change,
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .elast_5d = .elast_5d
    ) %>%
    mutate(class = class)

  bev_ghg <-
    calc_ghg_direct(
      bev_vmt,
      .pass_tb, mode, .electric_scenario,
      .aeo_scenario, mpe
    )

  bev_fuel <-
    calc_fuel_use(
      tb_vmt = bev_vmt,
      tb = .pass_tb,
      .mode = mode,
      # .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpe
      # .is_av = .is_av
    )

  bev_cost <-
    calc_cost(
      bev_vmt,
      mode, "BEVPrice"
    )


  # Finish up -----
  vmt_all <- dplyr::bind_rows(
    ci_vmt,
    bev_vmt
  )

  dir_ghg_all <- dplyr::bind_rows(
    ci_ghg,
    bev_ghg
  )

  emb_ghg_all <- dir_ghg_all %>%
    dplyr::mutate(
      ghg_embodied_source = NA,
      type = type
    ) %>%
    dplyr::select(type, scenario, mode, ctu, year, aeo_mode,
      ghg_embodied_source,
      ghg_embodied = dir_ghg
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
    "vmt" = vmt_all,
    "dir_ghg" = dir_ghg_all,
    "emb_ghg" = emb_ghg_all,
    "fuel_use" = fuel_use_all,
    "cost" = cost_all
  )

  usethis::ui_done(paste("School bus", emo::ji("school")))

  return(school_bus)
}
