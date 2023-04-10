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
                            .selected_ctu = "all",
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
                            .elast_5d = elast_5d,
                            .calc_transp_cost = FALSE,
                            .calc_transp_fuel_use = FALSE,
                            .calc_transp_ghg_embodied = FALSE) {
  # cli::cli_progress_message("** calculating scenario school bus \n")

  .pass_tb <- filter_ctu(.pass_tb, .selected_ctu)

  fcm <- calc_fuel_cost_mile(
    .pass_tb,
    .mode = "PLDV",
    .aeo_scenario,
    .miles_per_gallon = "SIMPG",
    .enviro_factors$SI_FUEL_COST_GAL
  )

  school_bus <- list()

  type <- "P"
  # For all passenger modes, variable = PMT
  var <- "PMT"


  # School Bus-----
  mode <- "BS"

  # VMT Calculation

  ## CI School bus -----
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  message("School bus, diesel")

  ci_vmt <-
    calc_vmt_forecast(
      tb = .pass_tb,
      .scenario,
      .selected_ctu,
      mode, stock, var, fcm,
      .aeo_scenario, .transit_avo_pct, .transit_service_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price,
      .parking_price,
      .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_diversity_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change,
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .elast_5d = .elast_5d
    ) %>%
    mutate(class = class)

  ## BEV school bus -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"

  message("School bus, electric")
  bev_vmt <-
    calc_vmt_forecast(
      .scenario,
      .selected_ctu,
      tb = .pass_tb, mode, stock, var, fcm,
      .aeo_scenario, .transit_avo_pct, .transit_service_pct, .vmt_fee, .payd_fee, .gas_tax, .cong_price,
      .parking_price,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_diversity_pct_change, .intersection_design_pct_change, .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change,
      .elast = .elast,
      .enviro_factors = .enviro_factors,
      .elast_5d = .elast_5d
    ) %>%
    mutate(class = class)

  # GHG Calculation
  ci_ghg <-
    calc_ghg_direct(
      ci_vmt, .pass_tb,
      mode, "CI", .aeo_scenario, mpg
    )

  bev_ghg <-
    calc_ghg_direct(
      bev_vmt,
      .pass_tb, mode, .electric_scenario,
      .aeo_scenario, mpe
    )

  vmt_all <- dplyr::bind_rows(
    ci_vmt,
    bev_vmt
  )

  dir_ghg_all <- dplyr::bind_rows(
    ci_ghg,
    bev_ghg
  )

  school_bus <- list("vmt" = vmt_all, "dir_ghg" = dir_ghg_all)



  if (.calc_transp_fuel_use == TRUE) {
    ci_fuel <-
      calc_fuel_use(
        tb_vmt = ci_vmt,
        tb = .pass_tb,
        .mode = mode,
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = mpg
      )

    bev_fuel <-
      calc_fuel_use(
        tb_vmt = bev_vmt,
        tb = .pass_tb,
        .mode = mode,
        # .electric_scenario,
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = mpe
      )

    school_bus$fuel_use <- dplyr::bind_rows(
      ci_fuel,
      bev_fuel
    )
  }

  if (.calc_transp_cost == TRUE) {
    ci_cost <- calc_cost(ci_vmt, .selected_ctu, mode, "CIPrice")

    bev_cost <- calc_cost(bev_vmt, .selected_ctu, mode, "BEVPrice")

    school_bus$cost <- dplyr::bind_rows(
      ci_cost,
      bev_cost
    )
  }

  # Finish up -----

  if (.calc_transp_ghg_embodied == TRUE) {
    emb_ghg_all <- dir_ghg_all %>%
      dplyr::mutate(
        ghg_embodied_source = NA,
        type = type
      ) %>%
      dplyr::select(type, scenario, mode, ctu, year, aeo_mode,
        ghg_embodied_source,
        ghg_embodied = dir_ghg
      )

    school_bus$emb_ghg <- emb_ghg_all
  }

  usethis::ui_done(paste("School bus", emo::ji("school")))

  return(school_bus)
}
