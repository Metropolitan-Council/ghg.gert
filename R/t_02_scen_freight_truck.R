#' @title Calculate scenario for freight trucks
#' @family Freight
#' @family transportation
#'
#' @inheritParams run_scenario_transportation
#' @inheritParams calc_vmt_forecast
#'
#' @export
#'
#' @importFrom emo ji
#' @importFrom cli cli_alert_success
scen_freight_truck <- function(.freight_tb = transportation_data$freight,
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
                               .factor_values = factor_values,
                               .elast = elast,
                               .elast_5d = elast_5d) {
  .freight_tb <- filter_ctu(.freight_tb, .selected_ctu)

  mode <- "FR"
  # (measured in ton-miles NOT miles)
  type <- "F"
  # For all freight, var = TMT
  var <- "TMT"



  ## CUT ----
  mode <- "CUT"

  ### CI -----
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"



  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(
    tb = .freight_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = mpg,
    .fuel_cost_gallon = .enviro_factors$CI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  cut_ci_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .freight_tb,
      .mode = mode,
      .stock = stock,
      .variable = var,
      .tb_fuel_cost_mile = fcm,
      .aeo_scenario = .aeo_scenario,
      .transit_avo_pct = .transit_avo_pct,
      .pldv_avo_pct = .pldv_avo_pct,
      .transit_service_pct = .transit_service_pct,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .cong_price = .cong_price,
      .parking_price = .parking_price,
      .freight_parking_price = .freight_parking_price,
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
      .enviro_factors = .enviro_factors,
      .elast = .elast,
      .elast_5d = .elast_5d,
      .factor_values = .factor_values
    ) %>%
    mutate(class = class)


  cut_ci_ghg <-
    calc_ghg_direct(
      tb_vmt = cut_ci_vmt,
      tb = .freight_tb,
      .mode = mode,
      .fuel_type = "CUTCI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values
    )


  ### BEV  -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"


  fcm <- calc_fuel_cost_mile(
    tb = .freight_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = mpe,
    .fuel_cost_gallon = .enviro_factors$ELEC_FUEL_COST_KWH,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  cut_bev_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .freight_tb,
      .mode = mode,
      .stock = stock,
      .variable = var,
      .tb_fuel_cost_mile = fcm,
      .aeo_scenario = .aeo_scenario,
      .transit_avo_pct = .transit_avo_pct,
      .pldv_avo_pct = .pldv_avo_pct,
      .transit_service_pct = .transit_service_pct,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .cong_price = .cong_price,
      .parking_price = .parking_price,
      .freight_parking_price = .freight_parking_price,
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
      .elast_5d = .elast_5d,
      .factor_values = .factor_values
    ) %>%
    mutate(class = class)

  cut_bev_ghg <-
    calc_ghg_direct(
      tb_vmt = cut_bev_vmt,
      tb = .freight_tb,
      .mode = mode,
      .fuel_type = .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpe,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values
    )


  ## SUT -----
  mode <- "SUT"

  ### CI ----
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"


  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(
    tb = .freight_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = mpg,
    .fuel_cost_gallon = .enviro_factors$CI_FUEL_COST_GAL,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  sut_ci_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .freight_tb,
      .mode = mode,
      .stock = stock,
      .variable = var,
      .tb_fuel_cost_mile = fcm,
      .aeo_scenario = .aeo_scenario,
      .transit_avo_pct = .transit_avo_pct,
      .pldv_avo_pct = .pldv_avo_pct,
      .transit_service_pct = .transit_service_pct,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .cong_price = .cong_price,
      .parking_price = .parking_price,
      .freight_parking_price = .freight_parking_price,
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
      .elast_5d = .elast_5d,
      .factor_values = .factor_values
    ) %>%
    mutate(class = class)

  sut_ci_ghg <-
    calc_ghg_direct(
      tb_vmt = sut_ci_vmt,
      tb = .freight_tb,
      .mode = mode,
      .fuel_type = "SUTCI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpg,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values
    )


  ### BEV -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"


  fcm <- calc_fuel_cost_mile(
    tb = .freight_tb,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = mpe,
    .fuel_cost_gallon = .enviro_factors$ELEC_FUEL_COST_KWH,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )


  sut_bev_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      .selected_ctu = .selected_ctu,
      tb = .freight_tb,
      .mode = mode,
      .stock = stock,
      .variable = var,
      .tb_fuel_cost_mile = fcm,
      .aeo_scenario = .aeo_scenario,
      .transit_avo_pct = .transit_avo_pct,
      .pldv_avo_pct = .pldv_avo_pct,
      .transit_service_pct = .transit_service_pct,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .cong_price = .cong_price,
      .parking_price = .parking_price,
      .freight_parking_price = .freight_parking_price,
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
      .elast_5d = .elast_5d,
      .factor_values = .factor_values
    ) %>%
    mutate(class = class)



  sut_bev_ghg <-
    calc_ghg_direct(
      tb_vmt = sut_bev_vmt,
      tb = .freight_tb,
      .mode = mode,
      .fuel_type = .electric_scenario,
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = mpe,
      .enviro_factors = .enviro_factors,
      .factor_values = .factor_values
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

  cli::cli_alert_success(paste("Freight trucks", emo::ji("truck")))

  return(freight_truck)
}
