#' @title Calculate scenario for freight trucks
#' @family Freight
#' @family transportation
#'
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#' @export
#'
#' @importFrom cli cli_alert_success
#' @importFrom stringr str_to_lower
mode_freight_truck <- function(.freight_tb = transportation_data$freight,
                               .selected_ctu = "all",
                               .scenario = "BAU",
                               .electric_scenario = "ER",
                               .aeo_scenario = "REF",
                               .parking_cost = parking_cost,
                               .vehicle_occupancy = vehicle_occupancy,
                               .transit_avo_pct = ghg.gert::transportation_defaults$transit_avo_pct,
                               .pldv_avo_pct = ghg.gert::transportation_defaults$pldv_avo_pct,
                               .transit_service_pct = ghg.gert::transportation_defaults$transit_service_pct,
                               .vmt_fee = ghg.gert::transportation_defaults$vmt_fee,
                               .payd_fee = ghg.gert::transportation_defaults$payd_fee,
                               .gas_tax = ghg.gert::transportation_defaults$gas_tax,
                               .parking_price = ghg.gert::transportation_defaults$parking_price,
                               .freight_parking_price = ghg.gert::transportation_defaults$freight_parking_price,
                               .cong_price = ghg.gert::transportation_defaults$cong_price,
                               .freight_vmt_fee = ghg.gert::transportation_defaults$freight_vmt_fee,
                               .pop_dens_pct_change = ghg.gert::transportation_defaults$pop_dens_pct_change,
                               .emp_dens_pct_change = ghg.gert::transportation_defaults$emp_dens_pct_change,
                               .land_use_diversity_pct_change = ghg.gert::transportation_defaults$land_use_diversity_pct_change,
                               .intersection_design_pct_change = ghg.gert::transportation_defaults$intersection_design_pct_change,
                               .intersection_density_pct_change = ghg.gert::transportation_defaults$intersection_density_pct_change,
                               .job_access_pct_change = ghg.gert::transportation_defaults$job_access_pct_change,
                               .transit_dist_pct_change = ghg.gert::transportation_defaults$transit_dist_pct_change,
                               .comb_5d_impact_pct_change = ghg.gert::transportation_defaults$comb_5d_impact_pct_change,
                               .telework_pct = ghg.gert::transportation_defaults$telework_pct,
                               .enviro_factors = enviro_factors,
                               .factor_values = factor_values,
                               .elast = elast,
                               .elast_5d = elast_5d,
                               .fuel_economy = fuel_economy) {
  .freight_tb <- filter_ctu(.freight_tb, .selected_ctu)


  mode <- "FR"
  # (measured in ton-miles NOT miles)
  type <- "F"
  # For all freight, var = TMT
  var <- "TMT"


  # establish commonly passed parameters -----

  fcm_common_freight <- list(
    .aeo_scenario = .aeo_scenario,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values
  )

  vmt_common_freight <- list(
    tb = .freight_tb,
    .variable = var,
    .scenario = .scenario,
    .parking_cost = .parking_cost,
    .selected_ctu = .selected_ctu,
    .aeo_scenario = .aeo_scenario,
    .transit_avo_pct = .transit_avo_pct,
    .pldv_avo_pct = .pldv_avo_pct,
    .vehicle_occupancy = .vehicle_occupancy,
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
    .intersection_density_pct_change = .intersection_density_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    #
    .elast = .elast,
    .enviro_factors = .enviro_factors,
    .elast_5d = .elast_5d,
    .factor_values = .factor_values
  )

  dir_ghg_common_freight <- list(
    tb = .freight_tb,
    .aeo_scenario = .aeo_scenario,
    .enviro_factors = .enviro_factors,
    .factor_values = .factor_values,
    .fuel_economy = .fuel_economy
  )


  freight_params <- tribble(
    ~mode,  ~fuel_label, ~mpg_name,   ~stock_name, ~fuel_cost_var,                        ~fuel_type,
    "CUT",  "CI",        "CIMPG",     "CIStock",   .enviro_factors$CI_FUEL_COST_GAL,      "CUTCI",
    "CUT",  "BEV",       "BEVElec",   "BEVStock",  .enviro_factors$ELEC_FUEL_COST_KWH,    .electric_scenario,
    "SUT",  "CI",        "CIMPG",     "CIStock",   .enviro_factors$CI_FUEL_COST_GAL,      "SUTCI",
    "SUT",  "BEV",       "BEVElec",   "BEVStock",  .enviro_factors$ELEC_FUEL_COST_KWH,    .electric_scenario
  )

  results_list <- purrr::pmap(
    freight_params,
    run_freight_calculations,
    fcm_common_freight,
    vmt_common_freight,
    dir_ghg_common_freight
  )


  purrr::map(
    results_list,
    list2env,
    envir = environment()
  )

  # Finish up -----


  vmt_all <- dplyr::bind_rows(
    sut_ci_vmt,
    sut_bev_vmt,
    cut_ci_vmt,
    cut_bev_vmt
  )


  dir_ghg_all <- dplyr::bind_rows(
    sut_ci_dir_ghg,
    sut_bev_dir_ghg,
    cut_ci_dir_ghg,
    cut_bev_dir_ghg
  )


  freight_truck <- list(
    "vmt" = vmt_all,
    "dir_ghg" = dir_ghg_all
  )

  cli::cli_alert_success("Freight trucks 🚚")

  return(freight_truck)
}


#' @keywords internal
#' @importFrom stringr str_to_lower
run_freight_calculations <- function(
  mode, # "CUT" or "SUT"
  fuel_label, # "CI" or "BEV"
  mpg_name, # "CIMPG" or "BEVElec"
  stock_name, # "CIStock" or "BEVStock"
  fuel_cost_var, # .enviro_factors$CI_FUEL_COST_GAL or $ELEC_FUEL_COST_KWH
  fuel_type, # "CUTCI", "SUTCI", or .electric_scenario
  fcm_common_freight,
  vmt_common_freight,
  dir_ghg_common_freight
) {
  message(paste("Freight:", mode, "-", fuel_label))

  # browser()
  # 1. Fuel cost per mile
  fcm <- do.call(
    calc_fuel_cost_mile,
    c(
      # tb = .freight_tb,
      .mode = mode,
      .miles_per_gallon = mpg_name,
      .fuel_cost_gallon = fuel_cost_var,
      fcm_common_freight
    )
  )

  # 2. VMT forecast
  vmt <- do.call(
    calc_vmt_forecast,
    c(
      .mode = mode,
      .stock = stock_name,
      append(vmt_common_freight, list(.tb_fuel_cost_mile = fcm))
    )
  ) %>%
    dplyr::mutate(class = fuel_label)

  # 3. Direct GHG
  dir_ghg <- do.call(
    calc_ghg_direct,
    c(
      .mode = mode,
      .fuel_type = fuel_type,
      .miles_per_gallon = mpg_name,
      append(dir_ghg_common_freight, list(tb_vmt = vmt))
    )
  )


  fuel_label_lower <- stringr::str_to_lower(paste0(mode, "_", fuel_label))

  return(
    list(fcm, vmt, dir_ghg) %>%
      setNames(
        nm = c(
          paste0(fuel_label_lower, "_fcm"),
          paste0(fuel_label_lower, "_vmt"),
          paste0(fuel_label_lower, "_dir_ghg")
        )
      )
  )
}
