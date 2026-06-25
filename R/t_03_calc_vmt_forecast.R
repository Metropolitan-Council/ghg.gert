#' @title Calculate vehicle miles traveled by mode and power train
#'
#' @param .scenario character, scenario name. Useful for labeling.
#' @param tb input table for appropriate mode type. Should have columns `mode`, `var`, `geog_name`, `geog_id`,
#'    and one for each year. Package provided datasets `transportation_data$passenger` or
#'    `transportation_data$freight` are suitable.
#' @param .mode character, current mode
#' @param .stock character, stock for current mode
#' @param .variable character, variable name - e.g., "VMT"
#' @param .tb_fuel_cost_mile table, table with fuel cost per mile
#' @param .factor_values list, generalized factor values. Default is `ghg.ccap::factor_values`.
#' @param .elast table of elasticities. Default is `ghg.ccap::elast`.
#' @param .elast_5d table of 5D elasticities. Default is `ghg.ccap::elast_5d` included in this package.
#'
#' @inheritParams run_module_transportation
#' @inheritParams vmt_parking_policy
#' @inheritParams vmt_land_use_change
#' @inheritParams vmt_road_policy
#' @inheritParams vmt_transit_service
#' @inheritParams vmt_vehicle_occupancy
#' @inheritParams vmt_telework
#' @inheritParams filter_ctu
#'
#' ### Eqn: (PMT in 1000 mi) x Pr(stock by fuel) / AVO
#'
#' @return a tibble with columns `scenario`, `geog_name`, `year`, `aeo_mode`, `type`, `vmt`,
#'     with `vmt` in _thousands_ of miles.
#' @export
#' @family transportation
#'
#'
#' @importFrom dplyr filter select case_when
#' @importFrom tidyselect all_of
#'
calc_vmt_forecast <- function(.scenario,
                              tb,
                              .selected_ctu = "all",
                              .mode,
                              .stock,
                              .variable,
                              .tb_fuel_cost_mile,
                              .aeo_scenario = "REF",
                              .parking_cost = ghg.ccap::parking_cost,
                              .vehicle_occupancy = ghg.ccap::vehicle_occupancy,
                              .transit_avo_pct = ghg.ccap::transportation_defaults$transit_avo_pct,
                              .transit_service_pct = ghg.ccap::transportation_defaults$transit_service_pct,
                              .pldv_avo_pct = ghg.ccap::transportation_defaults$pldv_avo_pct,
                              .vmt_fee = ghg.ccap::transportation_defaults$vmt_fee,
                              .payd_fee = ghg.ccap::transportation_defaults$payd_fee,
                              .gas_tax = ghg.ccap::transportation_defaults$gas_tax,
                              .cong_price = ghg.ccap::transportation_defaults$cong_price,
                              .parking_price = ghg.ccap::transportation_defaults$parking_price,
                              .vmt_reduction_pct = ghg.ccap::transportation_defaults$vmt_reduction_pct,
                              .freight_parking_price = ghg.ccap::transportation_defaults$freight_parking_price,
                              .freight_vmt_fee = ghg.ccap::transportation_defaults$freight_vmt_fee,
                              .pop_dens_pct_change = ghg.ccap::transportation_defaults$pop_dens_pct_change,
                              .emp_dens_pct_change = ghg.ccap::transportation_defaults$emp_dens_pct_change,
                              .land_use_diversity_pct_change = ghg.ccap::transportation_defaults$land_use_diversity_pct_change,
                              .intersection_design_pct_change = ghg.ccap::transportation_defaults$intersection_design_pct_change,
                              .intersection_density_pct_change = ghg.ccap::transportation_defaults$intersection_density_pct_change,
                              .job_access_pct_change = ghg.ccap::transportation_defaults$job_access_pct_change,
                              .transit_dist_pct_change = ghg.ccap::transportation_defaults$transit_dist_pct_change,
                              .comb_5d_impact_pct_change = ghg.ccap::transportation_defaults$comb_5d_impact_pct_change,
                              .telework_pct = ghg.ccap::transportation_defaults$telework_pct,
                              .cbtp_prop_targeted = ghg.ccap::transportation_defaults$cbtp_prop_targeted,
                              .cbtp_start_year = ghg.ccap::transportation_defaults$cbtp_start_year,
                              .enviro_factors = ghg.ccap::enviro_factors,
                              .factor_values = ghg.ccap::factor_values,
                              .elast = ghg.ccap::elast,
                              .elast_5d = ghg.ccap::elast_5d) {
  tb <- filter_ctu(tb, .selected_ctu)

  check_inputs("mode", .mode)

  tb_vmt <- tb %>%
    dplyr::filter(
      mode == .mode,
      var == .variable
    ) %>%
    dplyr::mutate(
      miles_traveled = value,
      scenario = .scenario
    ) %>%
    dplyr::select(scenario, mode, geog_name, geog_id, year, aeo_mode, type, miles_traveled) %>%
    unique()


  # Shared land_use args (only .type varies)
  make_land_use <- function(.type, ...) {
    vmt_land_use_change(
      .type = .type,
      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
      .pop_dens_pct_change = .pop_dens_pct_change,
      .emp_dens_pct_change = .emp_dens_pct_change,
      .land_use_diversity_pct_change = .land_use_diversity_pct_change,
      .intersection_design_pct_change = .intersection_design_pct_change,
      .intersection_density_pct_change = .intersection_density_pct_change,
      .job_access_pct_change = .job_access_pct_change,
      .transit_dist_pct_change = .transit_dist_pct_change,
      .elast_5d = .elast_5d,
      .enviro_factors = .enviro_factors
    )
  }

  # Used by all modes except WALK and BIKE
  if (!.mode %in% c("WALK", "BIKE")) {
    mode_stock <- vmt_stock_proportion(.tb = tb, .mode = .mode, .stock = .stock)
    ann_energy_outlook <- vmt_annual_energy_outlook(
      tb = tb, .aeo_scenario = .aeo_scenario, .mode = .mode,
      .enviro_factors = .enviro_factors, .factor_values = .factor_values
    )
    veh_occupancy <- vmt_vehicle_occupancy(
      tb = tb, .tb_vmt = tb_vmt, .mode = .mode, .stock = .stock,
      .vehicle_occupancy = .vehicle_occupancy, .transit_avo_pct = .transit_avo_pct,
      .pldv_avo_pct = .pldv_avo_pct, .enviro_factors = .enviro_factors
    )
  }

  # Only road modes need fuel cost adjustments
  if (.mode %in% c("PLDV", "BU", "BRT", "RU", "RI", "SUT", "CUT")) {
    fc_adjustments <- vmt_road_policy(
      .pass_tb = tb, .tb_vmt = tb_vmt, .mode = .mode,
      .tb_fuel_cost_mile = .tb_fuel_cost_mile, .vmt_fee = .vmt_fee,
      .cong_price = .cong_price, .gas_tax = .gas_tax, .payd_fee = .payd_fee,
      .stock = .stock, .freight_vmt_fee = .freight_vmt_fee,
      .enviro_factors = .enviro_factors, .elast = .elast
    )
  }

  # Only modes with parking sensitivity
  if (.mode %in% c("PLDV", "BU", "BRT", "RU", "RI", "SUT", "WALK")) {
    parking <- vmt_parking_policy(
      tb = tb, .mode = .mode, .parking_cost = .parking_cost,
      .freight_parking_price = .freight_parking_price, .parking_price = .parking_price,
      .elast = .elast, .enviro_factors = .enviro_factors
    )
  }

  # Final select cols are identical in every branch
  final_cols <- c(
    "type", "stock", "scenario", "geog_name", "geog_id",
    "year", "mode", "aeo_mode", "vmt"
  )

  tb_fin <- switch(.mode,
    PLDV = {
      # browser()
      at_adjustment <- tb %>%
        dplyr::filter(mode == "AT", var == .variable) %>%
        dplyr::select(geog_name, geog_id, year, active_transportation_adj = value)

      trans_service <- vmt_transit_service(
        tb = tb, .mode = .mode, .transit_service_pct = .transit_service_pct,
        .elast = .elast, .enviro_factors = .enviro_factors
      )
      telework_adjust <- vmt_telework(
        .pass_tb = tb, .mode = .mode, .telework_pct = .telework_pct,
        .enviro_factors = .enviro_factors
      )
      vmt_total_adj <- vmt_total_reduction(
        .pass_tb = tb, .mode = .mode, .vmt_reduction_pct = .vmt_reduction_pct,
        .enviro_factors = .enviro_factors
      )
      cbtp_adjust <-
        vmt_trip_reduction(
          .pass_tb = tb,
          .cbtp_prop_targeted = .cbtp_prop_targeted,
          .cbtp_start_year = .cbtp_start_year,
          .enviro_factors = .enviro_factors
        )

      tb_vmt %>%
        dplyr::left_join(ann_energy_outlook, by = "year") %>%
        dplyr::left_join(trans_service, by = c("geog_name", "geog_id", "year")) %>%
        dplyr::left_join(fc_adjustments, by = c("geog_name", "geog_id", "year")) %>%
        dplyr::left_join(make_land_use("DRIVE"), by = "year") %>%
        dplyr::left_join(parking, by = c("year", "geog_name", "geog_id")) %>%
        dplyr::left_join(veh_occupancy, by = c("year", "geog_name", "geog_id")) %>%
        dplyr::left_join(telework_adjust, by = "year") %>%
        dplyr::left_join(vmt_total_adj, by = "year") %>%
        dplyr::left_join(mode_stock, by = c("geog_name", "geog_id", "year", "mode")) %>%
        dplyr::left_join(at_adjustment, by = c("year", "geog_name", "geog_id")) %>%
        dplyr::left_join(cbtp_adjust, by = c("year", "geog_name", "geog_id")) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          pass_ld_vmt = (((miles_traveled * vmt_reduction_adj) - (transit_adj * mode_stock_adj)) *
            cbtp_adj * aeo_adj * vmt_fee_adj * cong_adjust * gas_adj *
            telework_adj * land_use_adj * park_price_adj) / occupancy_adj * mode_stock_adj,
          stock = .stock,
          vmt = pass_ld_vmt
        ) %>%
        dplyr::select(dplyr::all_of(final_cols)) %>%
        dplyr::distinct()
    },
    BU = ,
    BRT = ,
    RU = ,
    RI = {
      trans_service <- vmt_transit_service(
        tb = tb, .mode = .mode, .transit_service_pct = .transit_service_pct,
        .elast = .elast, .enviro_factors = .enviro_factors
      )

      tb_vmt %>%
        dplyr::left_join(ann_energy_outlook, by = "year") %>%
        dplyr::left_join(trans_service, by = c("geog_name", "geog_id", "year")) %>%
        dplyr::left_join(fc_adjustments, by = c("geog_name", "geog_id", "year")) %>%
        dplyr::left_join(make_land_use("TRANSIT"), by = "year") %>%
        dplyr::left_join(parking, by = c("year", "geog_name", "geog_id")) %>%
        dplyr::left_join(veh_occupancy, by = c("year", "geog_name", "geog_id")) %>%
        dplyr::left_join(mode_stock, by = c("geog_name", "geog_id", "year", "mode")) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          transit_vmt = ((miles_traveled * aeo_adj * transit_adj *
            (1 + ((vmt_fee_adj + payd_ins_adj + cong_adjust) * cross_vmt)) *
            land_use_adj * park_price_adj * gas_adj) / occupancy_adj) * mode_stock_adj,
          transit_vmt = dplyr::case_when(
            is.infinite(transit_vmt) | is.na(transit_vmt) | transit_vmt < 0 ~ 0,
            TRUE ~ transit_vmt
          ),
          stock = .stock,
          vmt = transit_vmt
        ) %>%
        dplyr::select(dplyr::all_of(final_cols))
    },
    SUT = {
      tb_vmt %>%
        dplyr::left_join(ann_energy_outlook, by = "year") %>%
        dplyr::left_join(parking, by = c("year", "geog_name", "geog_id")) %>%
        dplyr::left_join(veh_occupancy, by = c("year", "geog_name", "geog_id")) %>%
        dplyr::left_join(fc_adjustments, by = "year") %>%
        dplyr::left_join(mode_stock, by = c("geog_name", "geog_id", "year", "mode")) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          stock = .stock,
          vmt   = ((miles_traveled * aeo_adj * vmt_fee_adj * park_price_adj) / occupancy_adj) * mode_stock_adj
        ) %>%
        dplyr::select(dplyr::all_of(final_cols))
    },
    CUT = {
      tb_vmt %>%
        dplyr::left_join(ann_energy_outlook, by = "year") %>%
        dplyr::left_join(veh_occupancy, by = c("year", "geog_name", "geog_id")) %>%
        dplyr::left_join(fc_adjustments, by = "year") %>%
        dplyr::left_join(mode_stock, by = c("geog_name", "geog_id", "year", "mode")) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          stock = .stock,
          vmt   = ((miles_traveled * aeo_adj * vmt_fee_adj) / occupancy_adj) * mode_stock_adj
        ) %>%
        dplyr::select(dplyr::all_of(final_cols))
    },
    WALK = {
      tb_vmt %>%
        dplyr::left_join(make_land_use("WALK"), by = "year") %>%
        dplyr::left_join(parking, by = c("year", "geog_name", "geog_id")) %>%
        dplyr::distinct() %>%
        dplyr::mutate(stock = .stock, vmt = miles_traveled * land_use_adj * park_price_adj) %>%
        dplyr::select(dplyr::all_of(final_cols))
    },
    BIKE = {
      tb_vmt %>%
        dplyr::left_join(make_land_use("WALK"), by = "year") %>%
        dplyr::distinct() %>%
        dplyr::mutate(stock = .stock, vmt = miles_traveled * land_use_adj) %>%
        dplyr::select(dplyr::all_of(final_cols))
    },
    BS = {
      tb_vmt %>%
        dplyr::left_join(veh_occupancy, by = c("year", "geog_name", "geog_id")) %>%
        dplyr::left_join(ann_energy_outlook, by = "year") %>%
        dplyr::left_join(mode_stock, by = c("geog_name", "geog_id", "year", "mode")) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          stock = .stock,
          vmt   = (miles_traveled * aeo_adj / occupancy_adj) * mode_stock_adj
        ) %>%
        dplyr::select(dplyr::all_of(final_cols))
    },
    FR = {
      tb_vmt %>%
        dplyr::left_join(ann_energy_outlook, by = "year") %>%
        dplyr::left_join(veh_occupancy, by = c("year", "geog_name", "geog_id")) %>%
        dplyr::left_join(mode_stock, by = c("geog_name", "geog_id", "year", "mode")) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          stock = .stock,
          vmt   = (miles_traveled * aeo_adj / occupancy_adj) * mode_stock_adj
        ) %>%
        dplyr::select(dplyr::all_of(final_cols))
    },
    MM = ,
    AIR = ,
    WAT = {
      tb_vmt %>%
        dplyr::left_join(ann_energy_outlook, by = "year") %>%
        dplyr::left_join(veh_occupancy, by = c("year", "geog_name", "geog_id")) %>%
        dplyr::left_join(mode_stock, by = c("geog_name", "geog_id", "year", "mode")) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          scenario = .scenario,
          stock = .stock,
          vmt = (miles_traveled * aeo_adj / occupancy_adj) * mode_stock_adj,
          vmt = dplyr::case_when(
            is.infinite(vmt) | is.na(vmt) | vmt < 0 ~ 0,
            TRUE ~ vmt
          )
        ) %>%
        dplyr::select(dplyr::all_of(final_cols)) %>%
        dplyr::distinct()
    }
  )

  # final return -----
  return(tb_fin %>% dplyr::ungroup())
}
