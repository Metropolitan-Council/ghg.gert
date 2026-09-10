#' @title  Calculate strategy effects on vehicle miles traveled.
#' @family transportation
#' @family VMT Effects
#'
#' @description Calculates annual energy outlook (AEO)
#' multipliers for each forecast year.
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @return a table with columns `aeo_scen`, `metric`, `mode`, `year`, and `aeo_adj`.
#' @export
#' @importFrom dplyr pull
vmt_annual_energy_outlook <- function(tb,
                                      .mode,
                                      .aeo_scenario,
                                      .enviro_factors = ghg.ccap::enviro_factors,
                                      .factor_values = ghg.ccap::factor_values) {
  check_inputs(name = "aeo_scenario", value = .aeo_scenario)

  aeo_mode <- tb %>%
    dplyr::filter(mode == .mode) %>%
    dplyr::pull(aeo_mode) %>%
    unique()

  .factor_values$aeo %>%
    dplyr::filter(
      aeo_scen == .aeo_scenario,
      metric == "VMT",
      mode == aeo_mode
    ) %>%
    dplyr::select(year, metric, aeo_adj = value) %>%
    return()
}

#' Calculate combined 5D land use change impact
#'
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#' @param .type character, one of `"DRIVE"`, `"WALK"`, `"TRANSIT"`.
#' @param .pop_dens_pct_change percent change in population density in the final forecast year relative to BAU.
#'     Numeric between -1 and 1. Default is `r ghg.ccap::transportation_defaults$pop_dens_pct_change`.
#' @param .emp_dens_pct_change percent change in employment density in the final forecast year relative to BAU.
#'      Numeric between -1 and 1. Default is `r ghg.ccap::transportation_defaults$emp_dens_pct_change`
#' @param .land_use_diversity_pct_change percent change in land use diversity/mix in the final forecast year relative to BAU.
#'      Numeric between -1 and 1. Default is `r ghg.ccap::transportation_defaults$land_use_diversity_pct_change`
#' @param .intersection_design_pct_change percent change in intersection design
#'     (% 4-way stops) in the final forecast year relative to BAU.
#'     Numeric between -1 and 1.  Default is `r ghg.ccap::transportation_defaults$intersection_design_pct_change`
#' @param .intersection_density_pct_change percent change in intersection density
#'     (intersections per square mile) in the final forecast year relative to BAU.
#'     Numeric between -1 and 1.  Default is `r ghg.ccap::transportation_defaults$intersection_density_pct_change`
#' @param .job_access_pct_change percent change in job accessibility in the final forecast year relative to BAU.
#'     Numeric between -1 and 1. Default is `r ghg.ccap::transportation_defaults$job_access_pct_change`
#' @param .transit_dist_pct_change percent change in transit distance in the final forecast year relative to BAU.
#'     Numeric between -1 and 1. Default is `r ghg.ccap::transportation_defaults$transit_dist_pct_change`
#' @param .comb_5d_impact_pct_change percent change in population density in the final forecast year relative to BAU
#'      as a measure of composite change in 5Ds on VMT. Numeric between -1 and 1.
#'      Default is `r ghg.ccap::transportation_defaults$comb_5d_impact_pct_change`
#'
#' @note An input value of 1 indicates a 100% increase, where a value of 0.75 indicates a 75% increase.
#'     A value of -0.75 indicates a 75% decrease.
#'
#' @return a table with columns `year`, `type`, and `land_use_adj`
#' @export
#' @details
#'
#' \loadmathjax
#' \mjdeqn{GHG = \frac{PMT}{AVO \times FF} \times LUF \times GF}{GHG = (PMT)/(AVO x FF) x LUF x GF}
#'     where \eqn{GHG} is the impact in metric tons of CO2 equivalent,
#'     \eqn{PMT} is passenger miles traveled,
#'     \eqn{AVO} is the average vehicle occupancy,
#'     \eqn{FF} is the fuel factor representing consumption of fuel per mile of travel,
#'     \eqn{GF} is the greenhouse gas factor per unit of consumed fuel,
#'     and \eqn{LUF} is the land use elasticity factor.
#'
#'
#' @family VMT effects
vmt_land_use_change <- function(.type,
                                .comb_5d_impact_pct_change = ghg.ccap::transportation_defaults$comb_5d_impact_pct_change,
                                .pop_dens_pct_change = ghg.ccap::transportation_defaults$pop_dens_pct_change,
                                .emp_dens_pct_change = ghg.ccap::transportation_defaults$emp_dens_pct_change,
                                .land_use_diversity_pct_change = ghg.ccap::transportation_defaults$land_use_diversity_pct_change,
                                .intersection_design_pct_change = ghg.ccap::transportation_defaults$intersection_design_pct_change,
                                .intersection_density_pct_change = ghg.ccap::transportation_defaults$intersection_density_pct_change,
                                .job_access_pct_change = ghg.ccap::transportation_defaults$job_access_pct_change,
                                .transit_dist_pct_change = ghg.ccap::transportation_defaults$transit_dist_pct_change,
                                .enviro_factors = ghg.ccap::enviro_factors,
                                .elast_5d = ghg.ccap::elast_5d) {
  max_value <- switch(.type,
    DRIVE   = 1 + .enviro_factors$MAX_5D_DR,
    TRANSIT = 1 + .enviro_factors$MAX_5D_TRANS,
    WALK    = 1 + .enviro_factors$MAX_5D_ACT,
    cli::cli_abort(".type must be one of 'WALK', 'DRIVE', or 'TRANSIT'.")
  )

  .elast_5d %>%
    dplyr::ungroup() %>%
    dplyr::filter(type == .type) %>%
    dplyr::mutate(
      # multiply parameter input by elasticity value
      n_population_density = 1 + .pop_dens_pct_change * population_density,
      n_employment_density = 1 + .emp_dens_pct_change * employment_density,
      n_diversity = 1 + .land_use_diversity_pct_change * diversity,
      n_design = 1 + .intersection_design_pct_change * design,
      n_intersection_density = 1 + .intersection_density_pct_change * intersection_density,
      n_job_access = 1 + .job_access_pct_change * job_access,
      n_distance = 1 + .transit_dist_pct_change * distance,
      n_combined_density = 1 + .pop_dens_pct_change * combined_density,
      # multiply all 5D effects together to get a single land use adjustment factor
      product_all = n_population_density * n_employment_density * n_diversity *
        n_design * n_intersection_density * n_job_access * n_distance * n_combined_density,
      # check that the product of all 5D effects does not exceed the maximum value for each mode
      land_use_adj = dplyr::case_when(
        .type == "DRIVE" & product_all < max_value ~ max_value,
        .type != "DRIVE" & product_all > max_value ~ max_value,
        TRUE ~ product_all
      ),
      # if the land use adjustment factor is 0, set it to 1 to avoid multiplying by 0
      land_use_adj = dplyr::if_else(land_use_adj == 0, 1, land_use_adj)
    ) %>%
    dplyr::select(year, land_use_adj) %>%
    return()
}

#' @title Calculate parking price effect for passenger light-duty vehicles (PLDV) for each forecast year
#'
#' @param .parking_price numeric, measured in dollars per hour.
#'   Default is `r ghg.ccap::transportation_defaults$parking_price`.
#' @param .freight_parking_price numeric, measured in dollars per hour.
#'   Default is `r ghg.ccap::transportation_defaults$freight_parking_price`.
#' @param .parking_cost table, existing parking cost assumptions. Default is `ghg.ccap::parking_cost`.
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#' @return a table with
#' @export
#' @details
#'
#' Increase parking prices will decrease PLDV by up to 30% and increase all transit
#' modes and walk.
#'
#' The long-run elasticity of VMT to parking cost is estimated to be in the range
#'     of -0.18 to -0.45 based on a meta-analysis of values reported in the literature
#'      (Lehner & Peer, 2019). On-street parking in central St. Paul costs between
#'       $0.50 to $2.00 per hour. However, parking is free in most locations and
#'       at most times of the day. In the absence of better data, we use an
#'       estimate of the search cost found for Melbourne, Australia of $1.00 per hour
#'       in St. Paul. For the other two CTU (Lake Elmo and Shoreview), it is assumed
#'       that the search cost is considerably lower and an estimate of $0.10 per hour
#'       is used as the marginal parking cost. TBI data bear out this assumption, with
#'        average monetary parking costs of $0 for trips ending in Lake Elmo and Shoreview
#'        and $0.22 for trips ending in St. Paul.
#'
#' \loadmathjax
#' \mjdeqn{GHG = \frac{PMT}{AVO \times FF} \times AV \times GF}{GHG = (PMT)/(AVO x FF) x AV x GF}
#'     where \eqn{GHG} is the impact in metric tons of CO2 equivalent,
#'     \eqn{PMT} is passenger miles traveled,
#'     \eqn{AVO} is the average vehicle occupancy,
#'     \eqn{FF} is the fuel factor representing consumption of fuel per mile of travel,
#'     \eqn{GF} is the greenhouse gas factor per unit of consumed fuel,
#'     and \eqn{AV} is an adjustment factor for the effect of introducing vehicle automation on VMT by mode
#' @family VMT effects
#' @importFrom dplyr cross_join
vmt_parking_policy <- function(tb,
                               .mode,
                               .parking_cost = ghg.ccap::parking_cost,
                               .elast = ghg.ccap::elast,
                               .parking_price = ghg.ccap::transportation_defaults$parking_price,
                               .freight_parking_price = ghg.ccap::transportation_defaults$freight_parking_price,
                               .enviro_factors = ghg.ccap::enviro_factors) {
  if (!.mode %in% unique(tb$mode)) {
    cli::cli_abort("Make sure you are using the correct input table")
  }

  # filtering to var == "PARK" gives one value per row
  park_price_current <- .parking_cost %>%
    filter_ctu(unique(tb$geog_name)) %>%
    dplyr::filter(mode == .mode, var == "PARK") %>%
    dplyr::distinct() %>%
    dplyr::select(geog_name, geog_id, PARK = value)

  switch(.mode,
    PLDV = ,
    AV = {
      park_price_current %>%
        dplyr::cross_join(.elast) %>%
        dplyr::mutate(
          park_price_base = dplyr::case_when(
            PARK == 0 & .parking_price == 0 ~ 1.0,
            PARK == 0 & .parking_price > 0 ~ 1 + 1.0 * park_elast,
            TRUE ~ 1 + (.parking_price / PARK) * park_elast
          ),
          park_price_adj = pmax(1 + .enviro_factors$MAX_PARKING_REDUCTION_PCT, park_price_base)
        ) %>%
        dplyr::select(year, geog_name, geog_id, park_price_adj) %>%
        return()
    },
    BU = ,
    BRT = ,
    RU = ,
    RI = ,
    WALK = {
      .parking_cost %>%
        filter_ctu(unique(tb$geog_name)) %>%
        dplyr::filter(mode == "PLDV", var == "PARK") %>%
        dplyr::distinct() %>%
        dplyr::select(geog_name, geog_id, PARK = value) %>%
        dplyr::cross_join(.elast) %>%
        dplyr::mutate(
          park_price_base = dplyr::case_when(
            PARK == 0 & .parking_price == 0 ~ 1.0,
            PARK == 0 & .parking_price > 0 ~ 1 + 1.0 * park_transit,
            TRUE ~ 1 + (.parking_price / PARK) * park_transit
          ),
          # Apply ceiling for transit: cannot increase more than inverse of vehicle reduction
          park_price_adj = pmin(1 - .enviro_factors$MAX_PARKING_REDUCTION_PCT, park_price_base)
        ) %>%
        dplyr::select(year, geog_name, geog_id, park_price_adj) %>%
        return()
    },
    SUT = {
      park_price_current %>%
        dplyr::cross_join(.elast) %>%
        dplyr::mutate(
          park_price_base = dplyr::case_when(
            PARK == 0 & .freight_parking_price == 0 ~ 1.0,
            PARK == 0 & .freight_parking_price > 0 ~ 1 + 1.0 * park_elast,
            TRUE ~ 1 + (.freight_parking_price / PARK) * park_elast
          ),
          park_price_adj = pmax(1 + .enviro_factors$MAX_PARKING_REDUCTION_PCT, park_price_base)
        ) %>%
        dplyr::select(year, geog_name, geog_id, park_price_adj) %>%
        return()
    },
    cli::cli_abort("Parking adjustment is not applicable for {(.mode)}")
  )
}


#' Calculate fuel, VMT, stock, congestion, and gas adjustments for each forecast year
#'
#' @note This function restricts inputs to include either a PAYD fee or a VMT fee, but not both.
#'     There are differences in their acceptability and implementation, but they are essentially
#'     targeting the same thing. A VMT fee would be paid by the driver and could be varied based
#'     on the time of day and location. However, it could also be a straight fee per mile. PAYD
#'     is paid to an insurance provider in place of a flat insurance rate. The main difference is
#'     whether the driver would prefer to pay the cost to a government agency or insurance provider.
#'
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#' @param .tb_vmt [tibble::tibble()], VMT table
#' @param .vmt_fee VMT fee in dollars per mile.
#'   Default is `r ghg.ccap::transportation_defaults$vmt_fee`.
#' @param .payd_fee  Pay-as-you-drive (PAYD) insurance fee in dollars per mile.
#'  Default is `r ghg.ccap::transportation_defaults$payd_fee`.
#' @param .gas_tax Gas tax in dollars per mile.
#'   Default is `r ghg.ccap::transportation_defaults$gas_tax`.
#' @param .cong_price Congestion price in dollars per mile (only applies to an approximation of
#'     congested miles in MSP).
#'     Default is `r ghg.ccap::transportation_defaults$cong_price`.
#' @param .freight_vmt_fee freight VMT fee per mile.
#'   Default is `r ghg.ccap::transportation_defaults$freight_vmt_fee`.
#'
#' @return a table with columns   `year`, `geog_name`, `fuel_time_cost_mile`, `payd_ins_adj`,
#'    `vmt_fee_adj`, `cong_adjust`, `cross_vmt`, `gas_adj`
#'
#' @export
#' @family VMT effects
vmt_road_policy <- function(.pass_tb,
                            .tb_vmt,
                            .mode,
                            .tb_fuel_cost_mile,
                            .stock,
                            .vmt_fee = ghg.ccap::transportation_defaults$vmt_fee,
                            .freight_vmt_fee = ghg.ccap::transportation_defaults$freight_vmt_fee,
                            .cong_price = ghg.ccap::transportation_defaults$cong_price,
                            .gas_tax = ghg.ccap::transportation_defaults$gas_tax,
                            .payd_fee = ghg.ccap::transportation_defaults$payd_fee,
                            .enviro_factors = ghg.ccap::enviro_factors,
                            .elast = ghg.ccap::elast) {
  if (.vmt_fee > 0 & .payd_fee > 0) {
    cli::cli_abort("Implement a VMT fee OR a pay-as-you drive insurance fee, not both.")
  }

  ev_multiplier <- if (.mode %in% c("PLDV", "AV", "DRS")) {
    as.integer(.stock %in% c("SIStock", "CIStock", "HEVStock"))
  }

  # targeted filters + joins
  get_pldv_stocks <- function() {
    keys <- c("geog_name", "geog_id", "year")
    si <- dplyr::filter(.pass_tb, mode == "PLDV", var == "SIStock") %>% dplyr::select(all_of(keys), SIStock = value)
    ci <- dplyr::filter(.pass_tb, mode == "PLDV", var == "CIStock") %>% dplyr::select(all_of(keys), CIStock = value)
    hev <- dplyr::filter(.pass_tb, mode == "PLDV", var == "HEVStock") %>% dplyr::select(all_of(keys), HEVStock = value)
    tot <- dplyr::filter(.pass_tb, mode == "PLDV", var == "TotStock") %>% dplyr::select(all_of(keys), TotStock = value)

    si %>%
      dplyr::inner_join(ci, by = keys) %>%
      dplyr::inner_join(hev, by = keys) %>%
      dplyr::inner_join(tot, by = keys)
  }

  switch(.mode,
    PLDV = {
      .tb_fuel_cost_mile %>%
        dplyr::left_join(.elast, by = "year") %>%
        dplyr::left_join(.tb_vmt, by = c("year", "mode")) %>%
        dplyr::mutate(
          fuel_time_cost_mile = fuel_cost_mile + .enviro_factors$TIME_COST_MI,
          payd_ins_adj        = .payd_fee / .enviro_factors$INS_COST_MI,
          vmt_fee_adj         = 1 + (.vmt_fee / (fuel_time_cost_mile + payd_ins_adj)) * vmt_elast,
          cong_adjust         = 1 + ((.cong_price / fuel_time_cost_mile) * .enviro_factors$CONG_VMT) * cong_elast,
          cross_vmt           = vmt_cross,
          gas_adj             = 1 + (.gas_tax / fuel_time_cost_mile) * ev_multiplier * gas_elast
        ) %>%
        dplyr::select(
          year, geog_name, geog_id, fuel_time_cost_mile, payd_ins_adj,
          vmt_fee_adj, cong_adjust, cross_vmt, gas_adj
        ) %>%
        dplyr::distinct() %>%
        return()
    },
    BU = ,
    BRT = ,
    RU = ,
    RI = {
      elast_vmt <- dplyr::select(.elast, year, vmt_elas = vmt_cross)

      .tb_fuel_cost_mile %>%
        dplyr::left_join(get_pldv_stocks(), by = "year") %>%
        dplyr::left_join(.tb_vmt, by = c("year", "geog_name", "geog_id")) %>%
        dplyr::left_join(elast_vmt, by = "year") %>%
        dplyr::mutate(
          fuel_time_cost_mile = fuel_cost_mile + .enviro_factors$TIME_COST_MI,
          payd_ins_adj        = .payd_fee / .enviro_factors$INS_COST_MI,
          vmt_fee_adj         = .vmt_fee / fuel_time_cost_mile,
          cong_adjust         = .cong_price / fuel_time_cost_mile * .enviro_factors$CONG_VMT,
          cross_vmt           = vmt_elas,
          stock_proportion    = (SIStock + CIStock + HEVStock) / TotStock,
          gas_adj             = 1 + ((.gas_tax / fuel_cost_mile) * stock_proportion * cross_vmt)
        ) %>%
        dplyr::select(
          year, geog_name, geog_id, fuel_time_cost_mile, payd_ins_adj,
          vmt_fee_adj, cong_adjust, cross_vmt, gas_adj
        ) %>%
        dplyr::distinct() %>%
        return()
    },
    AV = {
      .tb_fuel_cost_mile %>%
        dplyr::left_join(.elast, by = "year") %>%
        dplyr::left_join(.tb_vmt, by = c("year", "mode")) %>%
        dplyr::mutate(
          fuel_time_cost_mile = fuel_cost_mile + .enviro_factors$TIME_COST_MI,
          payd_ins_adj        = .payd_fee / .enviro_factors$INS_COST_MI,
          vmt_fee_adj         = 1 + ((.vmt_fee / fuel_time_cost_mile) + payd_ins_adj) * vmt_elast,
          cong_adjust         = 1 + (.cong_price / fuel_time_cost_mile) * .enviro_factors$CONG_VMT * cong_elast,
          cross_vmt           = vmt_cross,
          gas_adj             = 1 + (.gas_tax / fuel_time_cost_mile) * ev_multiplier * gas_elast
        ) %>%
        dplyr::select(
          year, geog_name, geog_id, fuel_time_cost_mile, payd_ins_adj,
          vmt_fee_adj, cong_adjust, cross_vmt, gas_adj
        ) %>%
        return()
    },
    SUT = ,
    CUT = {
      f_fract <- if (.mode == "SUT") .enviro_factors$F_FRACT else 1

      .tb_fuel_cost_mile %>%
        dplyr::left_join(dplyr::select(.elast, year, freight_vmt_elast), by = "year") %>%
        dplyr::mutate(
          fuel_time_cost_mile = fuel_cost_mile + .enviro_factors$F_TIME_COST_MI,
          vmt_fee_adj         = 1 + .freight_vmt_fee / fuel_time_cost_mile * freight_vmt_elast * f_fract
        ) %>%
        dplyr::select(year, vmt_fee_adj) %>%
        return()
    },
    DRS = {
      elast_vmt <- dplyr::select(.elast, year, vmt_elas = vmt_cross)

      .tb_fuel_cost_mile %>%
        dplyr::left_join(.elast, by = "year") %>%
        dplyr::left_join(elast_vmt, by = "year") %>%
        dplyr::left_join(get_pldv_stocks(), by = "year") %>%
        dplyr::mutate(
          fuel_time_cost_mile = fuel_cost_mile + .enviro_factors$TIME_COST_MI,
          payd_ins_adj        = .payd_fee / .enviro_factors$INS_COST_MI,
          stock_proportion    = (SIStock + CIStock + HEVStock) / TotStock,
          vmt_fee_cross_adj   = 1 + (.vmt_fee / fuel_time_cost_mile) + payd_ins_adj * vmt_cross,
          vmt_fee_elas_adj    = 1 + (.vmt_fee / fuel_time_cost_mile) + payd_ins_adj * vmt_elas,
          cong_adjust         = 1 + .cong_price / fuel_time_cost_mile * .enviro_factors$CONG_VMT * cong_elast,
          gas_adj             = 1 + .gas_tax / fuel_cost_mile * stock_proportion * vmt_cross * ev_multiplier
        ) %>%
        dplyr::select(year, geog_name, fuel_time_cost_mile, payd_ins_adj,
          vmt_fee_cross_adj, vmt_fee_elas_adj, cong_adjust,
          stock_proportion,
          cross_vmt = vmt_cross, gas_adj
        ) %>%
        return()
    },
    cli::cli_abort("No valid mode found for { .mode }")
  )
}

#' Calculate telework multiplier
#' @param .telework_pct additional percent of people teleworking in the final forecast year.
#'     Numeric between 0 and 1.
#'     Default is `r ghg.ccap::transportation_defaults$telework_pct`
#'      Default is `0`
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#' @export
#' @family VMT effects
#' @details
#' Only applicable for passenger light-duty vehicles (PLDV).
#'
#' \loadmathjax
#' \mjdeqn{GHG = \frac{PMT}{AVO \times FF} \times TW \times GF}{GHG = (PMT)/(AVO x FF) x TW x GF}
#'     where \eqn{GHG} is the impact in metric tons of CO2 equivalent,
#'     \eqn{PMT} is passenger miles traveled,
#'     \eqn{AVO} is the average vehicle occupancy,
#'     \eqn{FF} is the fuel factor representing consumption of fuel per mile of travel,
#'     \eqn{GF} is the greenhouse gas factor per unit of consumed fuel,
#'     and \eqn{TW} is an adjustment factor for the effect of telework on baseline PMT.
#'
vmt_telework <- function(.pass_tb,
                         .mode,
                         .telework_pct = ghg.ccap::transportation_defaults$telework_pct,
                         .enviro_factors = ghg.ccap::enviro_factors) {
  switch(.mode,
    PLDV = {
      tibble::tibble(
        year = unique(.pass_tb$year),
        telework_elast_val = calc_elasticity(
          elas_list = c(rep(0, length(unique(.pass_tb$year)))),
          elas = .telework_pct,
          num_inits = 3,
          num_yrs = length(unique(.pass_tb$year)) - 5
        )
      ) %>%
        dplyr::mutate(
          telework_elast_val = ifelse(
            year %in% c("2045", "2050") & telework_elast_val == 0,
            .telework_pct, telework_elast_val
          ),
          telework_adj = 1 + telework_elast_val * .enviro_factors$MARG_TELEWORK
        ) %>%
        dplyr::select(year, telework_adj) %>%
        return()
    },
    cli::cli_abort("Telework adjustment is only applicable for passenger light-duty vehicles")
  )
}


#' Calculate stock adjustment
#'
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#' @return table with columns `geog_name`, `geog_id`, `year`, `mode`, and `mode_stock_adj`
#' @export
#' @family VMT effects
#'
vmt_stock_proportion <- function(.tb,
                                 .mode,
                                 .stock) {
  switch(.mode,
    WALK = ,
    BIKE = {
      .tb %>%
        dplyr::filter(mode == .mode) %>%
        dplyr::mutate(mode_stock_adj = 1) %>%
        dplyr::select(year, geog_name, mode, mode_stock_adj) %>%
        dplyr::distinct() %>%
        return()
    },
    DRS = {
      stock_vals <- dplyr::filter(.tb, mode == "PLDV", var %in% c("SIStock", "CIStock", "HEVStock"))
      tot_vals <- dplyr::filter(.tb, mode == "PLDV", var == "TotStock") %>%
        dplyr::select(geog_name, geog_id, year, tot = value)

      stock_vals %>%
        dplyr::group_by(geog_name, geog_id, year) %>%
        dplyr::summarise(stock_sum = sum(value), .groups = "drop") %>%
        dplyr::inner_join(tot_vals, by = c("geog_name", "geog_id", "year")) %>%
        dplyr::transmute(geog_name, geog_id, year,
          mode = .mode,
          mode_stock_adj = stock_sum / tot
        ) %>%
        dplyr::distinct() %>%
        return()
    },
    {
      stock_vals <- dplyr::filter(.tb, mode == .mode, var == .stock) %>%
        dplyr::select(geog_name, geog_id, year, mode, stock = value)

      tot_vals <- dplyr::filter(.tb, mode == .mode, var == "TotStock") %>%
        dplyr::select(geog_name, geog_id, year, tot = value)

      tb_stock_proportion <- dplyr::inner_join(
        stock_vals, tot_vals,
        by = c("geog_name", "geog_id", "year")
      ) %>%
        dplyr::transmute(geog_name, geog_id, year, mode,
          mode_stock_adj = stock / tot
        ) %>%
        dplyr::distinct()

      if (nrow(tb_stock_proportion) == 0) {
        cli::cli_abort("No stock proportions available")
      }

      if (nrow(dplyr::filter(tb_stock_proportion, mode_stock_adj == 0, mode == "PLDV", year == max(year))) != 0) {
        cli::cli_abort("No stock proportions available")
      }

      return(tb_stock_proportion)
    }
  )
}


#' Calculate transit service adjustment for each forecast year
#' @param .transit_service_pct numeric, change in transit service % adjustment.
#'    Default is `r ghg.ccap::transportation_defaults$transit_service_pct`.
#'
#' @description
#'     Transit service increase assumes that the base level of
#'     transit service has increased (more lines, more buses,
#'     more frequency, etc). As opposed to
#'     `.transit_avo_pct`, an increase in transit service implies
#'     additional **vehicle** miles traveled.
#'
#'     `.transit_service_pct` indicates the overall effect by
#'     the final forecast year. The increase is spread evenly
#'     over the intermediate years.
#'
#'    - If `.mode` is `"PLDV"` or `"AV"`, value returned is
#'        the number of passenger vehicle miles traveled
#'        decreased when transit service
#'        is increased (i.e., 7,000 passenger VMT).
#'    - If `.mode` is `"BS"`, `"BU"`, `"BRT"`, `"RU"`, or `"RI"`, value returned
#'        is the proportion of increase in transit VMT (i.e., 1.2).
#'
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#' @return a table with columns `year`, `geog_name`, and `transit_adj`.
#' @export
#' @family VMT effects
vmt_transit_service <- function(tb,
                                .mode,
                                .transit_service_pct = ghg.ccap::transportation_defaults$transit_service_pct,
                                .elast = ghg.ccap::elast,
                                .enviro_factors = ghg.ccap::enviro_factors) {
  years <- unique(tb$year)
  n_years <- length(years)

  transit_service_elast <- tibble::tibble(
    year = years,
    elast_new = calc_elasticity(
      elas_list = rep(0, n_years),
      elas      = .transit_service_pct,
      num_inits = 3,
      num_yrs   = n_years - 5
    )
  ) %>%
    dplyr::mutate(
      elast_new = dplyr::if_else(year %in% c("2045", "2050") & elast_new == 0, .transit_service_pct, elast_new)
    )

  additional_transit <- tb %>%
    dplyr::filter(mode == "AT", var == "PMT") %>%
    dplyr::select(geog_name, geog_id, year, all_transit = value) %>%
    dplyr::left_join(transit_service_elast, by = "year") %>%
    dplyr::mutate(all_transit_plus = all_transit * elast_new)

  tb_base <- tb %>%
    dplyr::select(year, geog_name, geog_id) %>%
    dplyr::distinct()

  switch(.mode,
    PLDV = ,
    AV = {
      tb_base %>%
        dplyr::left_join(additional_transit, by = c("year", "geog_name", "geog_id")) %>%
        dplyr::mutate(transit_adj = all_transit_plus * .enviro_factors$TRANSIT_SERVICE_ELAST) %>%
        dplyr::select(year, geog_name, geog_id, transit_adj) %>%
        dplyr::distinct() %>%
        return()
    },
    BU = ,
    BRT = ,
    RU = ,
    RI = {
      tb_base %>%
        dplyr::left_join(transit_service_elast, by = "year") %>%
        dplyr::mutate(transit_adj = 1 + elast_new) %>%
        dplyr::select(year, geog_name, geog_id, transit_adj) %>%
        dplyr::distinct() %>%
        return()
    }
  )
}

#' Calculate vehicle occupancy multiplier
#'
#' @param .transit_avo_pct numeric, transit average vehicle occupancy (AVO) % adjustment.
#'   Default is `r ghg.ccap::transportation_defaults$transit_avo_pct`.
#' @param .pldv_avo_pct numeric, passenger light duty vehicle occupancy adjustment.
#'   Default is `r ghg.ccap::transportation_defaults$pldv_avo_pct`.
#' @param .vehicle_occupancy table, vehicle occupancy averages by mode.
#'   Default is `ghg.ccap::vehicle_occupancy`.
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#' @inheritParams vmt_road_policy
#'
#' @return table with columns `geog_name`, `geog_id`, `year`, `occupancy_adj`
#' @export
#'
#'
#' @description
#'     Transit vehicle occupancy increase assumes that
#'     more people are riding transit. As opposed to
#'     `.transit_service_pct`, an increase in occupancy implies
#'     additional **passenger** miles traveled, with no implied
#'     change in vehicle miles traveled.
#'
#'     Passenger vehicle occupancy increase assumes that
#'     the average number of people in a car increases.
#'     An increase in  passenger occupancy implies
#'     additional **passenger** miles traveled, with no implied
#'     change in vehicle miles traveled.
#'
#'     `.transit_avo_pct` and `.pldv_avo_pct` indicate
#'     the overall effect by the final forecast year.
#'     The total increase is spread evenly
#'     over the intermediate years.
#' @details
#'
#' \loadmathjax
#' \mjdeqn{GHG = \frac{PMT}{AVO \times FF} \times AV \times GF}{GHG = (PMT)/(AVO x FF) x AV x GF}
#'     where \eqn{GHG} is the impact in metric tons of CO2 equivalent,
#'     \eqn{PMT} is passenger miles traveled,
#'     \eqn{AVO} is the average vehicle occupancy,
#'     \eqn{FF} is the fuel factor representing consumption of fuel per mile of travel,
#'     \eqn{GF} is the greenhouse gas factor per unit of consumed fuel,
#'     and \eqn{AV} is an adjustment factor for the effect of introducing vehicle automation on VMT by mode
#' @family VMT effects
#' @importFrom dplyr right_join
#' @importFrom cli cli_warn
vmt_vehicle_occupancy <- function(tb,
                                  .tb_vmt,
                                  .mode,
                                  .stock,
                                  .transit_avo_pct = ghg.ccap::transportation_defaults$transit_avo_pct,
                                  .pldv_avo_pct = ghg.ccap::transportation_defaults$pldv_avo_pct,
                                  .vehicle_occupancy = ghg.ccap::vehicle_occupancy,
                                  .enviro_factors = ghg.ccap::enviro_factors) {
  calc_avo_elast <- function(pct) {
    tibble::tibble(
      year = unique(tb$year),
      avo_elast = calc_elasticity(
        elas_list = c(rep(0, length(unique(tb$year)))),
        elas      = pct,
        num_inits = 3,
        num_yrs   = length(unique(tb$year)) - 5
      )
    ) %>%
      dplyr::mutate(
        avo_elast = ifelse(year %in% c("2045", "2050") & avo_elast == 0, pct, avo_elast)
      )
  }

  # filtering to var == "AVO" already gives one row per
  # geog/year, so we can rename value directly
  get_mode_avo <- function(geog_names) {
    .vehicle_occupancy %>%
      filter_ctu(geog_names) %>%
      dplyr::filter(mode == .mode, var == "AVO") %>%
      dplyr::select(mode, geog_name, geog_id, mode_avo = value) %>%
      dplyr::distinct()
  }

  switch(.mode,
    PLDV = {
      pldv_occupancy <- .vehicle_occupancy %>%
        filter_ctu(unique(.tb_vmt$geog_name)) %>%
        dplyr::filter(mode == .mode, var == "AVO") %>%
        dplyr::select(geog_name, geog_id, occupancy_adj = value) %>%
        dplyr::distinct()

      calc_avo_elast(.pldv_avo_pct) %>%
        dplyr::cross_join(pldv_occupancy) %>%
        dplyr::mutate(occupancy_adj = occupancy_adj * (1 + avo_elast)) %>%
        return()
    },
    BU = ,
    BRT = ,
    RU = ,
    RI = {
      get_mode_avo(unique(tb$geog_name)) %>%
        dplyr::left_join(.tb_vmt, by = c("geog_name", "geog_id", "mode")) %>%
        dplyr::left_join(calc_avo_elast(.transit_avo_pct), by = "year") %>%
        dplyr::mutate(occupancy_adj = mode_avo * (1 + avo_elast)) %>%
        dplyr::select(geog_name, geog_id, year, occupancy_adj) %>%
        dplyr::distinct() %>%
        return()
    },
    BS = ,
    FR = ,
    SUT = ,
    CUT = ,
    MM = ,
    AIR = ,
    WAT = {
      if (.transit_avo_pct != 0) {
        cli::cli_warn("Occupancy has no effect on school bus or freight modes")
      }

      get_mode_avo(unique(tb$geog_name)) %>%
        dplyr::left_join(.tb_vmt, by = c("geog_name", "geog_id", "mode")) %>%
        dplyr::mutate(occupancy_adj = mode_avo) %>%
        dplyr::select(geog_name, geog_id, year, occupancy_adj) %>%
        dplyr::distinct() %>%
        return()
    }
  )
}


#' Calculate VMT reduction multiplier
#' @param .vmt_reduction_pct total percent reduction in PLDV VMT by final forecast year.
#'     Numeric between 0 and 1.
#'     Default is `r ghg.ccap::transportation_defaults$vmt_reduction_pct`.
#' @inheritParams run_module_transportation
#' @inheritParams calc_vmt_forecast
#' @inheritParams filter_ctu
#'
#' @export
#' @family VMT effects
#' @details
#' Only applicable for passenger light-duty vehicles (PLDV).
#' This should not be used in conjunction with any other adjustments that would affect VMT.
#' Unlike the other VMT effects, this does not use any elasticity calculations. Instead, it
#' applies a straight percentage reduction to VMT in the final forecast year, with a gradual
#' increase in the reduction over the intermediate years. The percentage reduction is applied directly
#' to VMT (i.e., a 10% reduction would be an adjustment factor of 0.10). There are also no limits
#' on the amount of reduction and no change in other modes (mode shift).
#'
#'
vmt_total_reduction <- function(.pass_tb,
                                .mode,
                                .vmt_reduction_pct = ghg.ccap::transportation_defaults$vmt_reduction_pct,
                                .enviro_factors = ghg.ccap::enviro_factors) {
  switch(.mode,
    PLDV = {
      tibble::tibble(
        year = unique(.pass_tb$year),
        vmt_total_reduction_val = calc_elasticity(
          elas_list = c(rep(0, length(unique(.pass_tb$year)))),
          elas      = .vmt_reduction_pct,
          num_inits = 3,
          num_yrs   = length(unique(.pass_tb$year)) - 5
        )
      ) %>%
        dplyr::mutate(
          vmt_total_reduction_val = ifelse(
            year %in% c("2045", "2050") & vmt_total_reduction_val == 0,
            .vmt_reduction_pct, vmt_total_reduction_val
          ),
          vmt_reduction_adj = 1 - vmt_total_reduction_val
        ) %>%
        dplyr::select(year, vmt_reduction_adj) %>%
        return()
    },
    cli::cli_abort("VMT reduction adjustment is only applicable for passenger light-duty vehicles")
  )
}


#' @title Trip reduction program VMT VMT adjustment
#'
#' @description
#'
#' Community Based Travel Planning (CBTP) is a transportation demand management strategy
#' that provides personalized travel behavior change assistance to households in a target
#' community. This function calculates the expected reduction in vehicle miles traveled (VMT)
#' based on two key elasticities: the proportion of targeted households that participate and
#' the vehicle trip reduction achieved by participating households.
#'
#' The reduction is calculated as: `percent_reduction = -(prop_targeted × participation_rate × trip_reduction_rate)`,
#' with a maximum VMT reduction cap of 2.3%.
#'
#' @param .pass_tb tibble, baseline passenger transportation table with columns `year`
#' @param .cbtp_prop_targeted numeric, proportion of households targeted with CBTP (0 to 1).
#'     Default is `r ghg.ccap::transportation_defaults$cbtp_prop_targeted`
#' @param .cbtp_start_year character or numeric, the year the CBTP strategy begins. For years prior to this,
#'     no reduction is applied. For years at or after this year, the full reduction is applied.
#'     Default is `r ghg.ccap::transportation_defaults$cbtp_start_year`
#' @param .enviro_factors list, environmental factors including CBTP elasticities. Default is `ghg.ccap::enviro_factors`.
#'     Expected to contain:
#'     - `CBTP_PARTICIPATION_PCT`: proportion of targeted residences that participate (default 0.19)
#'     - `CBTP_TRIP_REDUCTION_PCT`: vehicle trip reduction by participating residences (default 0.12)
#'     - `MAX_TRIP_REDUCTION_PCT`: maximum trip VMT reduction cap (default 0.023, i.e., 2.3%)
#'
#' @return tibble with columns `year` and `cbtp_adj`, where `cbtp_adj` is the adjustment factor
#'     (1 = no effect, < 1 = reduction). For years before `.cbtp_start_year`, `cbtp_adj = 1`.
#'     For years at or after `.cbtp_start_year`, `cbtp_adj = 1 + reduction_pct` where
#'     `reduction_pct` is capped at `-0.023` (maximum 2.3% reduction).
#'
#' @details
#' \loadmathjax
#' \mjdeqn{cbtp\_reduction = -prop\_targeted \times participation \times trip\_reduction}{cbtp_reduction = -prop_targeted × participation × trip_reduction}
#'
#' where \eqn{cbtp\_reduction} is capped at \eqn{-0.023} (maximum 2.3% VMT reduction).
#'
#' **Example calculations:**
#' - `prop_targeted = 0.5`, participation = 0.19, trip_reduction = 0.12:
#'   reduction = -(0.5 × 0.19 × 0.12) = -0.0114 (−1.14% uncapped)
#' - `prop_targeted = 1.0`, participation = 0.19, trip_reduction = 0.12:
#'   reduction = -(1.0 × 0.19 × 0.12) = -0.0228, capped to -0.023 (−2.3%)
#'
#' @export
#' @family transportation
#' @family VMT effects
#'
#' @references
#' CAPCOA Handbook. Community Based Travel Planning strategies for reducing VMT.
#'
#' @importFrom dplyr tibble mutate select case_when
#' @importFrom tibble tibble
vmt_trip_reduction <- function(.pass_tb,
                               .cbtp_prop_targeted = ghg.ccap::transportation_defaults$cbtp_prop_targeted,
                               .cbtp_start_year = ghg.ccap::transportation_defaults$cbtp_start_year,
                               .enviro_factors = ghg.ccap::enviro_factors) {
  if (.cbtp_prop_targeted == 0) {
    # browser()
    return(.pass_tb %>%
      select(geog_id, geog_name, year) %>%
      distinct() %>%
      mutate(
        households_cbtp = 0,
        cbtp_adj = 1
      ))
  }

  # Extract elasticity values from enviro_factors
  # browser()
  participation_pct <- .enviro_factors$CBTP_PARTICIPATION_PCT
  trip_reduction_pct <- .enviro_factors$CBTP_TRIP_REDUCTION_PCT
  max_reduction_pct <- .enviro_factors$MAX_TRIP_REDUCTION_PCT

  households_community <- ghg.ccap::demographic_data %>%
    filter_ctu(unique(.pass_tb$geog_name)) %>%
    dplyr::filter(
      sp_categories == "total_households",
      inventory_year %in% .pass_tb$year
    ) %>%
    dplyr::select(geog_name, geog_id, year = inventory_year, households = value) %>%
    mutate(
      households_cbtp = (households * .cbtp_prop_targeted) * participation_pct,
      uncapped_reduction = 1 + (.cbtp_prop_targeted * participation_pct * trip_reduction_pct),
      capped_reduction = dplyr::case_when(
        uncapped_reduction < (1 + max_reduction_pct) ~ 1 + max_reduction_pct,
        TRUE ~ uncapped_reduction
      ),
      cbtp_adj = dplyr::case_when(
        as.numeric(year) < as.numeric(.cbtp_start_year) ~ 1,
        TRUE ~ capped_reduction
      ),
      year = as.character(year)
    ) %>%
    select(geog_id, geog_name, year, households_cbtp, cbtp_adj)

  return(households_community)
}

#' @title Commute trip reduction (CTR) program VMT adjustment
#'
#' @description
#' This function calculates the expected reduction in vehicle miles traveled (VMT) based on a commute trip reduction program.
#' The reduction is calculated using the proportion of employees in a given geographic unit targeted for the program,
#' whether the program is voluntary or mandatory, and the year the program begins.
#' The reduction is capped at a maximum percentage defined in `enviro_factors`.
#'
#' This function does not account for associated increases in transit or active transportation VMT that may result
#' from the commute trip reduction program.
#'
#' @param .ctr_employees_targeted numeric, proportion of employees targeted for commute trip reduction (0 to 1).
#'     Default is `r ghg.ccap::transportation_defaults$ctr_employees_targeted`.
#' @param .ctr_voluntary logical, whether the trip reduction program is voluntary or mandatory with monitoring.
#'     Default is `r ghg.ccap::transportation_defaults$ctr_voluntary`.
#' @param .ctr_start_year character or numeric, the year the trip reduction program begins. For years prior to this,
#'     no reduction is applied. For years at or after this year, the full reduction is applied.
#'     Default is `r ghg.ccap::transportation_defaults$ctr_start_year`.
#' @param .commute_vmt_proportion table, proportion of passenger light-duty vehicle VMT that is commute-related.
#'     Default is `ghg.ccap::commute_vmt_proportion`.
#' @export
vmt_commute_trip_reduction <- function(.pass_tb,
                                       .ctr_employees_targeted = ghg.ccap::transportation_defaults$ctr_employees_targeted,
                                       .ctr_voluntary = ghg.ccap::transportation_defaults$ctr_voluntary,
                                       .ctr_start_year = ghg.ccap::transportation_defaults$ctr_start_year,
                                       .commute_vmt_proportion = ghg.ccap::commute_vmt_proportion,
                                       .enviro_factors = ghg.ccap::enviro_factors) {
  if (.ctr_employees_targeted == 0) {
    return(.pass_tb %>%
      select(geog_id, geog_name, year) %>%
      distinct() %>%
      mutate(
        commute_trip_reduction_adj = 1
      ))
  }

  max_reduction_pct <- .enviro_factors$MAX_COMMUTE_TRIP_REDUCTION_PCT
  vmt_proportion <- .commute_vmt_proportion %>%
    filter_ctu(unique(.pass_tb$geog_name)) %>%
    dplyr::select(geog_id, geog_name, vmt_proportion = value) %>%
    pull("vmt_proportion")

  trip_reduction_percent <- if (.ctr_voluntary) {
    .enviro_factors$COMMUTE_TRIP_REDUCTION_VOLUNTARY_PCT
  } else {
    .enviro_factors$COMMUTE_TRIP_REDUCTION_MANDATORY_PCT
  }

  commute_trip_reduction <- .pass_tb %>%
    select(geog_id, geog_name, year) %>%
    distinct() %>%
    mutate(
      uncapped_reduction = 1 + (.ctr_employees_targeted * trip_reduction_percent),
      capped_reduction = dplyr::case_when(
        uncapped_reduction < (1 + max_reduction_pct) ~ 1 + max_reduction_pct,
        TRUE ~ uncapped_reduction
      ),
      # Scale commute-only reduction to total VMT: only the commute share is affected
      commute_trip_reduction_adj = dplyr::case_when(
        as.numeric(year) < as.numeric(.ctr_start_year) ~ 1,
        TRUE ~ 1 + (capped_reduction - 1) * vmt_proportion
      )
    )

  return(commute_trip_reduction)
}
