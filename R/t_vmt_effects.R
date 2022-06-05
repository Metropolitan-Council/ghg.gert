#' @title  Calculate strategy effects on vehicle miles traveled.

#' Calculate annual energy outlook (AEO) multipliers for each forecast year
#'
#' @return a table with columns `aeo_scen`, `metric`, `mode`, `year`, and `aeo_adj`.
#' @export
#' @family VMT effects
#' @importFrom tidyr pivot_wider
vmt_annual_energy_outlook <- function(tb,
                                      .mode,
                                      .aeo_scenario,
                                      .enviro_factors = enviro_factors) {
  check_inputs(
    name = "aeo_scenario",
    value = .aeo_scenario
  )

  # browser()
  tb_fin <- tb %>%
    filter(mode == .mode) %>%
    unique() %>%
    tidyr::pivot_wider(
      names_from = var,
      values_from = value,
    )

  aeo_vals <- factor_values$aeo %>%
    dplyr::filter(
      aeo_scen == .aeo_scenario,
      metric == "VMT",
      mode == unique(tb_fin$aeo_mode)
    ) %>%
    select(
      year,
      metric,
      # everything(),
      aeo_adj = value
    )
  return(aeo_vals)
}



#' Calculate autonomous vehicle multiplier
#'
#' @param .av_pct percent of trips made by AV. Default is `0`

#' @return table with columns `year`, `ctu`, `av_adj`
#' @export
#'
#' @family VMT effects
#' @details
#' \loadmathjax
#' \mjdeqn{GHG = \frac{PMT}{AVO \times FF} \times AV \times GF}{GHG = (PMT)/(AVO x FF) x AV x GF}
#'     where \eqn{GHG} is the impact in metric tons of CO2 equivalent,
#'     \eqn{PMT} is passenger miles traveled,
#'     \eqn{AVO} is the average vehicle occupancy,
#'     \eqn{FF} is the fuel factor representing consumption of fuel per mile of travel,
#'     \eqn{GF} is the greenhouse gas factor per unit of consumed fuel,
#'     and \eqn{AV} is an adjustment factor for the effect of introducing vehicle automation on VMT by mode
vmt_autonomous_vehicle <- function(.pass_tb = transportation_data$passenger,
                                   .av_pct,
                                   .mode,
                                   .enviro_factors = enviro_factors) {
  tb_avshare <- .pass_tb %>%
    dplyr::filter(var == "AVShare") %>%
    dplyr::select(year, ctu, av_share = value) %>%
    dplyr::distinct()

  if (nrow(tb_avshare) == 0) {
    stop("Make sure you are using the correct input table")
  }

  if (.mode == "PLDV") {
    # increase in AV usage will increase PLDV VMT
    av_return <- tb_avshare %>%
      rowwise() %>%
      mutate(av_adj = case_when(
        .av_pct > 0 ~ 1 - (av_share * .av_pct),
        TRUE ~ 1
      )) %>%
      select(year, ctu, av_adj) %>%
      ungroup()

    return(av_return)
  } else if (.mode %in% c(
    "BU",
    "BRT",
    "RU",
    "RI"
  )) {
    # increase in AV usage will decrease transit VMT
    av_return <- .pass_tb %>%
      # rowwise() %>%
      mutate(
        av_adj = dplyr::case_when(
          year %in% c("2015", "2018", "2020") ~ 1,
          (((.mode == "BU") | (.mode == "BRT")) & .av_pct > 0) ~
          1 + .enviro_factors$BUS_AV * .av_pct,
          (((.mode == "RU") | (.mode == "RI")) & .av_pct > 0) ~
          1 + .enviro_factors$RAIL_AV * .av_pct,
          TRUE ~ 1
        )
      ) %>%
      select(year, ctu, av_adj) %>%
      unique() %>%
      ungroup()

    return(av_return)
  } else if (.mode == "AV") {
    # browser()
    av_return <- tb_avshare %>%
      mutate(av_adj = case_when(
        .av_pct > 0 ~ av_share * .av_pct,
        TRUE ~ 1
      )) %>%
      select(year, ctu, av_adj) %>%
      ungroup()

    return(av_return)
  }
}

#' Calculate dynamic ride sharing effect
#'
#' @return
#' @export
#' @details
#'
#' \loadmathjax
#' \mjdeqn{GHG = \frac{PMT}{AVO \times FF} \times GF}{GHG = (PMT)/(AVO x FF) x GF}
#'     where \eqn{GHG} is the impact in metric tons of CO2 equivalent,
#'     \eqn{PMT} is passenger miles traveled,
#'     \eqn{AVO} is the average vehicle occupancy,
#'     \eqn{FF} is the fuel factor representing consumption of fuel per mile of travel measured in kilowatt hours per mile
#'     \eqn{GF} is the greenhouse gas factor per unit of consumed fuel,
#'     and \eqn{AV} is an adjustment factor for the effect of introducing vehicle automation on VMT by mode
#'
#' @family VMT effects
vmt_dynamic_ride_share_reduction <- function(.pass_tb = transportation_data$passenger,
                                             .drs_pct,
                                             .enviro_factors = enviro_factors) {
  if (.drs_pct > 0) {
    # browser()

    drs_share_tb <- .pass_tb %>%
      filter(var == "DRSShare") %>%
      select(year, ctu, drs_share = value)

    .pass_tb <- .pass_tb %>%
      left_join(drs_share_tb, by = c("year", "ctu")) %>%
      mutate(value = case_when(
        var == "PMT" & mode == "PLDV" ~ value * (1 - drs_share * .drs_pct),
        TRUE ~ value
      )) %>%
      select(names(.pass_tb))

    return(.pass_tb)
  } else {
    return(.pass_tb)
  }
}

#' Calculate combined 5D land use change impact
#'
#' @inheritParams calc_vmt_forecast
#' @param .type character, one of `"DRIVE"`, `"WALK"`, `"TRANSIT"`.
#' @param .pop_dens_pct_change percent change in population density in the final forecast year relative to BAU.
#'     Numeric between -1 and 1. Default is `0`
#' @param .emp_dens_pct_change percent change in employment density in the final forecast year relative to BAU.
#'      Numeric between -1 and 1. Default is `0`
#' @param .land_use_diversity_pct_change percent change in land use diversity/mix in the final forecast year relative to BAU.
#'      Numeric between -1 and 1. Default is `0`
#' @param .intersection_design_pct_change percent change in intersection design
#'     (% 4-way stops) in the final forecast year relative to BAU.
#'     Numeric between -1 and 1.  Default is `0`
#' @param .job_access_pct_change percent change in job accessibility in the final forecast year relative to BAU.
#'     Numeric between -1 and 1. Default is `0`
#' @param .transit_dist_pct_change percent change in transit distance in the final forecast year relative to BAU.
#'     Numeric between -1 and 1. Default is `0`
#' @param .comb_5d_impact_pct_change percent change in population density in the final forecast year relative to BAU
#'      as a measure of composite change in 5Ds on VMT. Numeric between -1 and 1. Default is `0`
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
                                .comb_5d_impact_pct_change,
                                .pop_dens_pct_change,
                                .emp_dens_pct_change,
                                .land_use_diversity_pct_change,
                                .intersection_design_pct_change,
                                .job_access_pct_change,
                                .transit_dist_pct_change,
                                .enviro_factors = enviro_factors,
                                .elast_5d = elast_5d) {
  if (!.type %in% c("WALK", "DRIVE", "TRANSIT")) {
    stop(".type must be one of 'WALK', 'DRIVE', or 'TRANSIT'. ")
  }
  # browser()
  max_value <- if (.type == "DRIVE") {
    .enviro_factors$MAX_5D_DR
  } else if (.type == "TRANSIT") {
    .enviro_factors$MAX_5D_TRANS
  } else {
    .enviro_factors$MAX_5D_ACT
  }

  comb_5d_elast <- .elast_5d %>%
    filter(type == .type) %>%
    mutate(
      n_population_density = 1 + .pop_dens_pct_change * .data$population_density,
      n_employment_density = 1 + .emp_dens_pct_change * .data$employment_density,
      n_diversity = 1 + .land_use_diversity_pct_change * .data$diversity,
      n_design = 1 + .intersection_design_pct_change * .data$design,
      n_job_access = 1 + .job_access_pct_change * .data$job_access,
      n_distance = 1 + .transit_dist_pct_change * .data$distance,
      n_combined_density = 1 + .pop_dens_pct_change * .data$combined_density
    ) %>%
    mutate(
      product_all =
        .data$n_population_density *
          .data$n_employment_density *
          .data$n_diversity *
          .data$n_design *
          .data$n_job_access *
          .data$n_distance *
          .data$n_combined_density
    ) %>%
    rowwise() %>%
    mutate(
      land_use_adj = ifelse(product_all < max_value,
        1 + max_value,
        product_all
      ),
      land_use_adj = ifelse(land_use_adj == 0, 1,
        land_use_adj
      )
    ) %>%
    select(year, land_use_adj)

  return(comb_5d_elast)
}

#' Calculate parking price effect for passenger light-duty vehicles (PLDV) for each forecast year
#'
#' @param .parking_price measured in dollars per hour. Default is `0`.
#' @param .freight_parking_price measured in dollars per hour. Default is `0`.
#' @inheritParams calc_vmt_forecast
#' @return a table with
#' @export
#' @details
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
#' @importFrom tidyr pivot_wider
vmt_parking_policy <- function(tb,
                               .mode,
                               .elast = elast,
                               .parking_price = 0,
                               .freight_parking_price = 0,
                               .enviro_factors = enviro_factors) {


  # current parking prices
  park_price_current <- tb %>%
    filter(
      var %in% c(
        "PARK"
      )
    ) %>%
    unique() %>%
    tidyr::pivot_wider(
      names_from = var,
      values_from = value
    )


  if (!.mode %in% unique(tb$mode)) {
    stop("Make sure you are using the correct input table")
  }

  if (.mode %in% c(
    "PLDV",
    "AV"
  )) {
    park_return <- park_price_current %>%
      left_join(.elast %>%
        select(year, park_elast),
      by = "year"
      ) %>%
      mutate(
        park_price_adj =
          1 + .parking_price / PARK * park_elast
        # park_price_adj = ifelse(is.na(park_price_adj), 1, park_price_adj)
      ) %>%
      select(year, ctu, park_price_adj)
  } else if (.mode %in% c(
    "BU",
    "BRT",
    "RU",
    "RI",
    "DRS"
  )) {
    park_return <- park_price_current %>%
      left_join(.elast %>%
        select(year, park_transit),
      by = "year"
      ) %>%
      mutate(
        park_price_adj =
          1 + .parking_price / PARK * park_transit
      ) %>%
      select(year, ctu, park_price_adj)
  } else if (.mode == "SUT") {
    # browser()

    park_return <- park_adj <- park_price_current %>%
      left_join(.elast %>%
        select(year, park_elast), by = "year") %>%
      mutate(park_price_adj = 1 + .freight_parking_price / PARK * park_elast) %>%
      select(year, ctu, park_price_adj)
  }

  return(park_return)
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
#' @inheritParams calc_vmt_forecast
#' @param .vmt_fee VMT fee in dollars per mile. Default is `0`
#' @param .payd_fee  Pay-as-you-drive (PAYD) insurance fee in dollars per mile.
#'      Default is `0`
#' @param .gas_tax Gas tax tax in dollars per mile. Default is `0`
#' @param .cong_price Congestion price in dollars per mile (only applies to an approximation of
#'     congested miles in MSP). Default is `0`
#' @param .freight_vmt_fee freight VMT fee per mile. Default is `0`
#'
#' @return a table with columns   `year`, `ctu`, `fuel_time_cost_mile`, `payd_ins_adj`,
#'    `vmt_fee_adj`, `cong_adjust`, `cross_vmt`, `gas_adj`
#'
#' @export
#' @family VMT effects
vmt_road_policy <- function(.pass_tb,
                            .tb_vmt,
                            .mode,
                            .tb_fuel_cost_mile,
                            .vmt_fee,
                            .freight_vmt_fee,
                            .cong_price,
                            .gas_tax,
                            .payd_fee,
                            .stock,
                            .phev_electric = FALSE,
                            .enviro_factors = enviro_factors,
                            .elast = elast) {
  if (.vmt_fee > 0 & .payd_fee > 0) {
    stop("Implement a VMT fee OR a pay-as-you drive insurance fee, not both.")
  }

  if (.mode == "PLDV") {
    # browser()
    ev_multiplier <- ifelse(
      (.stock %in% c(
        "SIStock",
        "CIStock",
        "HEVStock"
      ) | (.stock == "PHEVStock" & .phev_electric == FALSE)
      ), 1, 0
    )


    fc_return <- .tb_fuel_cost_mile %>%
      dplyr::left_join(.elast, by = "year") %>%
      dplyr::left_join(.tb_vmt, by = c("year", "mode")) %>%
      rowwise() %>%
      dplyr::mutate(
        fuel_time_cost_mile = fuel_cost_mile + .enviro_factors$TIME_COST_MI,
        payd_ins_adj = .payd_fee / .enviro_factors$INS_COST_MI,
        vmt_fee_adj = 1 + (.vmt_fee / (fuel_time_cost_mile + payd_ins_adj)) * vmt_elast,
        cong_adjust = 1 + ((.cong_price / fuel_time_cost_mile) *
          .enviro_factors$CONG_VMT) * cong_elast,
        cross_vmt = vmt_cross,
        gas_adj = 1 + (.gas_tax / fuel_time_cost_mile) * ev_multiplier * gas_elast
      ) %>%
      dplyr::select(
        year, ctu, fuel_time_cost_mile, payd_ins_adj,
        vmt_fee_adj, cong_adjust, cross_vmt, gas_adj
      ) %>%
      unique()

    return(fc_return)
  } else if (.mode %in% c(
    "BU",
    "BRT",
    "RU",
    "RI"
  )) {
    # browser()
    pldv_stocks <- .pass_tb %>%
      filter(
        mode == "PLDV",
        var %in% c(
          "SIStock",
          "CIStock",
          "HEVStock",
          "TotStock"
        )
      ) %>%
      unique() %>%
      tidyr::pivot_wider(
        names_from = var,
        values_from = value,
      ) %>%
      mutate(stock_proportion = (SIStock + CIStock + HEVStock) / TotStock)


    elast_vmt <- .elast %>%
      select(year, vmt_elas = vmt_cross)

    fc_return <- .tb_fuel_cost_mile %>%
      # select(-ctu, -var) %>%
      left_join(pldv_stocks, by = c("year", "mode")) %>%
      left_join(.tb_vmt, by = c("year", "ctu", "type")) %>%
      left_join(elast_vmt, by = "year") %>%
      mutate(
        fuel_time_cost_mile = fuel_cost_mile + .enviro_factors$TIME_COST_MI,
        payd_ins_adj = .payd_fee / .enviro_factors$INS_COST_MI,
        vmt_fee_adj = .vmt_fee / fuel_time_cost_mile,
        cong_adjust = .cong_price / fuel_time_cost_mile * .enviro_factors$CONG_VMT,
        cross_vmt = vmt_elas,
        # gas adjustment is based on PLDV, gas consuming vehicles
        gas_adj = 1 + ((.gas_tax / fuel_cost_mile) * stock_proportion * cross_vmt)
      ) %>%
      select(
        year, ctu, fuel_time_cost_mile, payd_ins_adj,
        vmt_fee_adj, cong_adjust,
        cross_vmt, gas_adj
      ) %>%
      unique()

    return(fc_return)
  } else if (.mode == "AV") {
    ev_multiplier <- ifelse(
      (.stock %in% c(
        "SIStock",
        "CIStock",
        "HEVStock"
      ) | (.stock == "PHEVStock" & .phev_electric == FALSE)
      ), 1, 0
    )


    fc_return <- .tb_fuel_cost_mile %>%
      # select(-ctu) %>%
      left_join(.elast, by = "year") %>%
      left_join(.tb_vmt, by = c("year", "mode")) %>%
      mutate(
        fuel_time_cost_mile = fuel_cost_mile + .enviro_factors$TIME_COST_MI,
        payd_ins_adj = .payd_fee / .enviro_factors$INS_COST_MI,
        vmt_fee_adj = 1 + ((.vmt_fee / fuel_time_cost_mile) + payd_ins_adj) * vmt_elast,
        cong_adjust = 1 + (.cong_price / fuel_time_cost_mile) * .enviro_factors$CONG_VMT * cong_elast,
        cross_vmt = vmt_cross,
        gas_adj = 1 + (.gas_tax / fuel_time_cost_mile) * ev_multiplier * gas_elast
      ) %>%
      select(
        year, ctu, fuel_time_cost_mile, payd_ins_adj,
        vmt_fee_adj, cong_adjust, cross_vmt, gas_adj
      )
  } else if (.mode == "SUT") {
    fc_return <- .tb_fuel_cost_mile %>%
      left_join(.elast %>%
        select(year, freight_vmt_elast),
      by = "year"
      ) %>%
      mutate(
        fuel_time_cost_mile = fuel_cost_mile + .enviro_factors$F_TIME_COST_MI,
        vmt_fee_adj = 1 + .freight_vmt_fee / fuel_time_cost_mile *
          freight_vmt_elast * .enviro_factors$F_FRACT
      ) %>%
      select(year, vmt_fee_adj)

    return(fc_return)
  } else if (.mode == "CUT") {
    # browser()
    fc_return <- .tb_fuel_cost_mile %>%
      left_join(.elast %>%
        select(year, freight_vmt_elast),
      by = "year"
      ) %>%
      mutate(
        fuel_time_cost_mile = fuel_cost_mile + .enviro_factors$F_TIME_COST_MI,
        vmt_fee_adj = 1 + .freight_vmt_fee / fuel_time_cost_mile * freight_vmt_elast
      ) %>%
      select(year, vmt_fee_adj)

    return(fc_return)
  } else if (.mode == "DRS") {
    # browser()
    ev_multiplier <- ifelse(
      (.stock %in% c(
        "SIStock",
        "CIStock",
        "HEVStock"
      ) | (.stock == "PHEVStock" & .phev_electric == FALSE)
      ), 1, 0
    )

    # browser()

    pldv_stocks <- .pass_tb %>%
      filter(
        mode == "PLDV",
        var %in% c(
          "SIStock",
          "CIStock",
          "HEVStock",
          "TotStock"
        )
      ) %>%
      unique() %>%
      tidyr::pivot_wider(
        names_from = var,
        values_from = value,
      )

    elast_vmt <- .elast %>%
      select(year, vmt_elas = vmt_cross)


    tb_fin <-
      .tb_fuel_cost_mile %>%
      left_join(.elast, by = "year") %>%
      left_join(elast_vmt, by = c("year")) %>%
      left_join(pldv_stocks, by = c("year")) %>%
      mutate(
        fuel_time_cost_mile = fuel_cost_mile + .enviro_factors$TIME_COST_MI,
        payd_ins_adj = .payd_fee / .enviro_factors$INS_COST_MI,
        vmt_fee_cross_adj = 1 + (.vmt_fee / fuel_time_cost_mile) +
          payd_ins_adj * vmt_cross,
        vmt_fee_elas_adj = 1 + (.vmt_fee / fuel_time_cost_mile) +
          payd_ins_adj * vmt_elas,
        cong_adjust = 1 + .cong_price / fuel_time_cost_mile *
          .enviro_factors$CONG_VMT * cong_elast,
        stock_proportion = (SIStock + CIStock + HEVStock) / TotStock,
        gas_adj = 1 + .gas_tax / fuel_cost_mile * stock_proportion *
          vmt_cross * ev_multiplier
      ) %>%
      select(
        year, ctu, fuel_time_cost_mile, payd_ins_adj,
        vmt_fee_cross_adj, vmt_fee_elas_adj, cong_adjust,
        stock_proportion,
        cross_vmt = vmt_cross, gas_adj
      )


    return(tb_fin)
  }
}


#' Calculate telework multiplier
#' @param .telework_pct additional percent of people teleworking in the final forecast year. Numeric between 0 and 1. Default is `0`
#' @inheritParams calc_vmt_forecast
#' @return
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
                         .telework_pct,
                         .enviro_factors = enviro_factors) {
  # browser()
  if (.mode == "PLDV") {
    telework_elast <- tibble(
      year = unique(.pass_tb$year),
      telework_elast_val = calc_elasticity(
        elas_list = c(rep(
          0, length(unique(.pass_tb$year))
        )),
        elas = .telework_pct,
        num_inits = 3,
        num_yrs = length(unique(.pass_tb$year)) - 3
      )
    )

    telework_adj_tb <- telework_elast %>%
      mutate(
        telework_adj = 1 + telework_elast_val * .enviro_factors$MARG_TELEWORK
      ) %>%
      select(year, telework_adj)

    return(telework_adj_tb)
  } else {
    stop("Telework adjustment is only applicable for passenger light-duty vehicles")
  }
}


#' Calculate stock adjustment
#'
#' @inheritParams calc_vmt_forecast
#' @return table with columns `ctu`, `year`, `mode`, and `mode_stock_adj`
#' @export
#' @family VMT effects
#'
vmt_stock_proportion <- function(.tb,
                                 .mode,
                                 .stock) {
  if (.mode %in% c(
    "WALK",
    "BIKE"
  )) {
    tb_stock_proportion <- .tb %>%
      filter(mode == .mode) %>%
      mutate(mode_stock_adj = 1) %>%
      select(year, ctu, mode, mode_stock_adj) %>%
      unique()
  } else if (.mode == "DRS") {
    tb_stock_proportion <- .tb %>%
      filter(
        mode == "PLDV",
        var %in% c(
          "SIStock",
          "CIStock",
          "HEVStock",
          "TotStock"
        )
      ) %>%
      tidyr::pivot_wider(
        names_from = var,
        values_from = value
      ) %>%
      mutate(
        mode_stock_adj = (SIStock + CIStock + HEVStock) / TotStock,
        mode = .mode
      ) %>%
      select(ctu, year, mode, mode_stock_adj) %>%
      unique()
  } else {
    tb_stock_proportion <- .tb %>%
      filter(
        mode == .mode,
        var %in% c(
          .stock,
          "TotStock"
        )
      ) %>%
      tidyr::pivot_wider(
        names_from = var,
        values_from = value
      ) %>%
      mutate(mode_stock_adj = !!as.name(.stock) / TotStock) %>%
      select(ctu, year, mode, mode_stock_adj) %>%
      unique()
  }

  return(tb_stock_proportion)
}



#' Calculate transit ridership adjustment for each forecast year
#' @param .transit_rider_pct transit ridership % adjustment. Numeric between -1 and 1.
#'      Default is `0`
#' @inheritParams calc_vmt_forecast
#' @return a table with columns `year`, `ctu`, and `transit_adj`.
#' @export
#' @family VMT effects
vmt_transit_ridership <- function(tb,
                                  .mode,
                                  .transit_rider_pct,
                                  .elast = elast,
                                  .enviro_factors = enviro_factors) {
  transit_rider_elast <-
    tibble(
      year = unique(tb$year),
      .elast =
        calc_elasticity(
          elas_list = c(rep(0, length(unique(tb$year)))),
          elas = .transit_rider_pct,
          num_inits = 3,
          num_yrs = length(unique(tb$year)) - 3
        )
    )


  if (.mode %in% c(
    "PLDV",
    "AV"
  )) {
    tb %>%
      select(year, ctu) %>%
      unique() %>%
      left_join(transit_rider_elast, by = c("year")) %>%
      mutate(transit_adj = .elast * .enviro_factors$PLDV_TRANSIT_RATIO) %>%
      select(year, ctu, transit_adj) %>%
      unique() %>%
      return()
  } else if (.mode %in% c(
    "BU",
    "BRT",
    "RU",
    "RI"
  )) {
    tb %>%
      select(year, ctu) %>%
      unique() %>%
      left_join(transit_rider_elast, by = c("year")) %>%
      mutate(transit_adj = 1 + .elast) %>%
      select(year, ctu, transit_adj) %>%
      unique() %>%
      return()
  }
}

#' Calculate vehicle occupancy multiplier
#'
#' @param .transit_avo_pct transit average vehicle occupancy (AVO) % adjustment. Default is `0`
#' @inheritParams calc_vmt_forecast
#' @return table with columns `ctu`, `year`, `occupancy_adj`
#' @export
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
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr right_join
vmt_vehicle_occupancy <- function(tb,
                                  .tb_vmt,
                                  .mode,
                                  .stock,
                                  .transit_avo_pct,
                                  .enviro_factors = enviro_factors) {
  # browser()


  # some modes apply the same AVO to all CTUs


  if (.mode %in% c("PLDV", "AV")) {
    pldv_occupancy <- tb %>%
      filter(
        mode == .mode,
        var == "AVO"
      ) %>%
      select(year, ctu,
        occupancy_adj = value
      ) %>%
      unique()
    return(pldv_occupancy)
  } else if (.mode %in% c(
    "BU",
    "BRT",
    "RU",
    "RI"
  )) {
    transit_avo_pct_elast <- tibble(
      year = unique(tb$year),
      avo_elast =
        calc_elasticity(
          elas_list = c(rep(0, length(unique(tb$year)))),
          elas = .transit_avo_pct,
          num_inits = 3,
          num_yrs = length(unique(tb$year)) - 3
        )
    )


    tb_mode_totstock <- tb %>%
      filter(
        mode == .mode,
        var %in% c(
          .stock,
          "TotStock",
          "AVO"
        )
      ) %>%
      unique() %>%
      tidyr::pivot_wider(
        names_from = var,
        values_from = value
      ) %>%
      select(mode,
        year,
        ctu,
        aeo_mode,
        type,
        mode_avo = AVO
      )

    occ_return <- tb_mode_totstock %>%
      left_join(.tb_vmt, by = c("year", "ctu", "mode", "aeo_mode", "type")) %>%
      rowwise() %>%
      left_join(transit_avo_pct_elast, by = c("year")) %>%
      mutate(occupancy_adj = mode_avo * (1 + avo_elast)) %>%
      select(ctu, year, occupancy_adj) %>%
      unique()

    return(occ_return)
  } else if (.mode %in% c(
    "BS",
    "FR",
    "SUT",
    "CUT",
    "MM",
    "AIR",
    "WAT"
  )) {
    tb_mode_totstock <- tb %>%
      filter(
        mode == .mode,
        var %in% c(
          .stock,
          "TotStock",
          "AVO"
        )
      ) %>%
      unique() %>%
      tidyr::pivot_wider(
        names_from = var,
        values_from = value
      ) %>%
      select(mode,
        year,
        ctu,
        aeo_mode,
        type,
        mode_avo = AVO
      )

    occ_return <- tb_mode_totstock %>%
      left_join(.tb_vmt, by = c("year", "ctu", "mode", "aeo_mode", "type")) %>%
      mutate(occupancy_adj = mode_avo) %>%
      select(ctu, year, occupancy_adj) %>%
      unique()

    return(occ_return)
  }
}
