#' Calculate VMT effects

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
vmt_autonomous_vehicle <- function(.tb_vmt,
                                   .av_pct,
                                   .mode,
                                   .enviro_factors = enviro_factors) {
  # browser()


  if (.mode == "PLDV") {
    tb_avshare <- transportation_data$passenger %>%
      dplyr::filter(var == "AVShare")

    tb_avshare %>%
      mutate(av_adj = case_when(
        .av_pct > 0 ~ 1 - (value * (.av_pct )),
        TRUE ~ 1
      )) %>%
      select(year, av_adj) %>%
      return()
  } else if (.mode %in% c(
    "BU",
    "BRT",
    "RU",
    "RI"
  )) {
    av_return <- .tb_vmt %>%
      mutate(av_adj = dplyr::case_when(
        (((.mode == "BU") | (.mode == "BRT")) & .av_pct > 0) ~ ((1 + .enviro_factors$BUS_AV * .av_pct) ),
        (((.mode == "RU") | (.mode == "RI")) & .av_pct > 0) ~ ((1 + .enviro_factors$RAIL_AV * .av_pct) ),
        TRUE ~ 1
      )) %>%
      select(year, ctu, av_adj)

    return(av_return)
  } else if (.mode == "AV") {
    tb_avshare <- transportation_data$passenger %>%
      dplyr::filter(var == "AVShare") %>%
      select(year, av_share = value)

    av_return <- .tb_vmt %>%
      left_join(tb_avshare, by = c("year")) %>%
      mutate(av_adj = av_share * .av_pct )

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
vmt_dynamic_ride_share_reduction <- function(.tb_vmt,
                                             .mode,
                                             .variable,
                                             .drs_pct,
                                             .enviro_factors = enviro_factors) {
  if (.drs_pct > 0) {
    browser()

    drs_share <- transportation_data$passenger %>%
      filter(var == "DRSShare") %>%
      select(year,
        ctu,
        drs_share_val = value
      )


    .tb_vmt %>%
      left_join(drs_share, by = c("year", "ctu")) %>%
      mutate(miles_traveled = miles_traveled *
        (1 - drs_share_val) * .drs_pct ) %>%
      unique() %>%
      return()



    # pass_transpo <- pass_transpo %>%
    #   dplyr::mutate(
    #     dplyr::across(
    #       all_of(YRS), ~ dplyr::case_when(
    #         ((mode == "PLDV") & var == "PMT") ~ .x * dplyr::case_when(
    #              .drs_pct > 0 ~ (1 - pass_transpo %>%
    #                                dplyr::filter(var == "DRSShare") %>%
    #                                dplyr::select(dplyr::cur_column()) %>%
    #                                as.numeric() * .drs_pct ),
    #              TRUE ~ 1
    #            ),
    #         TRUE ~ .x
    #       )
    #     )
    #   )
  } else {
    return(.tb_vmt)
  }
}

#' Calculate combined 5D land use change impact
#'
#' @inheritParams calc_vmt_forecast
#' @param .type character, one of `"DRIVE"`, `"WALK"`, `"TRANSIT"`.
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
vmt_land_use_change <- function(.mode,
                                .type,
                                .comb_5d_impact_pct_change,
                                .pop_dens_pct_change,
                                .emp_dens_pct_change,
                                .land_use_pct_change,
                                .intersection_design_pct_change,
                                .job_access_pct_change,
                                .transit_dist_pct_change,
                                .enviro_factors = enviro_factors) {
  # browser()

  if (!.type %in% c("WALK", "DRIVE", "TRANSIT")) {
    stop(".type must be one of 'WALK', 'DRIVE', or 'TRANSIT'. ")
  }

  comb_5d_elast <- elast_5d %>%
    filter(type == .type) %>%
    mutate(
      population_density = (1 + .pop_dens_pct_change ) * .data$population_density,
      employment_density = (1 + .emp_dens_pct_change ) * .data$employment_density,
      diversity = (1 + .land_use_pct_change ) * .data$diversity,
      design = (1 + .intersection_design_pct_change ) * .data$design,
      job_access = (1 + .job_access_pct_change ) * .data$job_access,
      distance = (1 + .transit_dist_pct_change ) * .data$distance,
      combined_density = (1 + .pop_dens_pct_change ) * .data$combined_density
    ) %>%
    mutate(
      product_all =
        .data$population_density *
          .data$employment_density *
          .data$diversity *
          .data$design *
          .data$job_access *
          .data$distance *
          .data$combined_density,
      land_use_adj = ifelse(.comb_5d_impact_pct_change < .enviro_factors$MAX_5D_TRANS,
        1 + .enviro_factors$MAX_5D_TRANS,
        product_all
      )
    ) %>%
    select(year, land_use_adj)

  return(comb_5d_elast)
}

#' Calculate parking price effect for passenger light-duty vehicles (PLDV) for each forecast year
#'
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
vmt_parking_policy <- function(.mode,
                               .parking_price,
                               .enviro_factors = enviro_factors) {
  # browser()


  pldv_si_parking <- transportation_data$passenger %>%
    filter(
      mode == "PLDV",
      var %in% c(
        "PARK",
        "SIStock"
      )
    ) %>%
    unique() %>%
    tidyr::pivot_wider(
      names_from = var,
      values_from = value
    )


  if (.mode == "PLDV") {
    pldv_si_parking %>%
      left_join(elast %>%
        select(year, park_elast),
      by = "year"
      ) %>%
      mutate(
        park_price_adj =
          1 + (.parking_price / (PARK * park_elast)),
        park_price_adj = ifelse(is.na(park_price_adj), 1, park_price_adj)
      ) %>%
      select(year, ctu, park_price_adj) %>%
      return()
  } else if (.mode %in% c(
    "BU",
    "BRT",
    "RU",
    "RI"
  )) {
    pldv_si_parking %>%
      left_join(elast %>%
        select(year, park_transit),
      by = "year"
      ) %>%
      mutate(
        park_price_adj =
          1 + (.parking_price / PARK * park_transit)
      ) %>%
      select(year, ctu, park_price_adj) %>%
      return()
  } else if (.mode == "AV") {
    pldv_si_parking %>%
      left_join(elast %>%
        select(year, park_elast),
      by = "year"
      ) %>%
      mutate(park_price_adj = 1 + (.parking_price / PARK * park_elast)) %>%
      select(year, ctu, park_price_adj) %>%
      return()
  } else if (.mode == "SUT") {
    # browser()

    tb_park <- transportation_data$freight %>%
      # note the freight data usage
      dplyr::filter(
        var == "PARK",
        mode == .mode
      ) %>%
      unique() %>%
      tidyr::pivot_wider(
        names_from = var,
        values_from = value
      )


    park_adj <- tb_park %>%
      left_join(elast %>%
        select(year, park_elast), by = "year") %>%
      mutate(park_price_adj = 1 + (.parking_price / (PARK * park_elast))) %>%
      select(year, ctu, park_price_adj) %>%
      return()
  }
}


#' Calculate fuel, VMT, stock, congestion, and gas adjustments for each forecast year
#'
#' @inheritParams calc_vmt_forecast
#' @return a table with columns   `year`, `ctu`, `fuel_time_cost_mile`, `payd_ins_adj`,
#'    `vmt_fee_adjust`, `cong_adjust`, `cross_vmt`, `gas_adj`
#' @export
#' @family VMT effects
vmt_road_policy <- function(.mode,
                            .tb_vmt,
                            .tb_fuel_cost_mile,
                            .vmt_fee,
                            .freight_vmt_fee = 0,
                            .cong_price,
                            .gas_tax,
                            .payd_fee,
                            .stock,
                            ch_phev = 0,
                            .enviro_factors = enviro_factors) {
  # browser()
  if (.mode == "PLDV") {
    ev_multiplier <- ifelse(.stock %in% c(
      "SIStock",
      "CIStock",
      "HEVStock"
    ), 1, 0)


    fc_return <- .tb_fuel_cost_mile %>%
      left_join(elast, by = "year") %>%
      left_join(.tb_vmt, by = c("year", "mode")) %>%
      mutate(
        fuel_time_cost_mile = fuel_cost_mile + .enviro_factors$TIME_COST_MI,
        payd_ins_adj = .payd_fee / .enviro_factors$INS_COST_MI,
        vmt_fee_adjust = 1 + ((miles_traveled / fuel_time_cost_mile) + payd_ins_adj) * vmt_elast,
        cong_adjust = 1 + (.cong_price / fuel_time_cost_mile) * .enviro_factors$CONG_VMT * cong_elast,
        cross_vmt = vmt_cross,
        gas_adj = 1 + (.gas_tax / fuel_time_cost_mile) * ev_multiplier * gas_elast
      ) %>%
      select(
        year, ctu, fuel_time_cost_mile, payd_ins_adj,
        vmt_fee_adjust, cong_adjust, cross_vmt, gas_adj
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
    pldv_stocks <- transportation_data$passenger %>%
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


    elast_vmt <- elast %>%
      select(year, vmt_elas = vmt_cross)

    fc_return <- .tb_fuel_cost_mile %>%
      # select(-ctu, -var) %>%
      left_join(pldv_stocks, by = c("year", "mode")) %>%
      left_join(.tb_vmt, by = c("year", "ctu", "type")) %>%
      left_join(elast_vmt, by = "year") %>%
      mutate(
        fuel_time_cost_mile = fuel_cost_mile + .enviro_factors$TIME_COST_MI,
        payd_ins_adj = .payd_fee / .enviro_factors$INS_COST_MI,
        vmt_fee_adjust = .vmt_fee / fuel_time_cost_mile,
        cong_adjust = (.cong_price / fuel_time_cost_mile) * .enviro_factors$CONG_VMT,
        stock_proportion = (SIStock + CIStock + HEVStock) / TotStock,
        cross_vmt = vmt_elas,
        gas_adj = 1 + ((.gas_tax / fuel_cost_mile) * (stock_proportion) * cross_vmt)
      ) %>%
      select(
        year, ctu, fuel_time_cost_mile, payd_ins_adj,
        vmt_fee_adjust, cong_adjust,
        stock_proportion,
        cross_vmt, gas_adj
      )

    return(fc_return)
  } else if (.mode == "AV") {
    ev_multiplier <- if (.stock %in% c(
      "SIStock",
      "CIStock",
      "HEVStock",
      "PHEVStock"
    ) & ch_phev == 1) {
      1
    } else {
      0
    }


    fc_return <- .tb_fuel_cost_mile %>%
      # select(-ctu) %>%
      left_join(elast, by = "year") %>%
      left_join(.tb_vmt, by = c("year", "mode")) %>%
      mutate(
        fuel_time_cost_mile = fuel_cost_mile + .enviro_factors$TIME_COST_MI,
        payd_ins_adj = .payd_fee / .enviro_factors$INS_COST_MI,
        vmt_fee_adjust = 1 + ((.vmt_fee / fuel_time_cost_mile) + payd_ins_adj) * vmt_elast,
        cong_adjust = 1 + (.cong_price / fuel_time_cost_mile) * .enviro_factors$CONG_VMT * cong_elast,
        cross_vmt = vmt_cross,
        gas_adj = 1 + (.gas_tax / fuel_time_cost_mile) * ev_multiplier * gas_elast
      ) %>%
      select(
        year, ctu, fuel_time_cost_mile, payd_ins_adj,
        vmt_fee_adjust, cong_adjust, cross_vmt, gas_adj
      )
  } else if (.mode == "SUT") {
    fc_return <- .tb_fuel_cost_mile %>%
      left_join(elast %>%
        select(year, freight_vmt_elast),
      by = "year"
      ) %>%
      mutate(
        fuel_time_cost_mile = fuel_cost_mile + .enviro_factors$TIME_COST_MI,
        vmt_fee_adj = 1 + ((.freight_vmt_fee / fuel_time_cost_mile) * freight_vmt_elast * .enviro_factors$F_FRACT)
      ) %>%
      select(year, vmt_fee_adj)

    return(fc_return)
  } else if (.mode == "CUT") {
    # browser()
    fc_return <- .tb_fuel_cost_mile %>%
      left_join(elast %>%
        select(year, freight_vmt_elast),
      by = "year"
      ) %>%
      mutate(
        fuel_time_cost_mile = fuel_cost_mile + .enviro_factors$TIME_COST_MI,
        vmt_fee_adj = 1 + ((.freight_vmt_fee / fuel_time_cost_mile) * freight_vmt_elast)
      ) %>%
      select(year, vmt_fee_adj)

    return(fc_return)
  }
}


#' Calculate telework multiplier
#'
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
vmt_telework <- function(.mode,
                         .telework_pct,
                         .enviro_factors = enviro_factors) {
  # browser()
  if (.mode == "PLDV") {
    telework_adj_tb <- tibble(
      year = unique(transportation_data$passenger$year),
      telework_adj = 1 + (.telework_pct ) * .enviro_factors$MARG_TELEWORK
    )

    return(telework_adj_tb)
  } else {
    stop("Telework adjustment is only applicable for passenger light-duty vehicles")
  }
}






#' Calculate transit ridership adjustment for each forecast year
#'
#' @return a table with columns `year`, `ctu`, and `transit_adj`.
#' @export
#' @family VMT effects
vmt_transit_ridership <- function(.tb_vmt,
                                  .mode,
                                  .transit_rider_pct,
                                  .enviro_factors = enviro_factors) {
  # browser()
  if (.mode %in% c(
    "PLDV",
    "AV"
  )) {
    .tb_vmt %>%
      select(year, ctu) %>%
      mutate(transit_adj = .transit_rider_pct  * .enviro_factors$PLDV_TRANSIT_RATIO) %>%
      select(year, ctu, transit_adj) %>%
      unique() %>%
      return()
  } else if (.mode %in% c(
    "BU",
    "BRT",
    "RU",
    "RI"
  )) {
    .tb_vmt %>%
      select(year, ctu) %>%
      mutate(transit_adj = 1 + .transit_rider_pct ) %>%
      select(year, ctu, transit_adj) %>%
      unique() %>%
      return()
  }
}

#' Calculate vehicle occupancy multiplier
#'
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
                                  .gas_tax,
                                  .stock,
                                  .transit_avo,
                                  .enviro_factors = enviro_factors) {
  # browser()
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
      mode_totstock = TotStock,
      mode_stock = !!rlang::sym(.stock),
      mode_avo = AVO
    )

  # some modes apply the same AVO to all CTUs


  if (.mode %in% c("PLDV", "AV")) {
    pldv_occupancy <- transportation_data$passenger %>%
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
    # if (.mode %in% c("RU", "RI", "BRT")) {
    #   tb_mode_totstock_ctu <- tb_mode_totstock %>%
    #     filter(is.na(mode_avo)) %>%
    #     select(-mode_avo)
    #
    #   tb_mode_totstock <- tb_mode_totstock %>%
    #     filter(!is.na(mode_avo)) %>%
    #     select(mode, year, aeo_mode, type, mode_avo) %>%
    #     dplyr::right_join(tb_mode_totstock_ctu,
    #       by = c("mode", "year", "aeo_mode", "type")
    #     )
    # }


    occ_return <- tb_mode_totstock %>%
      left_join(.tb_vmt, by = c("year", "ctu", "mode", "aeo_mode", "type")) %>%
      mutate(occupancy_adj = mode_avo * (1 + (.transit_avo )) * (mode_stock / mode_totstock)) %>%
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
    occ_return <- tb_mode_totstock %>%
      left_join(.tb_vmt, by = c("year", "ctu", "mode", "aeo_mode", "type")) %>%
      mutate(occupancy_adj = mode_avo * (mode_stock / mode_totstock)) %>%
      select(ctu, year, occupancy_adj) %>%
      unique()

    return(occ_return)
  }
}
