#' Calculate telework multiplier
#'
#' @return
#' @export
#' @details
#' Only applicable for PLDV,
#'
#' \loadmathjax
#' \mjdeqn{GHG = \frac{PMT}{AVO \times FF} \times TW \times GF}{GHG = (PMT)/(AVO x FF) x TW x GF}
#'     where \eqn{GHG} is the impact in metric tons of CO2 equivalent,
#'     \eqn{PMT} is passenger miles traveled,
#'     \eqn{AVO} is the average vehicle occupancy,
#'     \eqn{FF} is the fuel factor representing consumption of fuel per mile of travel,
#'     \eqn{GF} is the greenhouse gas factor per unit of consumed fuel,
#'     and \eqn{TW} is an adjustment factor for the effect of telework on baseline PMT.
calc_telework <- function(.mode,
                          .telework_pct) {
  browser()
  if (.mode == "PLDV") {
    1 + (.telework_pct / 100) * MARG_TELEWORK
  } else {
    stop("Telework adjustment is only applicable for passenger light-duty vehicles")
  }
}


#' Calculate autonomous vehicle multiplier
#'
#' @return table with columns `year`, `ctu`, `av_adj`
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
calc_autonomous_vehicle <- function(.tb_vmt,
                                    .av_pct,
                                    .mode
                                    ) {
  browser()


  if (.mode == "PLDV") {
    tb_avshare <- tb %>%
      dplyr::filter(var == "AVShare")

    if (.av_pct > 0) {
      tb_avshare %>%
        mutate(1 - value & .av_pct / 100)
    } else {
      1
    }
  } else if ((.mode == "BU") |
             (.mode == "BRT") |
             (.mode == "RU") |
             (.mode == "RI")) {

   av_return <-  .tb_vmt %>%
      mutate(av_adj = dplyr::case_when(
        (((.mode == "BU") | (.mode == "BRT")) & .av_pct > 0) ~ ((1 + BUS_AV * .av_pct) / 100),
        (((.mode == "RU") | (.mode == "RI")) & .av_pct > 0) ~ ((1 + RAIL_AV * .av_pct) / 100),
        TRUE ~ 1
      )) %>%
     select(year, ctu, av_adj)

   return(av_return)
  } else if (.mode == "AV"){

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
calc_vehicle_occupancy <- function(tb,
                                   .tb_vmt,
                                   .mode,
                                   .gas_tax,
                                   .stock,
                                   .transit_avo) {
  browser()
  tb_mode_totstock <- tb %>%
    filter(
      mode == .mode,
      var %in% c(.stock,
                 "TotStock",
                 "AVO")
    ) %>%
    unique() %>%
    pivot_wider(
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
           mode_avo = AVO)


  if (.mode == "PLDV") {

    pldv_occupancy <- transportation_data$passenger %>%
      filter(
        mode == "PLDV",
        var == "AVO"
      ) %>%
      select(year, ctu,
             pldv_avo = value)

  } else if ((.mode == "BU") |
             (.mode == "BRT") |
             (.mode == "RU") |
             (.mode == "RI")) {




    occ_return <- tb_mode_totstock %>%
      left_join(.tb_vmt, by = c("year", "ctu", "mode", "aeo_mode", "type")) %>%
      mutate(occupancy_adj = mode_avo * ( 1 + (.transit_avo / 100)) * (mode_stock / mode_totstock)) %>%
      select(ctu, year, occupancy_adj)

    return(occ_return)

  } else if (.mode == "AV"){

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
calc_dynamic_ride_sharing <- function() {

}

#' Title
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
#'     \eqn{FF} is the fuel factor representing consumption of fuel per mile of travel,
#'     and \eqn{GF} is the greenhouse gas factor per unit of consumed fuel.
#'
#'     In the case of electric vehicles, \eqn{FF} is measured in kWh of electricity consumed per mile and
#'     \eqn{GF} depends on the GHG intensity of the electricity grid.
#'
calc_road_pricing <- function() {


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
calc_parking_policy <- function(.mode,
                                .parking_price) {
  browser()


  pldv_si_parking <- transportation_data$passenger %>%
    filter(
      mode == "PLDV",
      var %in% c(
        "PARK",
        "SIStock"
      )
    ) %>%
    unique() %>%
    pivot_wider(
      names_from = var,
      values_from = value
    )


  if (.mode == "PLDV") {
    pldv_si_parking %>%
      left_join(elast %>%
                  select(year, park),
                by = "year") %>%
      mutate(
        park_price_adj =
          (1 + (.parking_price / PARK * park))
      ) %>%
      select(year, ctu, park_price_adj) %>%
      return()

  } else if ((.mode == "BU") |
             (.mode == "BRT") |
             (.mode == "RU") |
             (.mode == "RI")) {

    pldv_si_parking %>%
      left_join(elast_cross %>%
                  select(year, park_transit),
                by = "year") %>%
      mutate(
        park_price_adj =
          1 + (.parking_price / PARK * park_transit)
      ) %>%
      select(year, ctu, park_price_adj) %>%
      return()
  } else if (.mode == "AV"){

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
calc_land_use_change <- function(.mode,
                                 .type,
                                 .comb_5d_impact_pct_change,
                                 .pop_dens_pct_change,
                                 .emp_dens_pct_change,
                                 .land_use_pct_change,
                                 .intersection_design_pct_change,
                                 .job_access_pct_change,
                                 .transit_dist_pct_change) {
  browser()

  if(!.type %in% c("WALK", "DRIVE", "TRANSIT")){
    stop(".type must be one of 'WALK', 'DRIVE', or 'TRANSIT'. ")
  }

  comb_5d_elast <- elast_5d %>%
    filter(type == .type) %>%
    mutate(
      population_density = (1 + .pop_dens_pct_change / 100) * .data$population_density,
      employment_density = (1 + .emp_dens_pct_change / 100) * .data$employment_density,
      diversity = (1 + .land_use_pct_change / 100) * .data$diversity,
      design = (1 + .intersection_design_pct_change / 100) * .data$design,
      job_access = (1 + .job_access_pct_change / 100) * .data$job_access,
      distance = (1 + .transit_dist_pct_change / 100) * .data$distance,
      combined_density = (1 + .pop_dens_pct_change / 100) * .data$combined_density
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
      land_use_adj = ifelse(.comb_5d_impact_pct_change < MAX_5D_TRANS,
                            1 + MAX_5D_TRANS,
                            product_all
      )
    ) %>%
    select(year, land_use_adj)

  return(comb_5d_elast)
}


#' Calculate annual energy outlook (AEO) multipliers for each forecast year
#'
#' @return a table with columns `AEOScen`, `Metric`, `Mode`, `year`, and `aeo_adj`.
#' @export
#'
calc_annual_energy_outlook <- function(tb,
                                       .mode,
                                       .aeo_scenario) {
  browser()
  tb_fin <- tb %>%
    filter(mode == .mode) %>%
    unique() %>%
    pivot_wider(
      names_from = var,
      values_from = value,
    )

  aeo_vals <- factor_values$aeo %>%
    dplyr::filter(
      AEOScen == .aeo_scenario,
      Metric == "VMT",
      Mode == unique(tb_fin$aeo_mode)
    ) %>%
    select(everything(),
           aeo_adj = value
    )
  return(aeo_vals)
}

#' Calculate transit ridership adjustment for each forecast year
#'
#' @return a table with columns `year`, `ctu`, and `transit_adj`.
#' @export
#'
calc_transit_ridership <- function(.tb_vmt,
                                   .mode,
                                   .transit_rider_pct) {
  browser()
  if (.mode == "PLDV") {
    .tb_vmt %>%
      select(year, ctu) %>%
      mutate(transit_adj = .transit_rider_pct / 100 * PLDV_TRANSIT_RATIO) %>%
      return()

  } else if ((.mode == "BU") |
             (.mode == "BRT") |
             (.mode == "RU") |
             (.mode == "RI")) {
    .tb_vmt %>%
      select(year, ctu) %>%
      mutate(transit_adj = 1 + .transit_rider_pct / 100) %>%
      return()
  } else if(.mode == "AV"){

  }
}

#' Calculate fuel, VMT, stock, congestion, and gas adjustments for each forecast year
#'
#' @return
#' @export
#'
calc_vehicle_fuel <- function(.mode,
                                 .tb_vmt,
                                 .tb_fuel_cost_mile,
                                 .vmt_fee,
                                 .cong_price,
                                 .gas_tax,
                                 .payd_fee,
                                 .is_av,
                                 .stock) {
  browser()
  if (.mode == "PLDV") {
    ev_multiplier <- ifelse(.stock %in% c(
      "SIStock",
      "CIStock",
      "HEVStock"
    ), 1, 0)


    fc_return <- .tb_fuel_cost_mile %>%
      left_join(elast, by = "year") %>%
      left_join(elast_cross, by = "year")
    mutate(
      fuel_time_cost_mile = fuel_cost_mile + TIME_COST_MI,
      payd_ins_adj = .payd_fee / INS_COST_MI,
      vmt_fee_adjust = 1 + ((miles_traveled / fuel_time_cost_mile) + payd_ins_adj) * vmt,
      cong_adjust = 1 + (.cong_price / fuel_time_cost_mile) * CONG_VMT * cong,
      cross_vmt = vmt_cross,
      gas_adj = 1 + (.gas_tax / fuel_time_cost_mile) * ev_multiplier * gas
    ) %>%
      select(year, ctu, fuel_time_cost_mile, payd_ins_adj,
             vmt_fee_adjust, cong_adjust, cross_vmt, gas_adj)


    return(fc_return)
  } else if ((.mode == "BU") |
             (.mode == "BRT") |
             (.mode == "RU") |
             (.mode == "RI")) {

    elast_vmt <- elast_cross %>%
      select(year, vmt_elas = vmt_cross)

    fc_return <-  .tb_fuel_cost_mile %>%
      select(-ctu, -var) %>%
      left_join(pldv_stocks, by = c("year", "mode")) %>%
      left_join(.tb_vmt, by = c("year", "ctu", "type")) %>%
      left_join(elast_vmt, by = "year") %>%
      mutate(
        fuel_time_cost_mile = fuel_cost_mile + TIME_COST_MI,
        payd_ins_adj = .payd_fee / INS_COST_MI,
        vmt_fee_adjust = miles_traveled / fuel_time_cost_mile,
        cong_adjust = (.cong_price / fuel_time_cost_mile) * CONG_VMT,
        stock_proportion = (SIStock + CIStock + HEVStock)/TotStock,
        cross_vmt = vmt_elas,
        gas_adj = 1 + ((.gas_tax/fuel_cost_mile) * (stock_proportion) * cross_vmt)

      ) %>%
      select(year, ctu, fuel_time_cost_mile, payd_ins_adj,
             vmt_fee_adjust, cong_adjust,
             stock_proportion,
             cross_vmt, gas_adj)

    return(fc_return)
  } else if(.mode == "AV"){

  }
}
