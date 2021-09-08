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
    error("Telework adjustment is only applicable for passenger light-duty vehicles")
  }
}


#' Title
#'
#' @return
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
calc_autonomous_vehicle <- function(tb,
                                    .mode,
                                    .is_av,
                                    .av_pct,
                                    .gas_tax,
                                    .stock,
                                    .transit_avo) {
  browser()
  tb_mode_totstock <- tb %>%
    filter(
      mode == .mode,
      var == "TotStock"
    ) %>%
    pivot_wider(
      names_from = var,
      values_from = value
    )


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
    pldv_occupancy <- transportation_data$passenger %>%
      filter(
        mode == "PLDV",
        var == "AVO"
      )

    pldv_occupancy %>%
      left_join(tb_mode_totstock) %>%
      mutate(av_adj = value * .transit_avo / 100 * vmt / TotStock) %>%
      mutate(av_transit_adj = dplyr::case_when(
        (((.mode == "BU") | (.mode == "BRT")) & .av_pct > 0) ~ ((1 + BUS_AV * .av_pct) / 100),
        (((.mode == "RU") | (.mode == "RI")) & .av_pct > 0) ~ ((1 + RAIL_AV * .av_pct) / 100),
        TRUE ~ 1
      ))
  }
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
#' @inheritParams calc_vmt
#' @return
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
      mutate(
        park_price_adj =
          (1 + (.parking_price / PARK * elast$park))
      )
  } else if ((.mode == "BU") |
    (.mode == "BRT") |
    (.mode == "RU") |
    (.mode == "RI")) {
    pldv_si_parking %>%
      mutate(
        park_price_adj =
          1 + (.parking_price / PARK * crosses$park_transit)
      )
  }
}


#' Calculate combined 5D land use change impact
#'
#' @inheritParams calc_vmt
#' @return
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
    )

  return(comb_5d_elast)
}


#' Calculate annual energy outlook (AEO) multipliers for each forecast year
#'
#' @return
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
      aeo_value = value
    )
  return(aeo_vals)
}

#' Calculate transit ridership adjustment for each forecast year
#'
#' @return
#' @export
#'
calc_transit_ridership <- function(.mode,
                                   .transit_rider_pct) {
  browser()
  if (.mode == "PLDV") {
    return(.transit_rider_pct / 100 * PLDV_TRANSIT_RATIO)
  } else if ((.mode == "BU") |
    (.mode == "BRT") |
    (.mode == "RU") |
    (.mode == "RI")) {
    return(1 + .transit_rider_pct / 100)
  }
}

#' Calculate transit ridership adjustment for each forecast year
#'
#' @return
#' @export
#'
calc_fuel_congestion <- function(.mode,
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


    .tb_fuel_cost_mile %>%
      mutate(
        fuel_time_cost_mile = fuel_cost_mile + TIME_COST_MI,
        payd_ins_adj = .payd_fee / INS_COST_MI,
        vmt_fee_adjust = 1 + ((vmt / fuel_time_cost_mile) + payd_ins_adj) * elast$vmt,
        cong_adjust = 1 + (.cong_price / fuel_time_cost_mile) * CONG_VMT * elast$cong,
        cross_vmt = elast_cross$vmt,
        gas_adj = 1 + (.gas_tax / fuel_time_cost_mile) * ev_multiplier * elast$gas
      )
  } else if ((.mode == "BU") |
    (.mode == "BRT") |
    (.mode == "RU") |
    (.mode == "RI")) {
    elast_vmt <- elast_cross %>%
      select(year, vmt_elas = vmt)

    .tb_fuel_cost_mile %>%
      select(-ctu, -var) %>%
      left_join(pldv_stocks, by = c("year", "mode")) %>%
      left_join(.tb_vmt, by = c("year", "ctu", "type")) %>%
      left_join(elast_vmt, by = "year") %>%
      mutate(
        fuel_time_cost_mile = fuel_cost_mile + TIME_COST_MI,
        payd_ins_adj = .payd_fee / INS_COST_MI,
        vmt_fee_adjust = vmt / fuel_time_cost_mile,
        cong_adjust = (.cong_price / fuel_time_cost_mile) * CONG_VMT,
        cross_vmt = vmt_elas
      ) %>%
      return()
  }
}
