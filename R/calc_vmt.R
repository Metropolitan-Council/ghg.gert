#' @title Calculate vehicle miles traveled by mode and power train
#'
#' @param .scenario character, scenario name
#' @param tb input table. Should have columns `mode`, `var`, `ctu`,
#'    and one for each year
#' @param .mode current mode
#' @param .stock stock for current mode
#' @param .variable variable name - e.g., "VMT"
#' @param .fuel_cost_mile fuel cost per mile
#' @param .aeo_scenario selected EIA Annual Energy Outlook scenario. Default is `"REF"`
#' @param .transit_avo transit average vehicle occupancy (AVO) % adjustment. Default is `0`
#' @param .transit_rider_pct transit ridership % adjustment. Default is `0`
#' @param .vmt_fee VMT fee per mile. Default is `0`
#' @param .payd_fee  PAYD insurance fee per mile. Default is `0`
#' @param .gas_tax .gas_tax tax per mile. Default is `0`
#' @param .cong_price congestion price per mile (only applies to an approximation of
#'     congested miles in MSP). Default is `0`
#' @param .parking_price measured in cents per hour. Average price of parking based on TBI results and literature -
#'      Default is `0`
#' @param .drs_pct percent of trips by auto or transit that are now by dynamic ride sharing (DRS).
#'     Default is `0`
#' @param .av_pct percent of trips made by AV. Default is `0`
#' @param .freight_vmt_fee freight VMT fee per mile. Default is `0`
#' @param .pop_dens_pct_change percent change in population density in 2050 wrt BAU. Default is `0`
#' @param .emp_dens_pct_change percent change in employment density in 2050 wrt BAU. Default is `0`
#' @param .land_use_pct_change percent change in land use diversity/mix in 2050 wrt BAU. Default is `0`
#' @param .intersection_design_pct_change percent change in intersection .intersection_design_pct_change (% 4-way stops) in 2050 wrt BAU. Default is `0`
#' @param .job_access_pct_change percent change in job accessibility in 2050 wrt BAU. Default is `0`
#' @param .transit_dist_pct_change percent change in transit distance in 2050 wrt BAU. Default is `0`
#' @param .comb_5d_impact_pct_change percent change in population density in 2050 wrt BAU
#'      as a measure of composite change in 5Ds on VMT. Default is `0`
#' @param .telework_pct percent of people .telework_pcting in 2050. Default is `0`
#' @param ch_phev the current alternative is PHEV, which needs both
#'     gasoline and electric results (using assumption
#'     about gasoline/electric mode split). Default is `0`
#'
### Eqn: (PMT in 1000 mi) x Pr(stock by fuel) / AVO
#'
#' @return
#' @export
#' @family transportation
#'
#'
#' @importFrom dplyr filter select case_when
#' @importFrom tidyselect all_of
#'
calc_vmt <- function(.scenario,
                     tb,
                     .mode,
                     .stock,
                     .variable,
                     .fuel_cost_mile,
                     .aeo_scenario = "REF",
                     .transit_avo = 0,
                     .transit_rider_pct = 0,
                     .vmt_fee = 0,
                     .payd_fee = 0,
                     .gas_tax = 0,
                     .cong_price = 0,
                     .parking_price = 0,
                     .drs_pct = 0,
                     .av_pct = 0,
                     .freight_vmt_fee = 0,
                     .pop_dens_pct_change = 0,
                     .emp_dens_pct_change = 0,
                     .land_use_pct_change = 0,
                     .intersection_design_pct_change = 0,
                     .job_access_pct_change = 0,
                     .transit_dist_pct_change = 0,
                     .comb_5d_impact_pct_change = 0,
                     .telework_pct = 0,
                     ch_phev = 0) {
  browser()
  # Annual energy outlook VMT tables -------
  # Specific to each mode type
  aeo_vmt <- list(
    rail = aeo_factors %>%
      dplyr::filter(AEOScen == .aeo_scenario, Metric == "VMT", Mode == "RAIL") %>%
      dplyr::select(tidyselect::all_of(YRS)) %>%
      as.numeric(),
    bus = aeo_factors %>%
      dplyr::filter(AEOScen == .aeo_scenario, Metric == "VMT", Mode == "BUS") %>%
      dplyr::select(tidyselect::all_of(YRS)) %>%
      as.numeric(),
    ldv = aeo_factors %>%
      dplyr::filter(AEOScen == .aeo_scenario, Metric == "VMT", Mode == "LDV") %>%
      dplyr::select(tidyselect::all_of(YRS)) %>%
      as.numeric(),
    mdt = aeo_factors %>%
      dplyr::filter(AEOScen == .aeo_scenario, Metric == "VMT", Mode == "MDT") %>%
      dplyr::select(tidyselect::all_of(YRS)) %>%
      as.numeric(),
    hdt = aeo_factors %>%
      dplyr::filter(AEOScen == .aeo_scenario, Metric == "VMT", Mode == "HDT") %>%
      dplyr::select(tidyselect::all_of(YRS)) %>%
      as.numeric(),
    frail = aeo_factors %>%
      dplyr::filter(AEOScen == .aeo_scenario, Metric == "VMT", Mode == "FRAIL") %>%
      dplyr::select(tidyselect::all_of(YRS)) %>%
      as.numeric(),
    fship = aeo_factors %>%
      dplyr::filter(AEOScen == .aeo_scenario, Metric == "VMT", Mode == "FSHIP") %>%
      dplyr::select(tidyselect::all_of(YRS)) %>%
      as.numeric()
  )


  # plvd stocks -----
  # passenger light duty vehicle
  pldv_stocks <- list(
    si = tb %>%
      dplyr::filter(mode == "PLDV", var == "SIStock") %>%
      dplyr::select(tidyselect::all_of(YRS)),
    ci = tb %>%
      dplyr::filter(mode == "PLDV", var == "CIStock") %>%
      dplyr::select(tidyselect::all_of(YRS)),
    hev = tb %>%
      dplyr::filter(mode == "PLDV", var == "HEVStock") %>%
      dplyr::select(tidyselect::all_of(YRS)),
    .parking_price = tb %>%
      dplyr::filter(mode == "PLDV", var == "Park") %>%
      dplyr::select(tidyselect::all_of(YRS)),
    tot = tb %>%
      dplyr::filter(mode == "PLDV", var == "TotStock") %>%
      dplyr::select(tidyselect::all_of(YRS)),
    avo = tb %>%
      dplyr::filter(mode == "PLDV", var == "AVO") %>%
      dplyr::select(tidyselect::all_of(YRS))
  )

  # .mode held constant table subsets ------
  tb_mode <- list(
    var = tb %>%
      dplyr::filter(mode == .mode, var == .variable) %>%
      dplyr::select(tidyselect::all_of(YRS)),
    stock = tb %>%
      dplyr::filter(mode == .mode, var == .stock) %>%
      dplyr::select(tidyselect::all_of(YRS)),
    tot_stock = tb %>%
      dplyr::filter(mode == .mode, var == "TotStock") %>%
      dplyr::select(tidyselect::all_of(YRS)),
    avo = tb %>% # average vehicle occupancy
      dplyr::filter(mode == .mode, var == "AVO") %>%
      dplyr::select(tidyselect::all_of(YRS))
  )

  # variable held constant -----
  tb_var <- list(
    at = tb %>%
      dplyr::filter(mode == "AT", var == .variable) %>%
      dplyr::select(tidyselect::all_of(YRS)),
    pldv = tb %>%
      dplyr::filter(mode == "PLDV", var == .variable) %>%
      dplyr::select(tidyselect::all_of(YRS))
  )


  tb_avshare <- tb %>%
    dplyr::filter(var == "AVShare") %>%
    dplyr::select(tidyselect::all_of(YRS)) %>%
    as.numeric()

  tb_park <- tb %>%
    dplyr::filter(var == "PARK") %>%
    dplyr::select(tidyselect::all_of(YRS)) %>%
    as.numeric()

  # basic filter for mode and variable



  # calculation -----
  # If it's not the BAU scenario, then need to run elasticities, etc.
  if (.scenario != "BAU") {
    # If it's a transit mode, then apply the ridership and avo factors (including cross elasticity from PLDV fees)
    if ((.mode == "BU") |
      (.mode == "BRT") |
      (.mode == "RU") |
      (.mode == "RI")) {
      .vmt_fee <- tb_mode$var *
        # Apply AEO adjustments
        dplyr::case_when(
          ((.mode == "BU") | (.mode == "BRT")) ~ aeo_vmt$bus,
          ((.mode == "RU") | (.mode == "RI")) ~ aeo_vmt$rail,
          TRUE ~ 1
        ) *
        # Elasticities, etc.
        (1 + .transit_rider_pct / 100) * (1 + (.vmt_fee / (.fuel_cost_mile + TIME_COST_MI) + .payd_fee / INS_COST_MI + .cong_price / (.fuel_cost_mile + TIME_COST_MI) * CONG_VMT) * CROSS_VMT) *
        # Transit 5D: population density, employment density, diversity, .intersection_design_pct_change, distance
        # If the combined elasticity effect is greater than the max of 25% reduction in VMT (i.e., more negative) then use the max. Else, use the user provided elasticities.
        if (.comb_5d_impact_pct_change < MAX_5D_DR) {
          (1 + MAX_5D_TRANS)
        } else {{
          (1 + .pop_dens_pct_change / 100 * ELAST_DENS_TRANS_POP) *
            (1 + .emp_dens_pct_change / 100 * ELAST_DENS_TRANS_EMP) *
            (1 + .land_use_pct_change / 100 * ELAST_DIVER_TRANS) *
            (1 + .intersection_design_pct_change / 100 * ELAST_DES_TRANS) *
            (1 + .job_access_pct_change / 100 * ELAST_JOBS_TRANS) *
            (1 + .transit_dist_pct_change / 100 * ELAST_DIST_TRANS) *
            (1 + c.pop_dens_pct_change / 100 * ELAST_CDENS_TRANS)
        } *
          # Parking pricing effect
          (1 + .parking_price / pldv_stocks$.parking_price * CROSS_PARK_TRANSIT) *
          # Gas price effect (relative to fuel cost)
          (1 + (.gas_tax / .fuel_cost_mile) * # Only applied to SI/CI/HEV stock (assume PHEV not very sensitive and partially accounted for by a full inclusion of HEV, which is also not as sensitive to .gas_tax price because already switched stock from SI/CI)

            (pldv_stocks$si + pldv_stocks$ci + pldv_stocks$hev) /
            pldv_stocks$tot * CROSS_VMT) /
          (tb_mode$avo * (1 + .transit_avo / 100)) *
          (tb_mode$stock /
            tb_mode$tot_stock) *
          # AV adjustment
          # Remove AV from non-AV PMT
          dplyr::case_when(
            (((.mode == "BU") |
              (.mode == "BRT")) &
              .av_pct > 0) ~ (1 + BUS_AV * .av_pct / 100),
            (((.mode == "RU") |
              (.mode == "RI")) &
              .av_pct > 0) ~ (1 + RAIL_AV * .av_pct / 100),
            TRUE ~ 1
          )        }
    } else if (.mode == "PLDV") { # mode is passenger light duty
      .vmt_fee <- tb_mode$var -
        # Subtract the PMT reduction from a shift to transit (assuming equal per trip PMT)
        (tb_var$at *
          # Apply elasticities, etc.
          (.transit_rider_pct / 100 * PLDV_TRANSIT_RATIO)) *
          # Remove AV from non-AV PMT
          dplyr::case_when(.av_pct > 0 ~ (1 - tb_avshare %>%
            as.numeric() * .av_pct / 100), TRUE ~ 1) *
          # Apply AEO adjustments
          aeo_vmt$ldv *
          (1 + (.vmt_fee / (.fuel_cost_mile + TIME_COST_MI) + .payd_fee / INS_COST_MI) * ELAST_VMT) *
          # Congestion elasticity only applies to a portion of the VMT set by CONG_VMT, so scale the elasticity effect down
          (1 + (.cong_price / (.fuel_cost_mile + TIME_COST_MI) * CONG_VMT) * ELAST_CONG) *
          # Gas price effect (relative to fuel cost)
          (1 + (.gas_tax / .fuel_cost_mile) * ifelse(((.stock == "SIStock") |
            (.stock == "CIStock") |
            (.stock == "HEVStock") |
            ((.stock == "PHEVStock") &
              (ch_phev == 1))), 1, 0) * ELAST_GAS) *
          (1 + (.parking_price / tb_park * ELAST_PARK)) *
          # Auto 5D: population density, employment density, diversity, .intersection_design_pct_change, distance
          # If the combined elasticity effect is greater than the max of 25% reduction in VMT (i.e., more negative) then use the max. Else, use the user provided elasticities.
          if (.comb_5d_impact_pct_change < MAX_5D_DR) {
            (1 + MAX_5D_DR)
          } else {{
            (1 + .pop_dens_pct_change / 100 * ELAST_DENS_DR_POP) *
              (1 + .emp_dens_pct_change / 100 * ELAST_DENS_DR_EMP) *
              (1 + .land_use_pct_change / 100 * ELAST_DIVER_DR) *
              (1 + .intersection_design_pct_change / 100 * ELAST_DES_DR) *
              (1 + .job_access_pct_change / 100 * ELAST_JOBS_DR) *
              (1 + .transit_dist_pct_change / 100 * ELAST_DIST_DR) *
              (1 + c.pop_dens_pct_change / 100 * ELAST_CDENS_DR)
          } *
            # Telework
            (1 + .telework_pct / 100 * MARG_TELEWORK) /
            tb_mode$avo *
            (tb_mode$stock /
              tb_mode$tot_stock)        }
    } else if (.mode == "AV") { # if mode is AV
      .vmt_fee <- (tb_var$pldv
        # Subtract the PMT reduction from a shift to transit (assuming equal per trip PMT)
        -
        tb_var$at *
          (.transit_rider_pct / 100 * PLDV_TRANSIT_RATIO)) *
        # Remove non-AV from AV PMT
        tb_avshare * .av_pct / 100 *
        # Apply AEO adjustments
        aeo_vmt$ldv *
        # Apply elasticities, etc.
        (1 + (.vmt_fee / (.fuel_cost_mile + TIME_COST_MI) + .payd_fee / INS_COST_MI) * ELAST_VMT) *
        # Congestion elasticity only applies to a portion of the VMT set by CONG_VMT, so scale the elasticity effect down
        (1 + (.cong_price / (.fuel_cost_mile + TIME_COST_MI) * CONG_VMT) * ELAST_CONG) *
        (1 + (.gas_tax / .fuel_cost_mile) * ifelse(((.stock == "SIStock") |
          (.stock == "CIStock") |
          (.stock == "HEVStock") |
          ((.stock == "PHEVStock") &
            (ch_phev == 1))), 1, 0) * ELAST_GAS) *
        (1 + (.parking_price / tb_park * ELAST_PARK)) *
        # Auto 5D: population density, employment density, diversity, .intersection_design_pct_change, distance
        # If the combined elasticity effect is greater than the max of 25% reduction in VMT (i.e., more negative) then use the max. Else, use the user provided elasticities.
        if (.comb_5d_impact_pct_change < MAX_5D_DR) {
          (1 + MAX_5D_DR)
        } else {{
          (1 + .pop_dens_pct_change / 100 * ELAST_DENS_DR_POP) *
            (1 + .emp_dens_pct_change / 100 * ELAST_DENS_DR_EMP) *
            (1 + .land_use_pct_change / 100 * ELAST_DIVER_DR) *
            (1 + .intersection_design_pct_change / 100 * ELAST_DES_DR) *
            (1 + .job_access_pct_change / 100 * ELAST_JOBS_DR) *
            (1 + .transit_dist_pct_change / 100 * ELAST_DIST_DR) *
            (1 + c.pop_dens_pct_change / 100 * ELAST_CDENS_DR)
        } *
          # AV increases the VMT slightly, by about 15-20% for local trips (<50 miles)
          VMT_AV / pldv_stocks$avo        }
    } else if (.mode == "SUT") { # if  mode is freight single truck
      .vmt_fee <- tb_mode$var *
        # Apply AEO adjustments
        aeo_vmt$mdt *
        # Apply elasticities, etc.
        # Assumes no shift to other modes because there are other restrictions on that (you probably won't build a new rail line in a city based on a congestion price)
        (1 + .freight_vmt_fee / (.fuel_cost_mile + F_TIME_COST_MI) * ELAST_FVMT * F_FRACT) * # Only apply the VMT fee to fraction occuring in MSP (equivalent to a reduction in elasticity)
        (1 + .parking_price / tb_park * ELAST_PARK) /
        tb_mode$avo *
        (tb_mode$stock /
          tb_mode$tot_stock)
    } else if (.mode == "CUT") {
      .vmt_fee <- tb_mode$var *
        # Apply AEO adjustments
        aeo_vmt$hdt *
        # Apply elasticities, etc.
        # Assumes no shift to other modes because there are other restrictions on that (you probably won't build a new rail line in a city based on a congestion price)
        (1 + .freight_vmt_fee / (.fuel_cost_mile + F_TIME_COST_MI) * ELAST_FVMT) /
        tb_mode$avo *
        (tb_mode$stock /
          tb_mode$tot_stock)
    } else if (.mode == "WALK" | .mode == "BIKE") {
      .vmt_fee <- tb_mode$var *
        # Active (use walk) 5D: population density, employment density, diversity, .intersection_design_pct_change, distance
        # If the combined elasticity effect is greater than the max of 25% reduction in VMT (i.e., more negative) then use the max. Else, use the user provided elasticities.
        if (.comb_5d_impact_pct_change < MAX_5D_DR) {
          (1 + MAX_5D_ACT)
        } else {
          (1 + .pop_dens_pct_change / 100 * ELAST_DENS_ACT_POP) *
            (1 + .emp_dens_pct_change / 100 * ELAST_DENS_ACT_EMP) *
            (1 + .land_use_pct_change / 100 * ELAST_DIVER_ACT) *
            (1 + .intersection_design_pct_change / 100 * ELAST_DES_ACT) *
            (1 + .job_access_pct_change / 100 * ELAST_JOBS_ACT) *
            (1 + .transit_dist_pct_change / 100 * ELAST_DIST_ACT) *
            (1 + c.pop_dens_pct_change / 100 * ELAST_CDENS_ACT)
        }
    } else if (.mode == "BS") {
      .vmt_fee <- tb_mode$var *
        # Apply AEO adjustments
        aeo_vmt$bus / tb_mode$avo *
        (tb_mode$stock /
          tb_mode$tot_stock)
    } else {
      .vmt_fee <- tb_mode$var *
        # Apply AEO adjustments
        ifelse(
          .mode == "FR",
          aeo_vmt$frail,
          aeo_vmt$fship
        ) / tb_mode$avo *
        (tb_mode$stock /
          tb_mode$tot_stock)
    }
  } else if (.mode == "WALK" | .mode == "BIKE") {
    .vmt_fee <- tb_mode$var / tb_mode$avo
  } else { # if scenario is BAU
    .vmt_fee <- tb_mode$var * # miles traveled for given mode
      # Apply AEO adjustments
      dplyr::case_when(
        .mode == "PLDV" ~ aeo_vmt$ldv, # if passenger light duty, use aeo_vmt$ldv
        .mode == "SUT" ~ aeo_vmt$mdt,
        .mode == "CUT" ~ aeo_vmt$hdt,
        .mode == "BU" | .mode == "BRT" ~ aeo_vmt$bus,
        .mode == "RU" | .mode == "RI" ~ aeo_vmt$rail,
        .mode == "FR" ~ aeo_vmt$frail,
        .mode == "MM" | .mode == "AIR" | .mode == "WAT" ~ aeo_vmt$fship,
        TRUE ~ 1
      ) / tb_mode$avo * (tb_mode$stock / tb_mode$tot_stock)
  }
  return(.vmt_fee) # in thousands of miles
}
