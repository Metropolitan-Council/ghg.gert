#' @title Calculate vehicle miles traveled by mode and power train
#'
#' @param scen character, scenario name
#' @param tb input table. Should have columns `mode`, `var`, `ctu`,
#'    and one for each year
#' @param .mode current mode
#' @param .stock stock for current mode
#' @param .variable variable name - e.g., "VMT"
#' @param fcm fuel cost per mile
#' @param aeo selected EIA Annual Energy Outlook scenario. Default is `"REF"`
#' @param t_avo transit average vehicle occupancy (AVO) % adjustment. Default is `0`
#' @param t_rider transit ridership % adjustment. Default is `0`
#' @param vmt VMT fee per mile. Default is `0`
#' @param payd  PAYD insurance fee per mile. Default is `0`
#' @param gas gas tax per mile. Default is `0`
#' @param cong congestion price per mile (only applies to an approximation of
#'     congested miles in MSP). Default is `0`
#' @param park Average price of parking based on TBI results and literature -
#'      measured in cents per hour. Default is `0`
#' @param drs percent of trips by auto or transit that are now by dynamic ride sharing (DRS).
#'     Default is `0`
#' @param av_pct percent of trips made by AV. Default is `0`
#' @param fvmt freight VMT fee per mile. Default is `0`
#' @param pop_dens percent change in population density in 2050 wrt BAU. Default is `0`
#' @param emp_dens percent change in employment density in 2050 wrt BAU. Default is `0`
#' @param diverse percent change in land use diversity/mix in 2050 wrt BAU. Default is `0`
#' @param design percent change in intersection design (% 4-way stops) in 2050 wrt BAU. Default is `0`
#' @param job_access percent change in job accessibility in 2050 wrt BAU. Default is `0`
#' @param trans_dist percent change in transit distance in 2050 wrt BAU. Default is `0`
#' @param comb_5d_impact_dr percent change in population density in 2050 wrt BAU
#'      as a measure of composite change in 5Ds on VMT. Default is `0`
#' @param telework percent of people teleworking in 2050. Default is `0`
#' @param ch_phev the current alternative is PHEV, which needs both
#'     gasoline and electric results (using assumption
#'     about gasoline/electric mode split). Default is `0`
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
calc_vmt <- function(scen,
                     tb,
                     .mode,
                     .stock,
                     .variable,
                     fcm,
                     aeo = "REF",
                     t_avo = 0,
                     t_rider = 0,
                     vmt = 0,
                     payd = 0,
                     gas = 0,
                     cong = 0,
                     park = 0,
                     drs = 0,
                     av_pct = 0,
                     fvmt = 0,
                     pop_dens = 0,
                     emp_dens = 0,
                     diverse = 0,
                     design = 0,
                     job_access = 0,
                     trans_dist = 0,
                     comb_5d_impact_dr = 0,
                     telework = 0,
                     ch_phev = 0) {
  browser()
  # Annual energy outlook VMT tables -------
  # Specific to each mode type
  aeo_vmt <- list(
    rail = aeo_factors %>%
      dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "RAIL") %>%
      dplyr::select(tidyselect::all_of(YRS)) %>%
      as.numeric(),
    bus = aeo_factors %>%
      dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "BUS") %>%
      dplyr::select(tidyselect::all_of(YRS)) %>%
      as.numeric(),
    ldv = aeo_factors %>%
      dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "LDV") %>%
      dplyr::select(tidyselect::all_of(YRS)) %>%
      as.numeric(),
    mdt = aeo_factors %>%
      dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "MDT") %>%
      dplyr::select(tidyselect::all_of(YRS)) %>%
      as.numeric(),
    hdt = aeo_factors %>%
      dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "HDT") %>%
      dplyr::select(tidyselect::all_of(YRS)) %>%
      as.numeric(),
    frail = aeo_factors %>%
      dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "FRAIL") %>%
      dplyr::select(tidyselect::all_of(YRS)) %>%
      as.numeric(),
    fship = aeo_factors %>%
      dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "FSHIP") %>%
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
    park = tb %>%
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
  if (scen != "BAU") {
    # If it's a transit mode, then apply the ridership and avo factors (including cross elasticity from PLDV fees)
    if ((.mode == "BU") |
      (.mode == "BRT") |
      (.mode == "RU") |
      (.mode == "RI")) {
      vmt <- tb_mode$var *
        # Apply AEO adjustments
        dplyr::case_when(
          ((.mode == "BU") | (.mode == "BRT")) ~ aeo_vmt$bus,
          ((.mode == "RU") | (.mode == "RI")) ~ aeo_vmt$rail,
          TRUE ~ 1
        ) *
        # Elasticities, etc.
        (1 + t_rider / 100) * (1 + (vmt / (fcm + TIME_COST_MI) + payd / INS_COST_MI + cong / (fcm + TIME_COST_MI) * CONG_VMT) * CROSS_VMT) *
        # Transit 5D: population density, employment density, diversity, design, distance
        # If the combined elasticity effect is greater than the max of 25% reduction in VMT (i.e., more negative) then use the max. Else, use the user provided elasticities.
        if (comb_5d_impact_dr < MAX_5D_DR) {
          (1 + MAX_5D_TRANS)
        } else {{
          (1 + pop_dens / 100 * ELAST_DENS_TRANS_POP) *
            (1 + emp_dens / 100 * ELAST_DENS_TRANS_EMP) *
            (1 + diverse / 100 * ELAST_DIVER_TRANS) *
            (1 + design / 100 * ELAST_DES_TRANS) *
            (1 + job_access / 100 * ELAST_JOBS_TRANS) *
            (1 + trans_dist / 100 * ELAST_DIST_TRANS) *
            (1 + cpop_dens / 100 * ELAST_CDENS_TRANS)
        } *
          # Parking pricing effect
          (1 + park / pldv_stocks$park * CROSS_PARK_TRANSIT) *
          # Gas price effect (relative to fuel cost)
          (1 + (gas / fcm) * # Only applied to SI/CI/HEV stock (assume PHEV not very sensitive and partially accounted for by a full inclusion of HEV, which is also not as sensitive to gas price because already switched stock from SI/CI)

            (pldv_stocks$si + pldv_stocks$ci + pldv_stocks$hev) /
            pldv_stocks$tot * CROSS_VMT) /
          (tb_mode$avo * (1 + t_avo / 100)) *
          (tb_mode$stock /
            tb_mode$tot_stock) *
          # AV adjustment
          # Remove AV from non-AV PMT
          dplyr::case_when(
            (((.mode == "BU") |
              (.mode == "BRT")) &
              av_pct > 0) ~ (1 + BUS_AV * av_pct / 100),
            (((.mode == "RU") |
              (.mode == "RI")) &
              av_pct > 0) ~ (1 + RAIL_AV * av_pct / 100),
            TRUE ~ 1
          )        }
    } else if (.mode == "PLDV") { # mode is passenger light duty
      vmt <- tb_mode$var -
        # Subtract the PMT reduction from a shift to transit (assuming equal per trip PMT)
        (tb_var$at *
          # Apply elasticities, etc.
          (t_rider / 100 * PLDV_TRANSIT_RATIO)) *
          # Remove AV from non-AV PMT
          dplyr::case_when(av_pct > 0 ~ (1 - tb_avshare %>%
            as.numeric() * av_pct / 100), TRUE ~ 1) *
          # Apply AEO adjustments
          aeo_vmt$ldv *
          (1 + (vmt / (fcm + TIME_COST_MI) + payd / INS_COST_MI) * ELAST_VMT) *
          # Congestion elasticity only applies to a portion of the VMT set by CONG_VMT, so scale the elasticity effect down
          (1 + (cong / (fcm + TIME_COST_MI) * CONG_VMT) * ELAST_CONG) *
          # Gas price effect (relative to fuel cost)
          (1 + (gas / fcm) * ifelse(((.stock == "SIStock") |
            (.stock == "CIStock") |
            (.stock == "HEVStock") |
            ((.stock == "PHEVStock") &
              (ch_phev == 1))), 1, 0) * ELAST_GAS) *
          (1 + (park / tb_park * ELAST_PARK)) *
          # Auto 5D: population density, employment density, diversity, design, distance
          # If the combined elasticity effect is greater than the max of 25% reduction in VMT (i.e., more negative) then use the max. Else, use the user provided elasticities.
          if (comb_5d_impact_dr < MAX_5D_DR) {
            (1 + MAX_5D_DR)
          } else {{
            (1 + pop_dens / 100 * ELAST_DENS_DR_POP) *
              (1 + emp_dens / 100 * ELAST_DENS_DR_EMP) *
              (1 + diverse / 100 * ELAST_DIVER_DR) *
              (1 + design / 100 * ELAST_DES_DR) *
              (1 + job_access / 100 * ELAST_JOBS_DR) *
              (1 + trans_dist / 100 * ELAST_DIST_DR) *
              (1 + cpop_dens / 100 * ELAST_CDENS_DR)
          } *
            # Telework
            (1 + telework / 100 * MARG_TELEWORK) /
            tb_mode$avo *
            (tb_mode$stock /
              tb_mode$tot_stock)        }
    } else if (.mode == "AV") { # if mode is AV
      vmt <- (tb_var$pldv
        # Subtract the PMT reduction from a shift to transit (assuming equal per trip PMT)
        -
        tb_var$at *
          (t_rider / 100 * PLDV_TRANSIT_RATIO)) *
        # Remove non-AV from AV PMT
        tb_avshare * av_pct / 100 *
        # Apply AEO adjustments
        aeo_vmt$ldv *
        # Apply elasticities, etc.
        (1 + (vmt / (fcm + TIME_COST_MI) + payd / INS_COST_MI) * ELAST_VMT) *
        # Congestion elasticity only applies to a portion of the VMT set by CONG_VMT, so scale the elasticity effect down
        (1 + (cong / (fcm + TIME_COST_MI) * CONG_VMT) * ELAST_CONG) *
        (1 + (gas / fcm) * ifelse(((.stock == "SIStock") |
          (.stock == "CIStock") |
          (.stock == "HEVStock") |
          ((.stock == "PHEVStock") &
            (ch_phev == 1))), 1, 0) * ELAST_GAS) *
        (1 + (park / tb_park * ELAST_PARK)) *
        # Auto 5D: population density, employment density, diversity, design, distance
        # If the combined elasticity effect is greater than the max of 25% reduction in VMT (i.e., more negative) then use the max. Else, use the user provided elasticities.
        if (comb_5d_impact_dr < MAX_5D_DR) {
          (1 + MAX_5D_DR)
        } else {{
          (1 + pop_dens / 100 * ELAST_DENS_DR_POP) *
            (1 + emp_dens / 100 * ELAST_DENS_DR_EMP) *
            (1 + diverse / 100 * ELAST_DIVER_DR) *
            (1 + design / 100 * ELAST_DES_DR) *
            (1 + job_access / 100 * ELAST_JOBS_DR) *
            (1 + trans_dist / 100 * ELAST_DIST_DR) *
            (1 + cpop_dens / 100 * ELAST_CDENS_DR)
        } *
          # AV increases the VMT slightly, by about 15-20% for local trips (<50 miles)
          VMT_AV / pldv_stocks$avo        }
    } else if (.mode == "SUT") { # if  mode is freight single truck
      vmt <- tb_mode$var *
        # Apply AEO adjustments
        aeo_vmt$mdt *
        # Apply elasticities, etc.
        # Assumes no shift to other modes because there are other restrictions on that (you probably won't build a new rail line in a city based on a congestion price)
        (1 + fvmt / (fcm + F_TIME_COST_MI) * ELAST_FVMT * F_FRACT) * # Only apply the VMT fee to fraction occuring in MSP (equivalent to a reduction in elasticity)
        (1 + park / tb_park * ELAST_PARK) /
        tb_mode$avo *
        (tb_mode$stock /
          tb_mode$tot_stock)
    } else if (.mode == "CUT") {
      vmt <- tb_mode$var *
        # Apply AEO adjustments
        aeo_vmt$hdt *
        # Apply elasticities, etc.
        # Assumes no shift to other modes because there are other restrictions on that (you probably won't build a new rail line in a city based on a congestion price)
        (1 + fvmt / (fcm + F_TIME_COST_MI) * ELAST_FVMT) /
        tb_mode$avo *
        (tb_mode$stock /
          tb_mode$tot_stock)
    } else if (.mode == "WALK" | .mode == "BIKE") {
      vmt <- tb_mode$var *
        # Active (use walk) 5D: population density, employment density, diversity, design, distance
        # If the combined elasticity effect is greater than the max of 25% reduction in VMT (i.e., more negative) then use the max. Else, use the user provided elasticities.
        if (comb_5d_impact_dr < MAX_5D_DR) {
          (1 + MAX_5D_ACT)
        } else {
          (1 + pop_dens / 100 * ELAST_DENS_ACT_POP) *
            (1 + emp_dens / 100 * ELAST_DENS_ACT_EMP) *
            (1 + diverse / 100 * ELAST_DIVER_ACT) *
            (1 + design / 100 * ELAST_DES_ACT) *
            (1 + job_access / 100 * ELAST_JOBS_ACT) *
            (1 + trans_dist / 100 * ELAST_DIST_ACT) *
            (1 + cpop_dens / 100 * ELAST_CDENS_ACT)
        }
    } else if (.mode == "BS") {
      vmt <- tb_mode$var *
        # Apply AEO adjustments
        aeo_vmt$bus / tb_mode$avo *
        (tb_mode$stock /
          tb_mode$tot_stock)
    } else {
      vmt <- tb_mode$var *
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
    vmt <- tb_mode$var / tb_mode$avo
  } else { # if scenario is BAU
    vmt <- tb_mode$var * # miles traveled for given mode
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
  return(vmt) # in thousands of miles
}
