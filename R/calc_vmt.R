#' @title Calculate vehicle miles traveled by mode and power train
#'
#' @param scen character, scenario name
#' @param tb input table
#' @param .mode current mode
#' @param .stock stock for current mode
#' @param .variable variable name - e.g., "VMT"
#' @param fcm fuel cost per mile
#' @param aeo selected EIA Annual Energy Outlook scenario. Default is `"REF"`
#' @param t_avo transit average vehicle occupancy % adjustment. Default is `0`
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
  # If it's not the BAU scenario, then need to run elasticities, etc.
  if (scen != "BAU") {
    # If it's a transit mode, then apply the ridership and avo factors (including cross elasticity from PLDV fees)
    if ((m == "BU") | (m == "BRT") | (m == "RU") | (m == "RI")) {
      vmt <- tb %>%
        dplyr::filter(mode == m, var == v) %>%
        dplyr::select(tidyselect::all_of(YRS)) *
        # Apply AEO adjustments
        dplyr::case_when(
          ((m == "BU") | (m == "BRT")) ~ aeo_factors %>%
            dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "BUS") %>%
            dplyr::select(tidyselect::all_of(YRS)) %>%
            as.numeric(),
          ((m == "RU") | (m == "RI")) ~ aeo_factors %>%
            dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "RAIL") %>%
            dplyr::select(tidyselect::all_of(YRS)) %>%
            as.numeric(),
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
          (1 + park / tb %>%
            dplyr::filter(mode == "PLDV", var == "PARK") %>%
            dplyr::select(tidyselect::all_of(YRS)) * CROSS_PARK_TRANSIT) *
          # Gas price effect (relative to fuel cost)
          (1 + (gas / fcm) * # Only applied to SI/CI/HEV stock (assume PHEV not very sensitive and partially accounted for by a full inclusion of HEV, which is also not as sensitive to gas price because already switched stock from SI/CI)
            (tb %>% dplyr::filter(mode == "PLDV", var == "SIStock") %>%
              dplyr::select(tidyselect::all_of(YRS)) +
              tb %>% dplyr::filter(mode == "PLDV", var == "CIStock") %>%
              dplyr::select(tidyselect::all_of(YRS)) +
              tb %>% dplyr::filter(mode == "PLDV", var == "HEVStock") %>%
              dplyr::select(tidyselect::all_of(YRS))) /
            tb %>%
              dplyr::filter(mode == "PLDV", var == "TotStock") %>%
              dplyr::select(tidyselect::all_of(YRS)) * CROSS_VMT) /
          (tb %>% dplyr::filter(mode == m, var == "AVO") %>%
            dplyr::select(tidyselect::all_of(YRS)) * (1 + t_avo / 100)) *
          (tb %>% dplyr::filter(mode == m, var == s) %>%
            dplyr::select(tidyselect::all_of(YRS)) /
            tb %>%
              dplyr::filter(mode == m, var == "TotStock") %>%
              dplyr::select(tidyselect::all_of(YRS))) *
          # AV adjustment
          # Remove AV from non-AV PMT
          dplyr::case_when(
            (((m == "BU") | (m == "BRT")) & av_pct > 0) ~ (1 + BUS_AV * av_pct / 100),
            (((m == "RU") | (m == "RI")) & av_pct > 0) ~ (1 + RAIL_AV * av_pct / 100),
            TRUE ~ 1
          )        }
    } else if (m == "PLDV") {
      vmt <- (tb %>% dplyr::filter(mode == m, var == v) %>%
        # Subtract the PMT reduction from a shift to transit (assuming equal per trip PMT)
        dplyr::select(tidyselect::all_of(YRS)) -
        (tb %>% dplyr::filter(mode == "AT", var == v) %>%
          dplyr::select(tidyselect::all_of(YRS)) *
          # Apply elasticities, etc.
          (t_rider / 100 * PLDV_TRANSIT_RATIO))) *
        # Remove AV from non-AV PMT
        dplyr::case_when(av_pct > 0 ~ (1 - tb %>% dplyr::filter(var == "AVShare") %>% dplyr::select(tidyselect::all_of(YRS)) %>% as.numeric() * av_pct / 100), TRUE ~ 1) *
        # Apply AEO adjustments
        aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "LDV") %>%
          dplyr::select(tidyselect::all_of(YRS)) *
        (1 + (vmt / (fcm + TIME_COST_MI) + payd / INS_COST_MI) * ELAST_VMT) *
        # Congestion elasticity only applies to a portion of the VMT set by CONG_VMT, so scale the elasticity effect down
        (1 + (cong / (fcm + TIME_COST_MI) * CONG_VMT) * ELAST_CONG) *
        # Gas price effect (relative to fuel cost)
        (1 + (gas / fcm) * ifelse(((s == "SIStock") | (s == "CIStock") | (s == "HEVStock") | ((s == "PHEVStock") & (ch_phev == 1))), 1, 0) * ELAST_GAS) *
        (1 + (park / tb %>%
          dplyr::filter(var == "PARK") %>%
          dplyr::select(tidyselect::all_of(YRS)) * ELAST_PARK)) *
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
          tb %>%
            dplyr::filter(mode == m, var == "AVO") %>%
            dplyr::select(tidyselect::all_of(YRS)) *
          (tb %>% dplyr::filter(mode == m, var == s) %>%
            dplyr::select(tidyselect::all_of(YRS)) /
            tb %>%
              dplyr::filter(mode == m, var == "TotStock") %>%
              dplyr::select(tidyselect::all_of(YRS)))        }
    } else if (m == "AV") {
      vmt <- (tb %>% dplyr::filter(mode == "PLDV", var == v) %>%
        # Subtract the PMT reduction from a shift to transit (assuming equal per trip PMT)
        dplyr::select(tidyselect::all_of(YRS)) -
        tb %>% dplyr::filter(mode == "AT", var == v) %>%
        dplyr::select(tidyselect::all_of(YRS)) * (t_rider / 100 * PLDV_TRANSIT_RATIO)) *
        # Remove non-AV from AV PMT
        tb %>%
          dplyr::filter(var == "AVShare") %>%
          dplyr::select(tidyselect::all_of(YRS)) %>%
          as.numeric() * av_pct / 100 *
        # Apply AEO adjustments
        aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "LDV") %>%
          dplyr::select(tidyselect::all_of(YRS)) *
        # Apply elasticities, etc.
        (1 + (vmt / (fcm + TIME_COST_MI) + payd / INS_COST_MI) * ELAST_VMT) *
        # Congestion elasticity only applies to a portion of the VMT set by CONG_VMT, so scale the elasticity effect down
        (1 + (cong / (fcm + TIME_COST_MI) * CONG_VMT) * ELAST_CONG) *
        (1 + (gas / fcm) * ifelse(((s == "SIStock") |
          (s == "CIStock") | (s == "HEVStock") | ((s == "PHEVStock") & (ch_phev == 1))), 1, 0) * ELAST_GAS) *
        (1 + (park / tb %>%
          dplyr::filter(var == "PARK") %>%
          dplyr::select(tidyselect::all_of(YRS)) * ELAST_PARK)) *
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
          VMT_AV /
          tb %>%
            dplyr::filter(mode == "PLDV", var == "AVO") %>%
            dplyr::select(tidyselect::all_of(YRS))        }
    } else if (m == "SUT") {
      vmt <- tb %>%
        dplyr::filter(mode == m, var == v) %>%
        dplyr::select(tidyselect::all_of(YRS)) *
        # Apply AEO adjustments
        aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "MDT") %>%
          dplyr::select(tidyselect::all_of(YRS)) *
        # Apply elasticities, etc.
        # Assumes no shift to other modes because there are other restrictions on that (you probably won't build a new rail line in a city based on a congestion price)
        (1 + fvmt / (fcm + F_TIME_COST_MI) * ELAST_FVMT * F_FRACT) * # Only apply the VMT fee to fraction occuring in MSP (equivalent to a reduction in elasticity)
        (1 + park / tb %>%
          dplyr::filter(var == "PARK") %>%
          dplyr::select(tidyselect::all_of(YRS)) * ELAST_PARK) /
        tb %>%
          dplyr::filter(mode == m, var == "AVO") %>%
          dplyr::select(tidyselect::all_of(YRS)) *
        (tb %>% dplyr::filter(mode == m, var == s) %>%
          dplyr::select(tidyselect::all_of(YRS)) /
          tb %>%
            dplyr::filter(mode == m, var == "TotStock") %>%
            dplyr::select(tidyselect::all_of(YRS)))
    } else if (m == "CUT") {
      vmt <- tb %>%
        dplyr::filter(mode == m, var == v) %>%
        dplyr::select(tidyselect::all_of(YRS)) *
        # Apply AEO adjustments
        aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "HDT") %>%
          dplyr::select(tidyselect::all_of(YRS)) *
        # Apply elasticities, etc.
        # Assumes no shift to other modes because there are other restrictions on that (you probably won't build a new rail line in a city based on a congestion price)
        (1 + fvmt / (fcm + F_TIME_COST_MI) * ELAST_FVMT) /
        tb %>%
          dplyr::filter(mode == m, var == "AVO") %>%
          dplyr::select(tidyselect::all_of(YRS)) *
        (tb %>% dplyr::filter(mode == m, var == s) %>%
          dplyr::select(tidyselect::all_of(YRS)) /
          tb %>%
            dplyr::filter(mode == m, var == "TotStock") %>%
            dplyr::select(tidyselect::all_of(YRS)))
    } else if (m == "WALK" | m == "BIKE") {
      vmt <- tb %>%
        dplyr::filter(mode == m, var == v) %>%
        dplyr::select(tidyselect::all_of(YRS)) *
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
    } else if (m == "BS") {
      vmt <- tb %>%
        dplyr::filter(mode == m, var == v) %>%
        dplyr::select(tidyselect::all_of(YRS)) *
        # Apply AEO adjustments
        aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "BUS") %>%
          dplyr::select(tidyselect::all_of(YRS)) %>%
          as.numeric() /
        tb %>%
          dplyr::filter(mode == m, var == "AVO") %>%
          dplyr::select(tidyselect::all_of(YRS)) *
        (tb %>% dplyr::filter(mode == m, var == s) %>%
          dplyr::select(tidyselect::all_of(YRS)) /
          tb %>%
            dplyr::filter(mode == m, var == "TotStock") %>%
            dplyr::select(tidyselect::all_of(YRS)))
    } else {
      vmt <- tb %>%
        dplyr::filter(mode == m, var == v) %>%
        dplyr::select(tidyselect::all_of(YRS)) *
        # Apply AEO adjustments
        ifelse(
          m == "FR",
          aeo_factors %>% dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "FRAIL") %>%
            dplyr::select(tidyselect::all_of(YRS)),
          aeo_factors %>% dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "FSHIP") %>%
            dplyr::select(tidyselect::all_of(YRS))
        ) /
        tb %>%
          dplyr::filter(mode == m, var == "AVO") %>%
          dplyr::select(tidyselect::all_of(YRS)) *
        (tb %>% dplyr::filter(mode == m, var == s) %>%
          dplyr::select(tidyselect::all_of(YRS)) /
          tb %>%
            dplyr::filter(mode == m, var == "TotStock") %>%
            dplyr::select(tidyselect::all_of(YRS)))
    }
  } else if (m == "WALK" | m == "BIKE") {
    vmt <- tb %>%
      dplyr::filter(mode == m, var == v) %>%
      dplyr::select(tidyselect::all_of(YRS)) /
      tb %>%
        dplyr::filter(mode == m, var == "AVO") %>%
        dplyr::select(tidyselect::all_of(YRS))
  } else {
    vmt <- tb %>%
      dplyr::filter(mode == m, var == v) %>%
      dplyr::select(tidyselect::all_of(YRS)) *
      # Apply AEO adjustments
      dplyr::case_when(
        m == "PLDV" ~ aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "LDV") %>%
          dplyr::select(tidyselect::all_of(YRS)) %>%
          as.numeric(),
        m == "SUT" ~ aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "MDT") %>%
          dplyr::select(tidyselect::all_of(YRS)) %>%
          as.numeric(),
        m == "CUT" ~ aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "HDT") %>%
          dplyr::select(tidyselect::all_of(YRS)) %>%
          as.numeric(),
        m == "BU" | m == "BRT" ~ aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "BUS") %>%
          dplyr::select(tidyselect::all_of(YRS)) %>%
          as.numeric(),
        m == "RU" | m == "RI" ~ aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "RAIL") %>%
          dplyr::select(tidyselect::all_of(YRS)) %>%
          as.numeric(),
        m == "FR" ~ aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "FRAIL") %>%
          dplyr::select(tidyselect::all_of(YRS)) %>%
          as.numeric(),
        m == "MM" | m == "AIR" | m == "WAT" ~ aeo_factors %>%
          dplyr::filter(AEOScen == aeo, Metric == "VMT", Mode == "FSHIP") %>%
          dplyr::select(tidyselect::all_of(YRS)) %>%
          as.numeric(),
        TRUE ~ 1
      ) /
      tb %>%
        dplyr::filter(mode == m, var == "AVO") %>%
        dplyr::select(tidyselect::all_of(YRS)) *
      (tb %>% dplyr::filter(mode == m, var == s) %>%
        dplyr::select(tidyselect::all_of(YRS)) /
        tb %>%
          dplyr::filter(mode == m, var == "TotStock") %>%
          dplyr::select(tidyselect::all_of(YRS)))
  }
  return(vmt) # in thousands of miles
}
