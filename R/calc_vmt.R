#' @title Calculate vehicle miles traveled by mode and power train
#'
#' @param scen character, scenario name
#' @param tb input table
#' @param m current mode
#' @param s stock for current mode
#' @param v variable name - e.g., "VMT"
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
#' @param drs percent of trips by auto or transit that are now by DRS. Default is `0`
#' @param av percent of trips made by AV. Default is `0`
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
#'
calc_vmt <- function(scen,
                     tb,
                     m,
                     s,
                     v,
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
                     av = 0,
                     fvmt = 0,
                     pop_dens = 0,
                     emp_dens = 0,
                     diverse = 0,
                     design = 0,
                     job_access = 0,
                     trans_dist = 0,
                     comb_5d_impact_dr = 0,
                     telework = 0,
                     ch_phev = 0
) {
  # If it's not the BAU scenario, then need to run elasticities, etc.
  if (scen != "BAU") {
    # If it's a transit mode, then apply the ridership and avo factors (including cross elasticity from PLDV fees)
    if ((m == "BU") | (m == "BRT") | (m == "RU") | (m == "RI")) {
      vmt <- tb %>%
        filter(mode == m, var == v) %>%
        select(all_of(YRS)) *
        # Apply AEO adjustments
        case_when(
          ((m == "BU") | (m == "BRT")) ~ aeo_factors %>%
            filter(AEOScen == aeo, Metric == "VMT", Mode == "BUS") %>%
            select(all_of(YRS)) %>%
            as.numeric(),
          ((m == "RU") | (m == "RI")) ~ aeo_factors %>%
            filter(AEOScen == aeo, Metric == "VMT", Mode == "RAIL") %>%
            select(all_of(YRS)) %>%
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
               filter(mode == "PLDV", var == "PARK") %>%
               select(all_of(YRS)) * CROSS_PARK_TRANSIT) *
            # Gas price effect (relative to fuel cost)
            (1 + (gas / fcm) * # Only applied to SI/CI/HEV stock (assume PHEV not very sensitive and partially accounted for by a full inclusion of HEV, which is also not as sensitive to gas price because already switched stock from SI/CI)
               (tb %>% filter(mode == "PLDV", var == "SIStock") %>%
                  select(all_of(YRS)) +
                  tb %>% filter(mode == "PLDV", var == "CIStock") %>%
                  select(all_of(YRS)) +
                  tb %>% filter(mode == "PLDV", var == "HEVStock") %>%
                  select(all_of(YRS))) /
               tb %>%
               filter(mode == "PLDV", var == "TotStock") %>%
               select(all_of(YRS)) * CROSS_VMT) /
            (tb %>% filter(mode == m, var == "AVO") %>%
               select(all_of(YRS)) * (1 + t_avo / 100)) *
            (tb %>% filter(mode == m, var == s) %>%
               select(all_of(YRS)) /
               tb %>%
               filter(mode == m, var == "TotStock") %>%
               select(all_of(YRS))) *
            # AV adjustment
            # Remove AV from non-AV PMT
            case_when(
              (((m == "BU") | (m == "BRT")) & av > 0) ~ (1 + BUS_AV * av / 100),
              (((m == "RU") | (m == "RI")) & av > 0) ~ (1 + RAIL_AV * av / 100),
              TRUE ~ 1
            )        }
    } else if (m == "PLDV") {
      vmt <- (tb %>% filter(mode == m, var == v) %>%
                # Subtract the PMT reduction from a shift to transit (assuming equal per trip PMT)
                select(all_of(YRS)) -
                (tb %>% filter(mode == "AT", var == v) %>%
                   select(all_of(YRS)) *
                   # Apply elasticities, etc.
                   (t_rider / 100 * PLDV_TRANSIT_RATIO))) *
        # Remove AV from non-AV PMT
        case_when(av > 0 ~ (1 - tb %>% filter(var == "AVShare") %>% select(all_of(YRS)) %>% as.numeric() * av / 100), TRUE ~ 1) *
        # Apply AEO adjustments
        aeo_factors %>%
        filter(AEOScen == aeo, Metric == "VMT", Mode == "LDV") %>%
        select(all_of(YRS)) *
        (1 + (vmt / (fcm + TIME_COST_MI) + payd / INS_COST_MI) * ELAST_VMT) *
        # Congestion elasticity only applies to a portion of the VMT set by CONG_VMT, so scale the elasticity effect down
        (1 + (cong / (fcm + TIME_COST_MI) * CONG_VMT) * ELAST_CONG) *
        # Gas price effect (relative to fuel cost)
        (1 + (gas / fcm) * ifelse(((s == "SIStock") | (s == "CIStock") | (s == "HEVStock") | ((s == "PHEVStock") & (ch_phev == 1))), 1, 0) * ELAST_GAS) *
        (1 + (park / tb %>%
                filter(var == "PARK") %>%
                select(all_of(YRS)) * ELAST_PARK)) *
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
            filter(mode == m, var == "AVO") %>%
            select(all_of(YRS)) *
            (tb %>% filter(mode == m, var == s) %>%
               select(all_of(YRS)) /
               tb %>%
               filter(mode == m, var == "TotStock") %>%
               select(all_of(YRS)))        }
    } else if (m == "AV") {
      vmt <- (tb %>% filter(mode == "PLDV", var == v) %>%
                # Subtract the PMT reduction from a shift to transit (assuming equal per trip PMT)
                select(all_of(YRS)) -
                tb %>% filter(mode == "AT", var == v) %>%
                select(all_of(YRS)) * (t_rider / 100 * PLDV_TRANSIT_RATIO)) *
        # Remove non-AV from AV PMT
        tb %>%
        filter(var == "AVShare") %>%
        select(all_of(YRS)) %>%
        as.numeric() * av / 100 *
        # Apply AEO adjustments
        aeo_factors %>%
        filter(AEOScen == aeo, Metric == "VMT", Mode == "LDV") %>%
        select(all_of(YRS)) *
        # Apply elasticities, etc.
        (1 + (vmt / (fcm + TIME_COST_MI) + payd / INS_COST_MI) * ELAST_VMT) *
        # Congestion elasticity only applies to a portion of the VMT set by CONG_VMT, so scale the elasticity effect down
        (1 + (cong / (fcm + TIME_COST_MI) * CONG_VMT) * ELAST_CONG) *
        (1 + (gas / fcm) * ifelse(((s == "SIStock") | (s == "CIStock") | (s == "HEVStock") | ((s == "PHEVStock") & (ch_phev == 1))), 1, 0) * ELAST_GAS) *
        (1 + (park / tb %>%
                filter(var == "PARK") %>%
                select(all_of(YRS)) * ELAST_PARK)) *
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
            filter(mode == "PLDV", var == "AVO") %>%
            select(all_of(YRS))        }
    } else if (m == "SUT") {
      vmt <- tb %>%
        filter(mode == m, var == v) %>%
        select(all_of(YRS)) *
        # Apply AEO adjustments
        aeo_factors %>%
        filter(AEOScen == aeo, Metric == "VMT", Mode == "MDT") %>%
        select(all_of(YRS)) *
        # Apply elasticities, etc.
        # Assumes no shift to other modes because there are other restrictions on that (you probably won't build a new rail line in a city based on a congestion price)
        (1 + fvmt / (fcm + F_TIME_COST_MI) * ELAST_FVMT * F_FRACT) * # Only apply the VMT fee to fraction occuring in MSP (equivalent to a reduction in elasticity)
        (1 + park / tb %>%
           filter(var == "PARK") %>%
           select(all_of(YRS)) * ELAST_PARK) /
        tb %>%
        filter(mode == m, var == "AVO") %>%
        select(all_of(YRS)) *
        (tb %>% filter(mode == m, var == s) %>%
           select(all_of(YRS)) /
           tb %>%
           filter(mode == m, var == "TotStock") %>%
           select(all_of(YRS)))
    } else if (m == "CUT") {
      vmt <- tb %>%
        filter(mode == m, var == v) %>%
        select(all_of(YRS)) *
        # Apply AEO adjustments
        aeo_factors %>%
        filter(AEOScen == aeo, Metric == "VMT", Mode == "HDT") %>%
        select(all_of(YRS)) *
        # Apply elasticities, etc.
        # Assumes no shift to other modes because there are other restrictions on that (you probably won't build a new rail line in a city based on a congestion price)
        (1 + fvmt / (fcm + F_TIME_COST_MI) * ELAST_FVMT) /
        tb %>%
        filter(mode == m, var == "AVO") %>%
        select(all_of(YRS)) *
        (tb %>% filter(mode == m, var == s) %>%
           select(all_of(YRS)) /
           tb %>%
           filter(mode == m, var == "TotStock") %>%
           select(all_of(YRS)))
    } else if (m == "WALK" | m == "BIKE") {
      vmt <- tb %>%
        filter(mode == m, var == v) %>%
        select(all_of(YRS)) *
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
        filter(mode == m, var == v) %>%
        select(all_of(YRS)) *
        # Apply AEO adjustments
        aeo_factors %>%
        filter(AEOScen == aeo, Metric == "VMT", Mode == "BUS") %>%
        select(all_of(YRS)) %>%
        as.numeric() /
        tb %>%
        filter(mode == m, var == "AVO") %>%
        select(all_of(YRS)) *
        (tb %>% filter(mode == m, var == s) %>%
           select(all_of(YRS)) /
           tb %>%
           filter(mode == m, var == "TotStock") %>%
           select(all_of(YRS)))
    } else {
      vmt <- tb %>%
        filter(mode == m, var == v) %>%
        select(all_of(YRS)) *
        # Apply AEO adjustments
        ifelse(
          m == "FR",
          aeo_factors %>% filter(AEOScen == aeo, Metric == "VMT", Mode == "FRAIL") %>%
            select(all_of(YRS)),
          aeo_factors %>% filter(AEOScen == aeo, Metric == "VMT", Mode == "FSHIP") %>%
            select(all_of(YRS))
        ) /
        tb %>%
        filter(mode == m, var == "AVO") %>%
        select(all_of(YRS)) *
        (tb %>% filter(mode == m, var == s) %>%
           select(all_of(YRS)) /
           tb %>%
           filter(mode == m, var == "TotStock") %>%
           select(all_of(YRS)))
    }
  } else if (m == "WALK" | m == "BIKE") {
    vmt <- tb %>%
      filter(mode == m, var == v) %>%
      select(all_of(YRS)) /
      tb %>%
      filter(mode == m, var == "AVO") %>%
      select(all_of(YRS))
  } else {
    vmt <- tb %>%
      filter(mode == m, var == v) %>%
      select(all_of(YRS)) *
      # Apply AEO adjustments
      case_when(
        m == "PLDV" ~ aeo_factors %>%
          filter(AEOScen == aeo, Metric == "VMT", Mode == "LDV") %>%
          select(all_of(YRS)) %>%
          as.numeric(),
        m == "SUT" ~ aeo_factors %>%
          filter(AEOScen == aeo, Metric == "VMT", Mode == "MDT") %>%
          select(all_of(YRS)) %>%
          as.numeric(),
        m == "CUT" ~ aeo_factors %>%
          filter(AEOScen == aeo, Metric == "VMT", Mode == "HDT") %>%
          select(all_of(YRS)) %>%
          as.numeric(),
        m == "BU" | m == "BRT" ~ aeo_factors %>%
          filter(AEOScen == aeo, Metric == "VMT", Mode == "BUS") %>%
          select(all_of(YRS)) %>%
          as.numeric(),
        m == "RU" | m == "RI" ~ aeo_factors %>%
          filter(AEOScen == aeo, Metric == "VMT", Mode == "RAIL") %>%
          select(all_of(YRS)) %>%
          as.numeric(),
        m == "FR" ~ aeo_factors %>%
          filter(AEOScen == aeo, Metric == "VMT", Mode == "FRAIL") %>%
          select(all_of(YRS)) %>%
          as.numeric(),
        m == "MM" | m == "AIR" | m == "WAT" ~ aeo_factors %>%
          filter(AEOScen == aeo, Metric == "VMT", Mode == "FSHIP") %>%
          select(all_of(YRS)) %>%
          as.numeric(),
        TRUE ~ 1
      ) /
      tb %>%
      filter(mode == m, var == "AVO") %>%
      select(all_of(YRS)) *
      (tb %>% filter(mode == m, var == s) %>%
         select(all_of(YRS)) /
         tb %>%
         filter(mode == m, var == "TotStock") %>%
         select(all_of(YRS)))
  }
  return(vmt) # in thousands of miles
}
