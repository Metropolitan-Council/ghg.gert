#' Calculate Dynamic Ride Sharing VMT
#'
#' @param drs percent of trips by dynamic ride sharing (DRS)
#' @param class vehicle class for current mode
#' @param cong congestion price in cents per mile (only applies to congested VMT)
#' @inheritParams calc_vmt
#'
#' @return
#' @export
#'
#' @importFrom tidyselect all_of
#' @importFrom dplyr filter select case_when
#'
calc_drs_vmt <- function(tb,
                         drs,
                         class,
                         fcm,
                         vmt,
                         payd,
                         gas,
                         cong,
                         park = 0,
                         pop_dens = 0,
                         emp_dens = 0,
                         diverse = 0,
                         design = 0,
                         job_access = 0,
                         trans_dist = 0,
                         comb_5d_impact_dr = 0
) {
  # Percent of population using DRS
  vmt <- tb %>%
    dplyr::filter(var == "POP") %>%
    dplyr::select(tidyselect::all_of(YRS)) * drs / 100 *
    tb %>%
    dplyr::filter(var == "DRSShare") %>%
    dplyr::select(tidyselect::all_of(YRS)) %>%
    as.numeric() *
    # SAV per traveller
    tb %>%
    dplyr::filter(var == "SAV") %>%
    dplyr::select(tidyselect::all_of(YRS)) %>%
    as.numeric() *
    # VMT per SAV per year
    tb %>%
    dplyr::filter(var == "VMT") %>%
    dplyr::select(tidyselect::all_of(YRS)) %>%
    as.numeric() *
    # If SAV is PHEV or BEV, then additional VMT for charging
    dplyr::case_when(
      ((class == "PHEV") | (class == "BEV")) ~ (1 + EVCS_VMT),
      TRUE ~ 1
    ) *
    # Elasticities, etc. - assume DRS acts similar to transit in response to changes in PLDV policies
    (1 + (vmt / (fcm + TIME_COST_MI) + payd / INS_COST_MI) * CROSS_VMT) *
    # Transit 5D: population density, employment density, diversity, design, distance
    if (comb_5d_impact_dr < MAX_5D_DR) {
      (1 + MAX_5D_TRANS)
    } else {
      {
        (1 + pop_dens / 100 * ELAST_DENS_TRANS_POP) *
          (1 + emp_dens / 100 * ELAST_DENS_TRANS_EMP) *
          (1 + diverse / 100 * ELAST_DIVER_TRANS) *
          (1 + design / 100 * ELAST_DES_TRANS) *
          (1 + job_access / 100 * ELAST_JOBS_TRANS) *
          (1 + trans_dist / 100 * ELAST_DIST_TRANS) *
          (1 + cpop_dens / 100 * ELAST_CDENS_TRANS)
      } *
        (1 + (vmt / (fcm + TIME_COST_MI) + payd / INS_COST_MI) * ELAST_VMT) *
        # Congestion elasticity only applies to a portion of the VMT set by CONG_VMT, so scale the elasticity effect down
        (1 + (cong / (fcm + TIME_COST_MI) * CONG_VMT) * ELAST_CONG) *
        # Parking price effect
        (1 + park / tb %>%
           dplyr::filter(mode == "PLDV", var == "PARK") %>%
           dplyr::select(tidyselect::all_of(YRS)) * CROSS_PARK_TRANSIT) *
        # Gas price effect
        (1 + (gas / fcm) * # Only applied to SI/CI/HEV stock (assume PHEV not very sensitive and partially accounted for by a full inclusion of HEV, which is also not as sensitive to gas price because already switched stock from SI/CI)
           (tb %>% dplyr::filter(mode == "PLDV", var == "SIStock") %>%
              dplyr::select(tidyselect::all_of(YRS)) +
              tb %>% dplyr::filter(mode == "PLDV", var == "CIStock") %>%
              dplyr::select(tidyselect::all_of(YRS)) +
              tb %>% dplyr::filter(mode == "PLDV", var == "HEVStock") %>%
              dplyr::select(tidyselect::all_of(YRS))) /
           tb %>%
           dplyr::filter(mode == "PLDV", var == "TotStock") %>%
           dplyr::select(tidyselect::all_of(YRS)) * CROSS_VMT)
    } # in thousands of miles (because population in 1000s of persons)
  return(vmt)
}
