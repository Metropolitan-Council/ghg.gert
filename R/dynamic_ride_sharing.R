#' @title Calculate dynamic ride sharing stock sales
#'
#' @param tb input table
#' @param .drs_pct_trip percent of trips by dynamic ride sharing and therefore percent of sales
#'
#' @family transportation
#'
#' @return
#' @export
#' @importFrom dplyr select filter case_when
#'
calc_drs_sales <- function(tb,
                           .drs_pct_trip,
                           .enviro_factors = enviro_factors) {
  # Calculate basic sales (initial + 1/3 fleet replacement) and store as temp variable
  sales <- tb %>%
    dplyr::filter(var == "POP") %>%
    dplyr::select(all_of(YRS)) *
    dplyr::case_when(
      .drs_pct_trip > 0 ~ tb %>%
        dplyr::filter(var == "SAVSales") %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      TRUE ~ 1
    )
  return(sales)
}


#' Calculate Dynamic Ride Sharing VMT
#'
#' @param .drs_pct_trip percent of trips by dynamic ride sharing (DRS)
#' @param .class vehicle class for current mode
#' @inheritParams calc_vmt_forecast
#' @inheritParams calc_drs_sales
#'
#'
#'
#' @family transportation
#' @return
#' @export
#'
#' @importFrom tidyselect all_of
#' @importFrom dplyr filter select case_when
#'
calc_drs_vmt <- function(tb,
                         .drs_pct_trip,
                         .class,
                         .tb_fuel_cost_mile,
                         .vmt_fee,
                         .payd_fee,
                         .gas_tax,
                         .cong_price,
                         .parking_price = 0,
                         .pop_dens_pct_change = 0,
                         .emp_dens_pct_change = 0,
                         .land_use_pct_change = 0,
                         .intersection_design_pct_change = 0,
                         .job_access_pct_change = 0,
                         .transit_dist_pct_change = 0,
                         .comb_5d_impact_pct_change = 0,
                         .enviro_factors = enviro_factors) {
  # Percent of population using DRS
  .vmt_fee <- tb %>%
    dplyr::filter(var == "POP") %>%
    dplyr::select(tidyselect::all_of(YRS)) * .drs_pct_trip  *
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
      ((.class == "PHEV") | (.class == "BEV")) ~ (1 + .enviro_factors$EVCS_VMT),
      TRUE ~ 1
    ) *
    # Elasticities, etc. - assume DRS acts similar to transit in response to changes in PLDV policies
    (1 + (.vmt_fee / (.tb_fuel_cost_mile + .enviro_factors$TIME_COST_MI) + .payd_fee / .enviro_factors$INS_COST_MI) * CROSS_VMT) *
    # Transit 5D: population density, employment density, diversity, .intersection_design_pct_change, distance
    if (.comb_5d_impact_pct_change < .enviro_factors$MAX_5D_DR) {
      (1 + .enviro_factors$MAX_5D_TRANS)
    } else {
      {
        (1 + .pop_dens_pct_change  * ELAST_DENS_TRANS_POP) *
          (1 + .emp_dens_pct_change  * ELAST_DENS_TRANS_EMP) *
          (1 + .land_use_pct_change  * ELAST_DIVER_TRANS) *
          (1 + .intersection_design_pct_change  * ELAST_DES_TRANS) *
          (1 + .job_access_pct_change  * ELAST_JOBS_TRANS) *
          (1 + .transit_dist_pct_change  * ELAST_DIST_TRANS) *
          (1 + c.pop_dens_pct_change  * ELAST_CDENS_TRANS)
      } *
        (1 + (.vmt_fee / (.tb_fuel_cost_mile + .enviro_factors$TIME_COST_MI) + .payd_fee / .enviro_factors$INS_COST_MI) * ELAST_VMT) *
        # Congestion elasticity only applies to a portion of the VMT set by .enviro_factors$CONG_VMT, so scale the elasticity effect down
        (1 + (.cong_price / (.tb_fuel_cost_mile + .enviro_factors$TIME_COST_MI) * .enviro_factors$CONG_VMT) * ELAST_CONG) *
        # Parking price effect
        (1 + .parking_price / tb %>%
          dplyr::filter(mode == "PLDV", var == "PARK") %>%
          dplyr::select(tidyselect::all_of(YRS)) * CROSS_PARK_TRANSIT) *
        # Gas price effect
        (1 + (.gas_tax / .tb_fuel_cost_mile) * # Only applied to SI/CI/HEV stock (assume PHEV not very sensitive and partially accounted for by a full inclusion of HEV, which is also not as sensitive to .gas_tax price because already switched stock from SI/CI)
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
  return(.vmt_fee)
}
