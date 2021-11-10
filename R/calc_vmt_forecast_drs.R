#' Calculate Dynamic Ride Sharing VMT
#'
#' @param .drs_pct percent of trips by dynamic ride sharing (DRS)
#' @param .class vehicle class for current mode
#' @inheritParams calc_vmt_forecast
#' @inheritParams calc_drs_sales
#' @description Dynamic ride sharing vehicle miles traveled is calculated by population,
#'     not existing and projected VMT.
#'
#'
#' @family transportation, dynamic ride share
#' @return
#' @export
#'
#' @importFrom tidyselect all_of
#' @importFrom dplyr filter select case_when
#'
calc_vmt_forecast_drs <- function(.scenario,
                                  tb,
                                  .mode,
                                  .stock,
                                  .variable,
                                  .tb_fuel_cost_mile,
                                  .aeo_scenario = "REF",
                                  .transit_avo = 0,
                                  .transit_rider_pct = 0,
                                  .vmt_fee = 0,
                                  .payd_fee = 0,
                                  .gas_tax = 0,
                                  .cong_price = 0,
                                  .parking_price = 0,
                                  .drs_pct = 0,
                                  .drs_fuel_type = "",
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
                                  .phev_electric = FALSE,
                                  .enviro_factors = enviro_factors) {

  # dynamic ride share ----
  browser()
  # vmt = (POP * .drs_pct) * DRSShare * DRS veh. per traveler *
  # VMT per DRS veh. per year *  adj for charging *
  #  1 + (vmt_adj + payd_adj) * CROSS_VMT) *
  # land_use_adj * cong_adj * parking_adj * gas_adj

  drs_miles_per_veh <- tb %>%
    dplyr::filter(
      var == "VMT"
    ) %>%
    mutate(
      miles_per_drs_veh = value,
      scenario = .scenario
    ) %>%
    select(scenario, mode, ctu, year,
           aeo_mode, type, miles_traveled) %>%
    unique()

  drs_per_traveler <- tb %>%
    filter(var == "DRS") %>%
    select(year, ctu, sav_per = value)

  drs_charging <- ifelse(
    .drs_fuel_type %in% c("BEV", "PHEV"),
    1 + .enviro_factors$EVCS_VMT, 1
  )

  fc_adjustments <- vmt_road_policy(
    .mode = .mode,
    .tb_vmt = drs_miles_per_veh,
    .pass_tb = tb,
    .tb_fuel_cost_mile = .tb_fuel_cost_mile,
    .vmt_fee = .vmt_fee,
    .cong_price = .cong_price,
    .gas_tax = .gas_tax,
    .payd_fee = .payd_fee,
    .stock = .stock
  )

  land_use <- vmt_land_use_change(
    .mode = .mode,
    .type = "TRANSIT",
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_pct_change = .land_use_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change
  )

  parking <- vmt_parking_policy(
    .pass_tb = tb,
    .mode = .mode,
    .parking_price = .parking_price
  )


  tb_pop_drsshare <- tb %>%
    filter(var %in% c("POP", "DRSShare")) %>%
    tidyr::pivot_wider(names_from = var,
                       values_from = value) %>%
    select(ctu, year, POP, DRSShare)

  tb_fin <- drs_miles_per_veh %>%
    left_join(fc_adjustments, by = c("ctu", "year")) %>%
    left_join(tb_pop_drsshare, by = c("ctu", "year")) %>%
    left_join(land_use, by = c("year")) %>%
    left_join(parking, by = c("year", "ctu")) %>%
    unique() %>%
    rowwise() %>%
    mutate(
      stock = .stock,
      mode = .mode,
      drs_vmt = (POP * DRSShare) * .drs_pct *
        drs_charging *
        miles_per_drs_veh *
        vmt_fee_elas_adj *
        vmt_fee_cross_adj *
        land_use_adj *
        cong_adjust *
        gas_adj *
        park_price_adj
    ) %>%
    select(type, stock, scenario,
           ctu, year, mode, aeo_mode, vmt = drs_vmt)

  return(tb_fin)




  # # Percent of population using DRS
  # .vmt_fee <- tb %>%
  #   dplyr::filter(var == "POP") %>%
  #   dplyr::select(tidyselect::all_of(YRS)) * .drs_pct *
  #   tb %>%
  #     dplyr::filter(var == "DRSShare") %>%
  #     dplyr::select(tidyselect::all_of(YRS)) %>%
  #     as.numeric() *
  #   # DRS vehicles per traveller
  #   tb %>%
  #     dplyr::filter(var == "DRS") %>%
  #     dplyr::select(tidyselect::all_of(YRS)) %>%
  #     as.numeric() *
  #   # VMT per DRS vehicle per year
  #   tb %>%
  #     dplyr::filter(var == "VMT") %>%
  #     dplyr::select(tidyselect::all_of(YRS)) %>%
  #     as.numeric() *
  #   # If DRS is PHEV or BEV, then additional VMT for charging
  #   dplyr::case_when(
  #     ((.class == "PHEV") | (.class == "BEV")) ~ (1 + .enviro_factors$EVCS_VMT),
  #     TRUE ~ 1
  #   ) *
  #   # Elasticities, etc. - assume DRS acts similar to transit in response to changes in PLDV policies
  #   (1 + (.vmt_fee / (.tb_fuel_cost_mile + .enviro_factors$TIME_COST_MI) + .payd_fee / .enviro_factors$INS_COST_MI) * CROSS_VMT) *
  #   # Transit 5D: population density, employment density, diversity, .intersection_design_pct_change, distance
  #   if (.comb_5d_impact_pct_change < .enviro_factors$MAX_5D_DR) {
  #     (1 + .enviro_factors$MAX_5D_TRANS)
  #   } else {
  #     {
  #       (1 + .pop_dens_pct_change * ELAST_DENS_TRANS_POP) *
  #         (1 + .emp_dens_pct_change * ELAST_DENS_TRANS_EMP) *
  #         (1 + .land_use_pct_change * ELAST_DIVER_TRANS) *
  #         (1 + .intersection_design_pct_change * ELAST_DES_TRANS) *
  #         (1 + .job_access_pct_change * ELAST_JOBS_TRANS) *
  #         (1 + .transit_dist_pct_change * ELAST_DIST_TRANS) *
  #         (1 + c.pop_dens_pct_change * ELAST_CDENS_TRANS)
  #     } *
  #       (1 + (.vmt_fee / (.tb_fuel_cost_mile + .enviro_factors$TIME_COST_MI) + .payd_fee / .enviro_factors$INS_COST_MI) * ELAST_VMT) *
  #       # Congestion elasticity only applies to a portion of the VMT set by .enviro_factors$CONG_VMT, so scale the elasticity effect down
  #       (1 + (.cong_price / (.tb_fuel_cost_mile + .enviro_factors$TIME_COST_MI) * .enviro_factors$CONG_VMT) * ELAST_CONG) *
  #       # Parking price effect
  #       (1 + .parking_price / tb %>%
  #         dplyr::filter(mode == "PLDV", var == "PARK") %>%
  #         dplyr::select(tidyselect::all_of(YRS)) * CROSS_PARK_TRANSIT) *
  #       # Gas price effect
  #       (1 + (.gas_tax / .tb_fuel_cost_mile) * # Only applied to SI/CI/HEV stock (assume PHEV not very sensitive and partially accounted for by a full inclusion of HEV, which is also not as sensitive to .gas_tax price because already switched stock from SI/CI)
  #         (tb %>% dplyr::filter(mode == "PLDV", var == "SIStock") %>%
  #           dplyr::select(tidyselect::all_of(YRS)) +
  #           tb %>% dplyr::filter(mode == "PLDV", var == "CIStock") %>%
  #           dplyr::select(tidyselect::all_of(YRS)) +
  #           tb %>% dplyr::filter(mode == "PLDV", var == "HEVStock") %>%
  #           dplyr::select(tidyselect::all_of(YRS))) /
  #         tb %>%
  #           dplyr::filter(mode == "PLDV", var == "TotStock") %>%
  #           dplyr::select(tidyselect::all_of(YRS)) * CROSS_VMT)
  #   } # in thousands of miles (because population in 1000s of persons)
  # return(.vmt_fee)
}
