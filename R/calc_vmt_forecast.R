#' @title Calculate vehicle miles traveled by mode and power train
#'
#' @param .scenario character, scenario name
#' @param tb input table for appropriate mode type. Should have columns `mode`, `var`, `ctu`,
#'    and one for each year. Package provided datasets `transportation_data$passenger` or
#'    `transportation_data$freight` are suitable.
#' @param .mode current mode
#' @param .stock stock for current mode
#' @param .variable variable name - e.g., "VMT"
#' @param .tb_fuel_cost_mile table with fuel cost per mile
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
#' @param .telework_pct percent of people teleworking in 2050. Default is `0`
#' @param ch_phev the current alternative is PHEV, which needs both
#'     gasoline and electric results (using assumption
#'     about gasoline/electric mode split). Default is `0`
#' @param .enviro_factors list of environmental factors. Default is `enviro_factors`, included in this package.
#'
### Eqn: (PMT in 1000 mi) x Pr(stock by fuel) / AVO
#'
#' @return a tibble with columns `scenario`, `ctu`, `year`, `aeo_mode`, `type`, `vmt`,
#' @export
#' @family transportation
#'
#'
#' @importFrom dplyr filter select case_when
#' @importFrom tidyselect all_of
#' @importFrom rlang sym
#'
calc_vmt_forecast <- function(.scenario,
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
                              ch_phev = 0,
                              .enviro_factors = enviro_factors) {
  # browser()


  tb_vmt <- tb %>%
    filter(
      mode == .mode,
      var == .variable
    ) %>%
    mutate(
      miles_traveled = value,
      scenario = .scenario
    ) %>%
    select(scenario, mode, ctu, year, aeo_mode, type, miles_traveled)

  # If it's not the BAU scenario, then need to run elasticities, etc.
  if (.scenario != "BAU") {
    # Not BAU ----
    # browser()



    if ((.mode == "BU") |
      (.mode == "BRT") |
      (.mode == "RU") |
      (.mode == "RI")) {
      # bus and rail -----
      # If it's a transit mode, then apply the ridership and avo factors (including cross elasticity from PLDV fees)

      # browser()

      # formula is such
      # transit vmt = PMT * aeo_adj * transit_adj *
      # (1 + (vmt_fee_adjust +  payd_ins_adj + cong_adjust * cross_vmt)) *
      # land_use_adj * park_price_adj * gas_adj /
      # occupancy_adj / av_adj
      #


      ann_energy_outlook <- vmt_annual_energy_outlook(
        tb = transportation_data$passenger,
        .aeo_scenario = .aeo_scenario,
        .mode = .mode
      )

      trans_rider <- vmt_transit_ridership(
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .transit_rider_pct = .transit_rider_pct
      )

      fc_adjustments <- vmt_road_policy(
        .mode = .mode,
        .tb_vmt = tb_vmt,
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
        .mode = .mode,
        .parking_price = .parking_price
      )

      veh_occupancy <- vmt_vehicle_occupancy(
        tb = transportation_data$passenger,
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .gas_tax = .gas_tax,
        .stock = .stock,
        .transit_avo = .transit_avo
      )

      autonomous_adjust <- vmt_autonomous_vehicle(
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .av_pct = .av_pct
      )


      vmt_forecast <- left_join(tb_vmt, ann_energy_outlook, by = "year") %>%
        left_join(trans_rider, by = c("ctu", "year")) %>%
        left_join(fc_adjustments, by = c("ctu", "year")) %>%
        left_join(land_use, by = c("year")) %>%
        left_join(parking, by = c("year", "ctu")) %>%
        left_join(veh_occupancy, by = c("year", "ctu")) %>%
        left_join(autonomous_adjust, by = c("year", "ctu")) %>%
        mutate(transit_vmt = (miles_traveled *
          aeo_adj * transit_adj *
          (1 + (vmt_fee_adjust + payd_ins_adj + cong_adjust) * cross_vmt) *
          land_use_adj * park_price_adj * gas_adj) /
          occupancy_adj * av_adj) %>%
        select(scenario, ctu, year, mode, aeo_mode, vmt = transit_vmt)

      return(vmt_forecast)
    } else if (.mode == "PLDV") {
      # passenger light duty --------

      pass_tb_vmt <- vmt_dynamic_ride_share_reduction(
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .variable = .variable,
        .drs_pct = .drs_pct
      )

      # formula is such
      # pldv_vmt <- miles_traveled - transit shift * AV adjustment *
      # aeo adjustment *
      # (1 + (vmt_fee_adjust +  payd_ins_adj) * ELAST_VMT) *
      # (1 + cong_adj) *
      # (1 + gas_adj) *
      # park_price_adj *
      # land_use_adj *
      # telework_adj /
      # occupancy_adj
      ann_energy_outlook <- vmt_annual_energy_outlook(
        tb = transportation_data$passenger,
        .aeo_scenario = .aeo_scenario,
        .mode = .mode
      )

      trans_rider <- vmt_transit_ridership(
        .tb_vmt = pass_tb_vmt,
        .mode = .mode,
        .transit_rider_pct = .transit_rider_pct
      )

      fc_adjustments <- vmt_road_policy(
        .mode = .mode,
        .tb_vmt = pass_tb_vmt,
        .tb_fuel_cost_mile = .tb_fuel_cost_mile,
        .vmt_fee = .vmt_fee,
        .cong_price = .cong_price,
        .gas_tax = .gas_tax,
        .payd_fee = .payd_fee,
        .stock = .stock
      )

      land_use <- vmt_land_use_change(
        .mode = .mode,
        .type = "DRIVE",
        .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
        .pop_dens_pct_change = .pop_dens_pct_change,
        .emp_dens_pct_change = .emp_dens_pct_change,
        .land_use_pct_change = .land_use_pct_change,
        .intersection_design_pct_change = .intersection_design_pct_change,
        .job_access_pct_change = .job_access_pct_change,
        .transit_dist_pct_change = .transit_dist_pct_change
      )

      parking <- vmt_parking_policy(
        .mode = .mode,
        .parking_price = .parking_price
      )

      veh_occupancy <- vmt_vehicle_occupancy(
        tb = transportation_data$passenger,
        .tb_vmt = pass_tb_vmt,
        .mode = .mode,
        .gas_tax = .gas_tax,
        .stock = .stock,
        .transit_avo = .transit_avo
      )

      autonomous_adjust <- vmt_autonomous_vehicle(
        .tb_vmt = pass_tb_vmt,
        .mode = .mode,
        .av_pct = .av_pct
      )

      telework_adjust <- vmt_telework(
        .mode = .mode,
        .telework_pct = .telework_pct
      )


      vmt_forecast <- left_join(pass_tb_vmt, ann_energy_outlook, by = "year") %>%
        left_join(trans_rider, by = c("ctu", "year")) %>%
        left_join(fc_adjustments, by = c("ctu", "year")) %>%
        left_join(land_use, by = c("year")) %>%
        left_join(parking, by = c("year", "ctu")) %>%
        left_join(veh_occupancy, by = c("year", "ctu")) %>%
        left_join(autonomous_adjust, by = c("year")) %>%
        left_join(telework_adjust, by = c("year")) %>%
        left_join(tb %>%
          filter(mode == "AT", var == .variable) %>%
          select(ctu, year, active_transportation_adj = value),
        by = c("year", "ctu")
        ) %>%
        mutate(pass_ld_vmt = miles_traveled -
          (active_transportation_adj * transit_adj) *
            av_adj * aeo_adj *
            vmt_fee_adjust * cong_adjust * gas_adj *
            telework_adj * land_use_adj *
            park_price_adj / occupancy_adj) %>%
        select(scenario, ctu, year, mode, aeo_mode, vmt = pass_ld_vmt)


      return(vmt_forecast)
    } else if (.mode == "AV") {
      # autonomous vehicle -----
      # av_vmt = miles_traveled  -
      # (active transportation adjustment * transit_adj) *
      # aeo_adj * vmt_fee_adjust * cong_adjust *
      # gas_adj * park_price_adj * land_use_adj *
      # av_adjust

      ann_energy_outlook <- vmt_annual_energy_outlook(
        tb = transportation_data$passenger,
        .aeo_scenario = .aeo_scenario,
        .mode = .mode
      )

      fc_adjustments <- vmt_road_policy(
        .mode = .mode,
        .tb_vmt = tb_vmt,
        .tb_fuel_cost_mile = .tb_fuel_cost_mile,
        .vmt_fee = .vmt_fee,
        .cong_price = .cong_price,
        .gas_tax = .gas_tax,
        .payd_fee = .payd_fee,
        .stock = .stock
      )

      land_use <- vmt_land_use_change(
        .mode = .mode,
        .type = "DRIVE",
        .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
        .pop_dens_pct_change = .pop_dens_pct_change,
        .emp_dens_pct_change = .emp_dens_pct_change,
        .land_use_pct_change = .land_use_pct_change,
        .intersection_design_pct_change = .intersection_design_pct_change,
        .job_access_pct_change = .job_access_pct_change,
        .transit_dist_pct_change = .transit_dist_pct_change
      )

      parking <- vmt_parking_policy(
        .mode = .mode,
        .parking_price = .parking_price
      )

      veh_occupancy <- vmt_vehicle_occupancy(
        tb = transportation_data$passenger,
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .gas_tax = .gas_tax,
        .stock = .stock,
        .transit_avo = .transit_avo
      )

      at_adjustment <- tb %>%
        filter(mode == "AT", var == .variable) %>%
        select(year, ctu, at_adjust = value)

      trans_rider <- vmt_transit_ridership(
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .transit_rider_pct = .transit_rider_pct
      )

      vmt <- tb_vmt %>%
        left_join(at_adjustment, by = c("year", "ctu")) %>%
        left_join(trans_rider, by = c("year", "ctu")) %>%
        left_join(fc_adjustments, by = c("ctu", "year")) %>%
        left_join(land_use, by = c("year")) %>%
        left_join(parking, by = c("year", "ctu")) %>%
        left_join(veh_occupancy, by = c("year", "ctu")) %>%
        mutate(av_vmt = miles_traveled - (at_adjust * transit_adj) *
          av_adj * aeo_adj * vmt_fee_adj *
          cong_adjust * gas_adj * park_price_adj *
          land_use_adj * .enviro_factors$VMT_AV / occupancy_adj) %>%
        select(scenario, ctu, year, mode, aeo_mode, vmt = av_vmt)

      return(vmt)



      # .vmt_fee <- (tb_var$pldv
      #              # Subtract the PMT reduction from a shift to transit (assuming equal per trip PMT)
      #              -
      #                tb_var$at *
      #                (.transit_rider_pct / 100 * .enviro_factors$PLDV_TRANSIT_RATIO)) *
      #   # Remove non-AV from AV PMT
      #   tb_avshare * .av_pct / 100 *
      #   # Apply AEO adjustments
      #   aeo_vmt$ldv *
      #   # Apply elasticities, etc.
      #   (1 + (.vmt_fee / (.tb_fuel_cost_mile + .enviro_factors$TIME_COST_MI) +
      #           .payd_fee / .enviro_factors$INS_COST_MI) * ELAST_VMT) *
      #   # Congestion elasticity only applies to a portion of the VMT set by .enviro_factors$CONG_VMT, so scale the elasticity effect down
      #   (1 + (.cong_price / (.tb_fuel_cost_mile + .enviro_factors$TIME_COST_MI) * .enviro_factors$CONG_VMT) * ELAST_CONG) *
      #   (1 + (.gas_tax / .tb_fuel_cost_mile) *
      #      ifelse(((.stock == "SIStock") |
      #                (.stock == "CIStock") |
      #                (.stock == "HEVStock") |
      #                ((.stock == "PHEVStock") &
      #                   (ch_phev == 1))), 1, 0) * ELAST_GAS) *
      #   (1 + (.parking_price / tb_park * ELAST_PARK)) *
      #   # Auto 5D: population density, employment density, diversity, .intersection_design_pct_change, distance
      #   # If the combined elasticity effect is greater than the max of 25% reduction in VMT (i.e., more negative) then use the max. Else, use the user provided elasticities.
      #   if (.comb_5d_impact_pct_change < .enviro_factors$MAX_5D_DR) {
      #     (1 + .enviro_factors$MAX_5D_DR)
      #   } else {{
      #     (1 + .pop_dens_pct_change / 100 * ELAST_DENS_DR_POP) *
      #       (1 + .emp_dens_pct_change / 100 * ELAST_DENS_DR_EMP) *
      #       (1 + .land_use_pct_change / 100 * ELAST_DIVER_DR) *
      #       (1 + .intersection_design_pct_change / 100 * ELAST_DES_DR) *
      #       (1 + .job_access_pct_change / 100 * ELAST_JOBS_DR) *
      #       (1 + .transit_dist_pct_change / 100 * ELAST_DIST_DR) *
      #       (1 + c.pop_dens_pct_change / 100 * ELAST_CDENS_DR)
      #   } *
      #       # AV increases the VMT slightly, by about 15-20% for local trips (<50 miles)
      #       .enviro_factors$VMT_AV / pldv_stocks$avo
      #     }
    } else if (.mode == "SUT") {
      # single truck --------

      ann_energy_outlook <- vmt_annual_energy_outlook(
        tb = transportation_data$freight,
        .aeo_scenario = .aeo_scenario,
        .mode = .mode
      )

      veh_occupancy <- vmt_vehicle_occupancy(
        tb = transportation_data$freight,
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .gas_tax = .gas_tax,
        .stock = .stock,
        .transit_avo = .transit_avo
      )


      parking <- vmt_parking_policy(
        .mode = .mode,
        .parking_price = .parking_price
      )

      fc_adjustments <- vmt_road_policy(
        .mode = .mode,
        .tb_vmt = tb_vmt,
        .tb_fuel_cost_mile = .tb_fuel_cost_mile,
        .vmt_fee = .vmt_fee,
        .cong_price = .cong_price,
        .gas_tax = .gas_tax,
        .payd_fee = .payd_fee,
        .stock = .stock,
        .freight_vmt_fee = .freight_vmt_fee
      )



      # miles_traveled * aeo_adj * vmt_fee_adj * parking_adj / occupancy_adj



      vmt <- left_join(tb_vmt, ann_energy_outlook, by = c("year")) %>%
        left_join(parking, by = "year") %>%
        left_join(veh_occupancy, by = "year") %>%
        left_join(fc_adjustments, by = "year") %>%
        mutate(sut_vmt = miles_traveled * aeo_adj * vmt_fee_adj * park_price_adj / occupancy_adj) %>%
        select(scenario, ctu, year, mode, aeo_mode, vmt = sut_vmt)

      return(vmt)
      #
      #       .vmt_fee <- tb_mode$var *
      #         # Apply AEO adjustments
      #         aeo_vmt$mdt *
      #         # Apply elasticities, etc.
      #         # Assumes no shift to other modes because there are other restrictions on that (you probably won't build a new rail line in a city based on a congestion price)
      #         (1 + .freight_vmt_fee / (.tb_fuel_cost_mile + F_.enviro_factors$TIME_COST_MI) * ELAST_FVMT * .enviro_factors$F_FRACT) * # Only apply the VMT fee to fraction occuring in MSP (equivalent to a reduction in elasticity)
      #         (1 + .parking_price / tb_park * ELAST_PARK) /
      #         tb_mode$avo *
      #         (tb_mode$stock /
      #            tb_mode$tot_stock)
    } else if (.mode == "CUT") {
      # combined truck ------

      # miles_traveled * aeo_adj * vmt_fee_adj / occpancy_adj

      ann_energy_outlook <- vmt_annual_energy_outlook(
        tb = transportation_data$freight,
        .aeo_scenario = .aeo_scenario,
        .mode = .mode
      )


      veh_occupancy <- vmt_vehicle_occupancy(
        tb = transportation_data$freight,
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .gas_tax = .gas_tax,
        .stock = .stock,
        .transit_avo = .transit_avo
      )

      fc_adjustments <- vmt_road_policy(
        .mode = .mode,
        .tb_vmt = tb_vmt,
        .tb_fuel_cost_mile = .tb_fuel_cost_mile,
        .vmt_fee = .vmt_fee,
        .cong_price = .cong_price,
        .gas_tax = .gas_tax,
        .payd_fee = .payd_fee,
        .stock = .stock,
        .freight_vmt_fee = .freight_vmt_fee
      )


      vmt <- left_join(tb_vmt, ann_energy_outlook, by = c("year")) %>%
        # left_join(parking, by = c("year", "ctu")) %>%
        left_join(veh_occupancy, by = c("year", "ctu")) %>%
        left_join(fc_adjustments, by = c("year")) %>%
        mutate(cut_vmt = miles_traveled * aeo_adj * vmt_fee_adj / occupancy_adj) %>%
        select(scenario, ctu, year, mode, aeo_mode, vmt = cut_vmt)

      return(vmt)


      # .vmt_fee <- tb_mode$var *
      #   # Apply AEO adjustments
      #   aeo_vmt$hdt *
      #   # Apply elasticities, etc.
      #   # Assumes no shift to other modes because there are other restrictions on that (you probably won't build a new rail line in a city based on a congestion price)
      #   (1 + .freight_vmt_fee / (.tb_fuel_cost_mile + F_.enviro_factors$TIME_COST_MI) * ELAST_FVMT) /
      #   tb_mode$avo *
      #   (tb_mode$stock /
      #      tb_mode$tot_stock)
    } else if (.mode == "WALK" | .mode == "BIKE") {
      # walk bike -----
      # browser()

      # miles_traveled * land_use_adj * aeo_adj


      land_use <- vmt_land_use_change(
        .mode = .mode,
        .type = "WALK",
        .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
        .pop_dens_pct_change = .pop_dens_pct_change,
        .emp_dens_pct_change = .emp_dens_pct_change,
        .land_use_pct_change = .land_use_pct_change,
        .intersection_design_pct_change = .intersection_design_pct_change,
        .job_access_pct_change = .job_access_pct_change,
        .transit_dist_pct_change = .transit_dist_pct_change
      )



      vmt <- left_join(tb_vmt, land_use, by = c("year")) %>%
        mutate(walk_vmt = miles_traveled * land_use_adj) %>%
        select(scenario, ctu, year, mode, aeo_mode, vmt = walk_vmt)

      return(vmt)
    } else if (.mode == "BS") {
      # school bus-----

      # miles_traveled * aeo_adj/occupancy_adj

      ann_energy_outlook <- vmt_annual_energy_outlook(
        tb = transportation_data$passenger,
        .aeo_scenario = .aeo_scenario,
        .mode = .mode
      )


      veh_occupancy <- vmt_vehicle_occupancy(
        tb = transportation_data$passenger,
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .gas_tax = .gas_tax,
        .stock = .stock,
        .transit_avo = .transit_avo
      )

      vmt <- left_join(tb_vmt, veh_occupancy, c("year", "ctu")) %>%
        left_join(ann_energy_outlook, by = "year") %>%
        mutate(school_bus_vmt = miles_traveled * aeo_adj / occupancy_adj) %>%
        select(scenario, ctu, year, mode, aeo_mode, vmt = school_bus_vmt)

      return(vmt)
    } else if (.mode == "FR") {
      # freight rail ------
      # browser()
      # miles_traveled * aeo_adj / occupancy_adj

      ann_energy_outlook <- vmt_annual_energy_outlook(
        tb = transportation_data$freight,
        .aeo_scenario = .aeo_scenario,
        .mode = .mode
      )

      veh_occupancy <- vmt_vehicle_occupancy(
        tb = transportation_data$freight,
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .gas_tax = .gas_tax,
        .stock = .stock,
        .transit_avo = .transit_avo
      )



      vmt <- left_join(tb_vmt, ann_energy_outlook, by = c("year")) %>%
        left_join(veh_occupancy, by = c("year", "ctu")) %>%
        mutate(fr_vmt = (miles_traveled * aeo_adj) / occupancy_adj) %>%
        select(scenario, ctu, year, mode, aeo_mode, vmt = fr_vmt)

      return(vmt)
    } else if (.mode == "MM" |
      .mode == "AIR" |
      .mode == "WAT") {
      # freight multimodal, air, water-----

      # browser()
      ann_energy_outlook <- vmt_annual_energy_outlook(
        tb = tb,
        .aeo_scenario = .aeo_scenario,
        .mode = .mode
      )


      veh_occupancy <- vmt_vehicle_occupancy(
        tb = tb,
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .gas_tax = .gas_tax,
        .stock = .stock,
        .transit_avo = .transit_avo
      )

      tb_fin <- tb %>%
        dplyr::filter(
          mode == .mode,
          var %in% c(
            .variable,
            .stock,
            "TotStock"
          )
        ) %>%
        unique() %>%
        group_by(mode, ctu, year, aeo_mode, type) %>%
        tidyr::pivot_wider(
          names_from = var,
          values_from = value
        ) %>%
        select(
          type, ctu, year,
          mode,
          aeo_mode
        ) %>%
        ungroup() %>%
        left_join(tb_vmt, by = c("type", "ctu", "year", "mode", "aeo_mode")) %>%
        left_join(veh_occupancy, by = c("ctu", "year")) %>%
        left_join(ann_energy_outlook, by = c("year")) %>%
        rowwise() %>%
        mutate(
          scenario = .scenario,
          stock = .stock,
          vmt = miles_traveled * aeo_adj / occupancy_adj,
          vmt = case_when(
            vmt == Inf | is.na(vmt) | vmt < 0 ~ 0,
            TRUE ~ vmt
          )
        ) %>%
        select(type, stock, scenario, ctu, year, mode, aeo_mode, vmt) %>%
        unique()

      return(tb_fin) # in thousands of miles
    }
  } else if (.mode == "WALK" | .mode == "BIKE") {
    # BAU and walk/bike -----
    # browser()

    avo <- tb %>%
      dplyr::filter(
        mode == .mode,
        var %in% c(
          "AVO"
        )
      ) %>%
      select(-ctu) %>%
      tidyr::pivot_wider(
        names_from = var,
        values_from = value
      )

    tb_fin <- tb %>%
      dplyr::filter(
        mode == .mode,
        var %in% c(
          .variable,
          .stock,
          "TotStock"
        )
      ) %>%
      unique() %>%
      group_by(mode, ctu, year, aeo_mode, type) %>%
      pivot_wider(
        names_from = var,
        values_from = value
      ) %>%
      left_join(avo, by = c("mode", "year", "aeo_mode", "type")) %>%
      rowwise() %>%
      mutate(
        vmt = PMT / AVO,
        scenario = .scenario,
        stock = .stock
      ) %>%
      select(type, scenario, ctu, year, mode, aeo_mode, vmt) %>%
      unique()


    return(tb_fin)
  } else {
    # all other BAU------

    ann_energy_outlook <- vmt_annual_energy_outlook(
      tb = tb,
      .aeo_scenario = .aeo_scenario,
      .mode = .mode
    )


    veh_occupancy <- vmt_vehicle_occupancy(
      tb = tb,
      .tb_vmt = tb_vmt,
      .mode = .mode,
      .gas_tax = .gas_tax,
      .stock = .stock,
      .transit_avo = .transit_avo
    )

    tb_fin <- tb %>%
      dplyr::filter(
        mode == .mode,
        var %in% c(
          .variable,
          .stock,
          "TotStock",
          "AVO"
        )
      ) %>%
      unique() %>%
      group_by(mode, ctu, year, aeo_mode, type) %>%
      tidyr::pivot_wider(
        names_from = var,
        values_from = value
      ) %>%
      select(type, ctu, year,
        mode,
        aeo_mode,
        mode_stock = !!rlang::sym(.stock),
        mode_avo = AVO,
        totstock = TotStock,
        mode_var = !!rlang::sym(.variable)
      ) %>%
      ungroup() %>%
      left_join(tb_vmt, by = c("type", "ctu", "year", "mode", "aeo_mode")) %>%
      left_join(veh_occupancy, by = c("ctu", "year")) %>%
      left_join(ann_energy_outlook, by = c("year")) %>%
      rowwise() %>%
      mutate(
        scenario = .scenario,
        stock = .stock,
        mode_stock_proportion = mode_stock / totstock,
        vmt = miles_traveled * aeo_adj * 1 / (mode_avo * mode_stock_proportion),
        vmt = case_when(
          vmt == Inf | is.na(vmt) ~ 0,
          TRUE ~ vmt
        )
      ) %>%
      select(type, stock, scenario, ctu, year, mode, aeo_mode, vmt) %>%
      unique()


    return(tb_fin) # in thousands of miles
  }
}
