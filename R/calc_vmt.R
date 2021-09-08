#' @title Calculate vehicle miles traveled by mode and power train
#'
#' @param .scenario character, scenario name
#' @param tb input table. Should have columns `mode`, `var`, `ctu`,
#'    and one for each year
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
#'
calc_vmt <- function(.scenario,
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
                     ch_phev = 0) {
  # browser()
  # Annual energy outlook VMT tables -------
  # Specific to each mode type
  aeo_vmt <- list(
    rail = factor_values$aeo %>%
      dplyr::filter(
        AEOScen == .aeo_scenario,
        Metric == "VMT",
        Mode == "RAIL"
      ),
    bus = factor_values$aeo %>%
      dplyr::filter(AEOScen == .aeo_scenario, Metric == "VMT", Mode == "BUS"),
    ldv = factor_values$aeo %>%
      dplyr::filter(AEOScen == .aeo_scenario, Metric == "VMT", Mode == "LDV"),
    mdt = factor_values$aeo %>%
      dplyr::filter(AEOScen == .aeo_scenario, Metric == "VMT", Mode == "MDT"),
    hdt = factor_values$aeo %>%
      dplyr::filter(AEOScen == .aeo_scenario, Metric == "VMT", Mode == "HDT"),
    frail = factor_values$aeo %>%
      dplyr::filter(AEOScen == .aeo_scenario, Metric == "VMT", Mode == "FRAIL"),
    fship = factor_values$aeo %>%
      dplyr::filter(AEOScen == .aeo_scenario, Metric == "VMT", Mode == "FSHIP")
  )


  # plvd stocks -----
  # passenger light duty vehicle
  # pldv_stocks <- list(
  #   si = tb %>%
  #     dplyr::filter(mode == "PLDV", var == "SIStock"),
  #   ci = tb %>%
  #     dplyr::filter(mode == "PLDV", var == "CIStock"),
  #   hev = tb %>%
  #     dplyr::filter(mode == "PLDV", var == "HEVStock"),
  #   .parking_price = tb %>%
  #     dplyr::filter(mode == "PLDV", var == "PARK"),
  #   tot = tb %>%
  #     dplyr::filter(mode == "PLDV", var == "TotStock"),
  #   avo = tb %>%
  #     dplyr::filter(mode == "PLDV", var == "AVO")
  # )

  # .mode held constant table subsets ------
  tb_mode <- list(
    var = tb %>%
      dplyr::filter(mode == .mode, var == .variable),
    stock = tb %>%
      dplyr::filter(mode == .mode, var == .stock),
    tot_stock = tb %>%
      dplyr::filter(mode == .mode, var == "TotStock"),
    avo = tb %>% # average vehicle occupancy
      dplyr::filter(mode == .mode, var == "AVO")
  )

  # variable held constant -----
  tb_var <- list(
    at = tb %>%
      dplyr::filter(mode == "AT", var == .variable),
    pldv = tb %>%
      dplyr::filter(mode == "PLDV", var == .variable)
  )


  tb_avshare <- tb %>%
    dplyr::filter(var == "AVShare")

  tb_park <- tb %>%
    dplyr::filter(var == "PARK")

  # basic filter for mode and variable



  # calculation -----
  # If it's not the BAU scenario, then need to run elasticities, etc.
  if (.scenario != "BAU") {
    browser()
    # If it's a transit mode, then apply the ridership and avo factors (including cross elasticity from PLDV fees)
    if ((.mode == "BU") |
        (.mode == "BRT") |
        (.mode == "RU") |
        (.mode == "RI")) {


      # formula is such
      # transit vmt = PMT * AEO elasticity * transit ridership effect *
      # (1 + (vmt_fee_adjust +  payd_ins_adj + cong_adjust * cross_vmt)) *
      # land_use_adj * park_price_adj * gas_adj /
      # av_adj / av_transit_adj
      #
      tb_vmt <- tb %>%
        filter(mode == .mode,
               var  ==  .variable) %>%
        mutate(vmt = value)

      ann_energy_outlook <- calc_annual_energy_outlook(tb = transportation_data$passenger,
                                                       .aeo_scenario = .aeo_scenario,
                                                       .mode = .mode)

      trans_rider <- calc_transit_ridership(.mode = .mode,
                                            .transit_rider_pct = .transit_rider_pct)

      fc_adjustments <- calc_fuel_congestion(.mode = .mode,
                                             .tb_vmt = tb_vmt,
                                             .tb_fuel_cost_mile = .tb_fuel_cost_mile,
                                             .vmt_fee = .vmt_fee,
                                             .cong_price = .cong_price,
                                             .gas_tax = .gas_tax,
                                             .payd_fee = .payd_fee,
                                             .is_av = 0,
                                             .stock = .stock)

      land_use <- calc_land_use_change(.mode = .mode,
                                      .type = "P",
                                      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
                                      .pop_dens_pct_change = .pop_dens_pct_change,
                                      .emp_dens_pct_change = .emp_dens_pct_change,
                                      .land_use_pct_change = .land_use_pct_change,
                                      .intersection_design_pct_change = .intersection_design_pct_change,
                                      .job_access_pct_change = .job_access_pct_change,
                                      .transit_dist_pct_change = .transit_dist_pct_change)

      parking <-  calc_parking_policy(.mode = mode, .parking_price = .parking_price)

      tb_fin <- tb %>%
        dplyr::filter(
          mode == .mode,
          var %in% c(
            .variable,
            .stock,
            "TotStock",
            "AVO",
            "SIStock",
            "CIStock",
            "BCIStock",
            "HEVStock",
            "PARK"
          )
        ) %>%
        unique() %>%
        pivot_wider(
          names_from = var,
          values_from = value,
        ) %>%
        left_join(

          tb %>%
            filter(mode == "PLDV",
                   var %in% c("PARK",
                              "SIStock")) %>%
            unique() %>%
            pivot_wider(
              names_from = var,
              values_from = value
            ) %>%
            select(-ctu,
                   -mode)
        )



      aeo_vals <- factor_values$aeo %>%
        dplyr::filter(AEOScen == .aeo_scenario,
                      Metric == "VMT",
                      Mode == unique(tb_fin$aeo_mode)) %>%
        select(everything(),
               aeo_value = value)


      # adjustments ----
      transit_rider_adjust <- 1 + .transit_rider_pct/100

      payd_ins_adjust <- .payd_fee / INS_COST_MI



      comb_5d_elast <- elast_5d %>%
        filter(type == "TRANSIT") %>%
        mutate(population_density = (1 + .pop_dens_pct_change / 100) * population_density,
               employment_density = (1 + .emp_dens_pct_change / 100) * employment_density,
               diversity = (1 + .land_use_pct_change / 100) * diversity,
               design = (1 + .intersection_design_pct_change / 100) * design,
               job_access = (1 + .job_access_pct_change / 100) * job_access,
               distance = (1 + .transit_dist_pct_change / 100) * distance,
               combined_density = (1 + .pop_dens_pct_change / 100) * combined_density,
               product_all = population_density * employment_density * diversity *
                 design * job_access * distance * combined_density,
               fin_value = ifelse(.comb_5d_impact_pct_change < MAX_5D_TRANS,
                                  1 + MAX_5D_TRANS,
                                  product_all))





      fcm_adjust <- .tb_fuel_cost_mile %>%
        mutate(fuel_cost_mile_plus = fuel_cost_mile + TIME_COST_MI,
               vmt_fee_adjust = .vmt_fee/fuel_cost_mile_plus,
               cong_adjust = (.cong_price/fuel_cost_mile_plus) * CONG_VMT,
               cross_vmt = elast_cross$vmt,
               adj_5d = comb_5d_elast$fin_value,
               gas_adj = .gas_tax/fuel_cost_mile)


      left_join(tb_fin, aeo_vals) %>%
        left_join(fcm_adjust %>%
                    select(-ctu,
                           -mode)) %>%
        mutate(
          park_price_adj = 1 + (.parking_price / PARK * crosses$park_transit),

          vmt =  !!rlang::sym(.variable) * aeo_value * transit_rider_adjust *
            (1 + (vmt_fee_adjust + payd_ins_adjust + cong_adjust)) * cross_vmt *
            adj_5d * park_price_adj  * gas_adj



        ) %>% View


      .vmt_fee <- tb_mode$var *
        # Apply AEO adjustments
        dplyr::case_when(
          ((.mode == "BU") | (.mode == "BRT")) ~ aeo_vmt$bus,
          ((.mode == "RU") | (.mode == "RI")) ~ aeo_vmt$rail,
          TRUE ~ 1
        ) *
        # Elasticities, etc.
        (1 + .transit_rider_pct / 100) *
        (1 + (.vmt_fee / (.tb_fuel_cost_mile + TIME_COST_MI) +
                (.payd_fee / INS_COST_MI) +
                .cong_price /
                (.tb_fuel_cost_mile + TIME_COST_MI) *
                CONG_VMT) * CROSS_VMT) *
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
            (1 + .pop_dens_pct_change / 100 * ELAST_CDENS_TRANS)
        } *
            # Parking pricing effect
            (1 + .parking_price / pldv_stocks$.parking_price * CROSS_PARK_TRANSIT) *
            # Gas price effect (relative to fuel cost)
            (1 + (.gas_tax / .tb_fuel_cost_mile) * # Only applied to SI/CI/HEV stock (assume PHEV not very sensitive and partially accounted for by a full inclusion of HEV, which is also not as sensitive to .gas_tax price because already switched stock from SI/CI)

               (pldv_stocks$si + pldv_stocks$ci + pldv_stocks$hev) /
               pldv_stocks$tot * CROSS_VMT) /
            (tb_mode$avo * (1 + .transit_avo / 100)) *
            (tb_mode$stock / tb_mode$tot_stock) *
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
    } else if (.mode == "PLDV") { # passenger light duty -----
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
        (1 + (.vmt_fee / (.tb_fuel_cost_mile + TIME_COST_MI) +
                .payd_fee / INS_COST_MI) * ELAST_VMT) *
        # Congestion elasticity only applies to a portion of the VMT set by CONG_VMT, so scale the elasticity effect down
        (1 + (.cong_price / (.tb_fuel_cost_mile + TIME_COST_MI) * CONG_VMT) * ELAST_CONG) *
        # Gas price effect (relative to fuel cost)
        (1 + (.gas_tax / .tb_fuel_cost_mile) * ifelse(((.stock == "SIStock") |
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
        (1 + (.vmt_fee / (.tb_fuel_cost_mile + TIME_COST_MI) + .payd_fee / INS_COST_MI) * ELAST_VMT) *
        # Congestion elasticity only applies to a portion of the VMT set by CONG_VMT, so scale the elasticity effect down
        (1 + (.cong_price / (.tb_fuel_cost_mile + TIME_COST_MI) * CONG_VMT) * ELAST_CONG) *
        (1 + (.gas_tax / .tb_fuel_cost_mile) * ifelse(((.stock == "SIStock") |
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
        (1 + .freight_vmt_fee / (.tb_fuel_cost_mile + F_TIME_COST_MI) * ELAST_FVMT * F_FRACT) * # Only apply the VMT fee to fraction occuring in MSP (equivalent to a reduction in elasticity)
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
        (1 + .freight_vmt_fee / (.tb_fuel_cost_mile + F_TIME_COST_MI) * ELAST_FVMT) /
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
    browser()

    avo <- tb %>%
      dplyr::filter(
        mode == .mode,
        var %in% c(
          "AVO"
        )
      ) %>%
      select(-ctu) %>%
      pivot_wider(
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
      mutate(vmt = PMT / AVO,
             scenario = .scenario,
             stock = .stock) %>%
      select(
        scenario,
        mode,
        stock,
        ctu, year, aeo_mode, type, vmt
      )

    return(tb_fin)

  } else { # if scenario is BAU
    # browser()
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
      pivot_wider(
        names_from = var,
        values_from = value
      ) %>%
      mutate(
        scenario = .scenario,
        stock = .stock,
        vmt := !!rlang::sym(.variable) * 1 / AVO * (!!rlang::sym(.stock) / TotStock)
      ) %>%
      select(
        scenario, mode,
        stock,
        ctu, year, aeo_mode, type, vmt
      )


    return(tb_fin) # in thousands of miles
  }
}
