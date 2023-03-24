#' @title Calculate vehicle miles traveled by mode and power train
#'
#' @param .scenario character, scenario name. JUST A LABEL
#' @param tb input table for appropriate mode type. Should have columns `mode`, `var`, `ctu`,
#'    and one for each year. Package provided datasets `transportation_data$passenger` or
#'    `transportation_data$freight` are suitable.
#' @param .mode character, current mode
#' @param .stock character, stock for current mode
#' @param .variable character, variable name - e.g., "VMT"
#' @param .tb_fuel_cost_mile table, table with fuel cost per mile
#' @param .aeo_scenario character, selected EIA Annual Energy Outlook scenario.
#'      Default is `"REF"`
#' @param .phev_electric logical, is the current PHEV distinction electric. Default is `FALSE`.
#' @param .enviro_factors list, environmental factors. Default is `enviro_factors`, included in this package.
#' @param .elast table of elasticities. Default is `elast` included in this package.
#' @param .elast_5d table of 5D elasticities. Default is `elast_5d` included in this package.
#'
#' @inheritParams run_scenario_transportation
#' @inheritParams vmt_parking_policy
#' @inheritParams vmt_land_use_change
#' @inheritParams vmt_road_policy
#' @inheritParams vmt_transit_service
#' @inheritParams vmt_vehicle_occupancy
#' @inheritParams vmt_telework
#'
### Eqn: (PMT in 1000 mi) x Pr(stock by fuel) / AVO
#'
#' @return a tibble with columns `scenario`, `ctu`, `year`, `aeo_mode`, `type`, `vmt`,
#'     with `vmt` in thousands
#' @export
#' @family transportation
#'
#'
#' @importFrom dplyr filter select case_when
#' @importFrom tidyselect all_of
#'
calc_vmt_forecast <- function(.scenario,
                              tb,
                              .selected_ctu = "all",
                              .mode,
                              .stock,
                              .variable,
                              .tb_fuel_cost_mile,
                              .aeo_scenario = "REF",
                              .transit_avo_pct = 0,
                              .transit_service_pct = 0,
                              .pldv_avo_pct = 0,
                              .vmt_fee = 0,
                              .payd_fee = 0,
                              .gas_tax = 0,
                              .cong_price = 0,
                              .parking_price = 0,
                              .freight_parking_price = 0,
                              .freight_vmt_fee = 0,
                              .pop_dens_pct_change = 0,
                              .emp_dens_pct_change = 0,
                              .land_use_diversity_pct_change = 0,
                              .intersection_design_pct_change = 0,
                              .job_access_pct_change = 0,
                              .transit_dist_pct_change = 0,
                              .comb_5d_impact_pct_change = 0,
                              .telework_pct = 0,
                              .phev_electric = FALSE,
                              .enviro_factors = enviro_factors,
                              .elast = elast,
                              .elast_5d = elast_5d) {
  cli::cli_progress_message("*** calculating VMT forecast \n")
  tb <- filter_ctu(tb, .selected_ctu)
  # browser()
  check_inputs("mode", .mode)

  tb_vmt <- tb %>%
    filter(
      mode == .mode,
      var == .variable
    ) %>%
    mutate(
      miles_traveled = value,
      scenario = .scenario
    ) %>%
    select(scenario, mode, ctu, year, aeo_mode, type, miles_traveled) %>%
    unique()

  # If it's not the BAU scenario, then need to run elasticities, etc.
  if (!.mode %in% c("WALK", "BIKE")) {
    # Not BAU ----
    # browser()

    if (.mode %in% c(
      "BU",
      "BRT",
      "RU",
      "RI"
    )) {
      # transit bus and rail -----
      # If it's a transit mode, then apply transit service and average vehicle occupancy factors (including cross elasticity from PLDV fees)

      # browser()

      # formula is such
      # transit vmt = PMT * aeo_adj * transit_adj *
      # (1 + (vmt_fee_adj +  payd_ins_adj + cong_adjust * cross_vmt)) *
      # land_use_adj * park_price_adj * gas_adj /
      # occupancy_adj /  mode_stock_adj
      #

      mode_stock <- vmt_stock_proportion(
        .tb = tb,
        .mode = .mode,
        .stock = .stock
      )

      ann_energy_outlook <- vmt_annual_energy_outlook(
        tb = tb,
        .aeo_scenario = .aeo_scenario,
        .mode = .mode
      )

      # transit service adjustment
      # distributes final % increase across years
      trans_service <- vmt_transit_service(
        tb = tb,
        .mode = .mode,
        .transit_service_pct = .transit_service_pct,
        .elast = .elast,
        .enviro_factors = .enviro_factors
      )

      fc_adjustments <- vmt_road_policy(
        .pass_tb = tb,
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .tb_fuel_cost_mile = .tb_fuel_cost_mile,
        .vmt_fee = .vmt_fee,
        .cong_price = .cong_price,
        .gas_tax = .gas_tax,
        .payd_fee = .payd_fee,
        .stock = .stock,
        .phev_electric = .phev_electric,
        .freight_vmt_fee = .freight_vmt_fee,
        .enviro_factors = .enviro_factors,
        .elast = .elast
      )

      land_use <- vmt_land_use_change(
        .type = "TRANSIT",
        .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
        .pop_dens_pct_change = .pop_dens_pct_change,
        .emp_dens_pct_change = .emp_dens_pct_change,
        .land_use_diversity_pct_change = .land_use_diversity_pct_change,
        .intersection_design_pct_change = .intersection_design_pct_change,
        .job_access_pct_change = .job_access_pct_change,
        .transit_dist_pct_change = .transit_dist_pct_change,
        .elast_5d = .elast_5d
      )

      parking <- vmt_parking_policy(
        tb = tb,
        .mode = .mode,
        .freight_parking_price = .freight_parking_price,
        .parking_price = .parking_price,
        .elast = .elast
      )

      veh_occupancy <- vmt_vehicle_occupancy(
        tb = tb,
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .stock = .stock,
        .transit_avo_pct = .transit_avo_pct,
        .pldv_avo_pct = .pldv_avo_pct,
        .enviro_factors = .enviro_factors
      )



      tb_fin <- left_join(tb_vmt, ann_energy_outlook, by = "year") %>%
        left_join(trans_service, by = c("ctu", "year")) %>%
        left_join(fc_adjustments, by = c("ctu", "year")) %>%
        left_join(land_use, by = c("year")) %>%
        left_join(parking, by = c("year", "ctu")) %>%
        left_join(veh_occupancy, by = c("year", "ctu")) %>%
        left_join(mode_stock, by = c("ctu", "year", "mode")) %>%
        unique() %>%
        rowwise() %>%
        mutate(
          transit_vmt = miles_traveled *
            aeo_adj * transit_adj *
            (1 + ((vmt_fee_adj + payd_ins_adj + cong_adjust) * cross_vmt)) *
            land_use_adj * park_price_adj * gas_adj / occupancy_adj *
            mode_stock_adj,
          stock = .stock
        ) %>%
        select(type, stock, scenario, ctu, year, mode, aeo_mode, vmt = transit_vmt)

      # return(vmt_forecast)
    } else if (.mode == "PLDV") {
      # passenger light duty --------
      # browser()

      if (.stock == "PHEVStock") {
        # browser()
        # account for proportion of PHEV electric and gas
        phev_proportion <- tb %>%
          dplyr::filter(mode == mode, var == "PHEVPr") %>%
          dplyr::select(mode, var, ctu, year,
            phev_prop_electric = value, aeo_mode, type
          )

        tb_vmt <- tb_vmt %>%
          left_join(phev_proportion) %>%
          mutate(miles_traveled = case_when(
            .phev_electric == TRUE ~
              miles_traveled * phev_prop_electric,
            .phev_electric == FALSE ~
              miles_traveled * (1 - phev_prop_electric)
          )) %>%
          select(names(tb_vmt))
      }


      mode_stock <- vmt_stock_proportion(
        .tb = tb,
        .mode = .mode,
        .stock = .stock
      )


      # All transit
      at_adjustment <- tb %>%
        filter(mode == "AT", var == .variable) %>%
        select(ctu, year, active_transportation_adj = value)

      ann_energy_outlook <- vmt_annual_energy_outlook(
        tb = tb,
        .aeo_scenario = .aeo_scenario,
        .mode = .mode
      )

      trans_service <- vmt_transit_service(
        tb = tb,
        .mode = .mode,
        .transit_service_pct = .transit_service_pct,
        .elast = .elast,
        .enviro_factors = .enviro_factors
      )

      fc_adjustments <- vmt_road_policy(
        .mode = .mode,
        .pass_tb = tb,
        .tb_vmt = tb_vmt,
        .tb_fuel_cost_mile = .tb_fuel_cost_mile,
        .vmt_fee = .vmt_fee,
        .cong_price = .cong_price,
        .gas_tax = .gas_tax,
        .payd_fee = .payd_fee,
        .stock = .stock,
        .phev_electric = .phev_electric,
        .freight_vmt_fee = .freight_vmt_fee,
        .enviro_factors = .enviro_factors,
        .elast = .elast
      )

      land_use <- vmt_land_use_change(
        .type = "DRIVE",
        .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
        .pop_dens_pct_change = .pop_dens_pct_change,
        .emp_dens_pct_change = .emp_dens_pct_change,
        .land_use_diversity_pct_change = .land_use_diversity_pct_change,
        .intersection_design_pct_change = .intersection_design_pct_change,
        .job_access_pct_change = .job_access_pct_change,
        .transit_dist_pct_change = .transit_dist_pct_change,
        .elast_5d = .elast_5d
      )

      parking <- vmt_parking_policy(
        tb = tb,
        .mode = .mode,
        .freight_parking_price = .freight_parking_price,
        .parking_price = .parking_price,
        .elast = .elast
      )

      veh_occupancy <- vmt_vehicle_occupancy(
        tb = tb,
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .stock = .stock,
        .transit_avo_pct = .transit_avo_pct,
        .pldv_avo_pct = .pldv_avo_pct,
        .enviro_factors = .enviro_factors
      )


      telework_adjust <- vmt_telework(
        .pass_tb = tb,
        .mode = .mode,
        .telework_pct = .telework_pct
      )


      # formula is such
      # pldv_vmt <- miles_traveled - transit shift * AV adjustment *
      # aeo adjustment *
      # vmt_fee_adj
      # cong_adj *
      # gas_adj *
      # park_price_adj *
      # land_use_adj *
      # telework_adj /
      # occupancy_adj
      # * mode_stock_adj

      tb_fin <- left_join(tb_vmt, ann_energy_outlook, by = "year") %>%
        left_join(trans_service, by = c("ctu", "year")) %>%
        left_join(fc_adjustments, by = c("ctu", "year")) %>%
        left_join(land_use, by = c("year")) %>%
        left_join(parking, by = c("year", "ctu")) %>%
        left_join(veh_occupancy, by = c("year", "ctu")) %>%
        left_join(telework_adjust, by = c("year")) %>%
        left_join(mode_stock, by = c("ctu", "year", "mode")) %>%
        left_join(at_adjustment,
          by = c("year", "ctu")
        ) %>%
        unique() %>%
        rowwise() %>%
        mutate(
          pass_ld_vmt =
            (miles_traveled - transit_adj) *
              aeo_adj *
              vmt_fee_adj * cong_adjust * gas_adj *
              telework_adj * land_use_adj *
              park_price_adj / occupancy_adj * mode_stock_adj,
          stock = .stock
        ) %>%
        select(type, stock, scenario,
          ctu, year, mode, aeo_mode,
          vmt = pass_ld_vmt
        ) %>%
        unique()


      # return(vmt_forecast)
    } else if (.mode == "AV") {
      # browser()
      # autonomous vehicle -----
      # av_vmt = miles_traveled  -
      # (active transportation adjustment * transit_adj) *
      # aeo_adj * vmt_fee_adj * cong_adjust *
      # gas_adj * park_price_adj * land_use_adj

      tb_vmt <- tb %>%
        dplyr::filter(
          mode == "PLDV",
          var == .variable
        ) %>%
        mutate(
          miles_traveled = value,
          scenario = .scenario
        ) %>%
        select(
          scenario, mode, ctu, year,
          aeo_mode, type, miles_traveled
        ) %>%
        unique()


      # mode_stock <- vmt_stock_proportion(.tb = tb,
      #                                    .mode = .mode,
      #                                    .stock = .stock)

      ann_energy_outlook <- vmt_annual_energy_outlook(
        tb = tb,
        .aeo_scenario = .aeo_scenario,
        .mode = .mode
      )


      fc_adjustments <- vmt_road_policy(
        .mode = .mode,
        .pass_tb = tb,
        .tb_vmt = tb_vmt,
        .tb_fuel_cost_mile = .tb_fuel_cost_mile,
        .vmt_fee = .vmt_fee,
        .cong_price = .cong_price,
        .gas_tax = .gas_tax,
        .payd_fee = .payd_fee,
        .stock = .stock,
        .phev_electric = .phev_electric,
        .freight_vmt_fee = .freight_vmt_fee,
        .enviro_factors = .enviro_factors,
        .elast = .elast
      )

      land_use <- vmt_land_use_change(
        .type = "DRIVE",
        .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
        .pop_dens_pct_change = .pop_dens_pct_change,
        .emp_dens_pct_change = .emp_dens_pct_change,
        .land_use_diversity_pct_change = .land_use_diversity_pct_change,
        .intersection_design_pct_change = .intersection_design_pct_change,
        .job_access_pct_change = .job_access_pct_change,
        .transit_dist_pct_change = .transit_dist_pct_change, .elast_5d = .elast_5d
      )

      parking <- vmt_parking_policy(
        tb = tb,
        .mode = .mode,
        .freight_parking_price = .freight_parking_price,
        .parking_price = .parking_price,
        .elast = .elast
      )

      veh_occupancy <- vmt_vehicle_occupancy(
        tb = tb,
        .tb_vmt = tb_vmt,
        .mode = "PLDV",
        .stock = .stock,
        .transit_avo_pct = .transit_avo_pct,
        .pldv_avo_pct = .pldv_avo_pct,
        .enviro_factors = .enviro_factors
      )

      at_adjustment <- tb %>%
        filter(mode == "AT", var == .variable) %>%
        select(year, ctu, at_adjust = value)

      trans_service <- vmt_transit_service(
        tb = tb,
        .mode = .mode,
        .transit_service_pct = .transit_service_pct,
        .elast = .elast
      )

      tb_fin <- tb_vmt %>%
        left_join(ann_energy_outlook, by = c("year")) %>%
        left_join(at_adjustment, by = c("year", "ctu")) %>%
        left_join(trans_service, by = c("year", "ctu")) %>%
        left_join(fc_adjustments, by = c("ctu", "year")) %>%
        left_join(land_use, by = c("year")) %>%
        left_join(parking, by = c("year", "ctu")) %>%
        left_join(veh_occupancy, by = c("year", "ctu")) %>%
        unique() %>%
        rowwise() %>%
        mutate(
          stock = .stock,
          mode = .mode,
          av_vmt = ((miles_traveled - (at_adjust * transit_adj) *
            aeo_adj * vmt_fee_adj *
            cong_adjust * gas_adj * park_price_adj *
            land_use_adj * .enviro_factors$VMT_AV) / occupancy_adj)
        ) %>%
        select(type, stock, scenario, ctu, year, mode, aeo_mode, vmt = av_vmt)

      return(tb_fin)
    } else if (.mode == "DRS") {
      # dynamic ride share  -----

      cli::cli_abort("Use calc_vmt_forecast_drs() for dynamic ride sharing VMT")
    } else if (.mode == "SUT") {
      # single truck --------
      # browser()

      mode_stock <- vmt_stock_proportion(
        .tb = tb,
        .mode = .mode,
        .stock = .stock
      )

      ann_energy_outlook <- vmt_annual_energy_outlook(
        tb = tb,
        .aeo_scenario = .aeo_scenario,
        .mode = .mode
      )

      veh_occupancy <- vmt_vehicle_occupancy(
        tb = tb,
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .stock = .stock,
        .transit_avo_pct = .transit_avo_pct,
        .pldv_avo_pct = .pldv_avo_pct,
        .enviro_factors = .enviro_factors
      )


      parking <- vmt_parking_policy(
        tb = tb,
        .mode = .mode,
        .parking_price = .parking_price,
        .freight_parking_price = .freight_parking_price,
        .elast = .elast
      )

      fc_adjustments <- vmt_road_policy(
        .mode = .mode,
        .pass_tb = tb,
        .tb_vmt = tb_vmt,
        .tb_fuel_cost_mile = .tb_fuel_cost_mile,
        .vmt_fee = .vmt_fee,
        .cong_price = .cong_price,
        .gas_tax = .gas_tax,
        .payd_fee = .payd_fee,
        .stock = .stock,
        .phev_electric = .phev_electric,
        .freight_vmt_fee = .freight_vmt_fee,
        .enviro_factors = .enviro_factors,
        .elast = .elast
      )



      # miles_traveled * aeo_adj * vmt_fee_adj * parking_adj / occupancy_adj



      tb_fin <- left_join(tb_vmt, ann_energy_outlook, by = c("year")) %>%
        left_join(parking, by = c("year", "ctu")) %>%
        left_join(veh_occupancy, by = c("year", "ctu")) %>%
        left_join(fc_adjustments, by = "year", "ctu") %>%
        left_join(mode_stock, by = c("ctu", "year", "mode")) %>%
        unique() %>%
        rowwise() %>%
        mutate(
          stock = .stock,
          sut_vmt = (miles_traveled * aeo_adj *
            vmt_fee_adj * park_price_adj / occupancy_adj) *
            mode_stock_adj
        ) %>%
        select(type, stock, scenario, ctu,
          year, mode, aeo_mode,
          vmt = sut_vmt
        )

      # return(vmt)
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

      mode_stock <- vmt_stock_proportion(
        .tb = tb,
        .mode = .mode,
        .stock = .stock
      )

      ann_energy_outlook <- vmt_annual_energy_outlook(
        tb = tb,
        .aeo_scenario = .aeo_scenario,
        .mode = .mode
      )


      veh_occupancy <- vmt_vehicle_occupancy(
        tb = tb,
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .stock = .stock,
        .transit_avo_pct = .transit_avo_pct,
        .pldv_avo_pct = .pldv_avo_pct,
        .enviro_factors = .enviro_factors
      )

      fc_adjustments <- vmt_road_policy(
        .mode = .mode,
        .pass_tb = tb,
        .tb_vmt = tb_vmt,
        .tb_fuel_cost_mile = .tb_fuel_cost_mile,
        .vmt_fee = .vmt_fee,
        .cong_price = .cong_price,
        .gas_tax = .gas_tax,
        .payd_fee = .payd_fee,
        .stock = .stock,
        .phev_electric = .phev_electric,
        .freight_vmt_fee = .freight_vmt_fee,
        .enviro_factors = .enviro_factors,
        .elast = .elast
      )


      tb_fin <- left_join(tb_vmt, ann_energy_outlook, by = c("year")) %>%
        left_join(veh_occupancy, by = c("year", "ctu")) %>%
        left_join(fc_adjustments, by = c("year")) %>%
        left_join(mode_stock, by = c("ctu", "year", "mode")) %>%
        unique() %>%
        rowwise() %>%
        mutate(
          stock = .stock,
          cut_vmt = (miles_traveled * aeo_adj * vmt_fee_adj / occupancy_adj) *
            mode_stock_adj
        ) %>%
        select(type, stock, scenario, ctu, year, mode, aeo_mode, vmt = cut_vmt)

      # return(vmt)


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
        .type = "WALK",
        .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
        .pop_dens_pct_change = .pop_dens_pct_change,
        .emp_dens_pct_change = .emp_dens_pct_change,
        .land_use_diversity_pct_change = .land_use_diversity_pct_change,
        .intersection_design_pct_change = .intersection_design_pct_change,
        .job_access_pct_change = .job_access_pct_change,
        .transit_dist_pct_change = .transit_dist_pct_change,
        .elast_5d = .elast_5d
      )



      tb_fin <- left_join(tb_vmt, land_use, by = c("year")) %>%
        unique() %>%
        rowwise() %>%
        mutate(
          stock = .stock,
          walk_vmt = miles_traveled * land_use_adj
        ) %>%
        select(type, stock, scenario, ctu,
          year, mode, aeo_mode,
          vmt = walk_vmt
        )

      # return(vmt)
    } else if (.mode == "BS") {
      # school bus-----

      # miles_traveled * aeo_adj/occupancy_adj

      mode_stock <- vmt_stock_proportion(
        .tb = tb,
        .mode = .mode,
        .stock = .stock
      )

      ann_energy_outlook <- vmt_annual_energy_outlook(
        tb = tb,
        .aeo_scenario = .aeo_scenario,
        .mode = .mode
      )


      veh_occupancy <- vmt_vehicle_occupancy(
        tb = tb,
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .stock = .stock,
        .transit_avo_pct = .transit_avo_pct,
        .pldv_avo_pct = .pldv_avo_pct,
        .enviro_factors = .enviro_factors
      )

      tb_fin <- left_join(tb_vmt, veh_occupancy, c("year", "ctu")) %>%
        left_join(ann_energy_outlook, by = "year") %>%
        left_join(mode_stock, by = c("ctu", "year", "mode")) %>%
        unique() %>%
        rowwise() %>%
        mutate(
          stock = .stock,
          school_bus_vmt = (miles_traveled * aeo_adj
            / occupancy_adj) * mode_stock_adj
        ) %>%
        select(type, stock, scenario, ctu, year, mode,
          aeo_mode,
          vmt = school_bus_vmt
        )
    } else if (.mode == "FR") {
      # freight rail ------
      # browser()
      # miles_traveled * aeo_adj / occupancy_adj

      mode_stock <- vmt_stock_proportion(
        .tb = tb,
        .mode = .mode,
        .stock = .stock
      )

      ann_energy_outlook <- vmt_annual_energy_outlook(
        tb = tb,
        .aeo_scenario = .aeo_scenario,
        .mode = .mode
      )

      veh_occupancy <- vmt_vehicle_occupancy(
        tb = tb,
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .stock = .stock,
        .transit_avo_pct = .transit_avo_pct,
        .pldv_avo_pct = .pldv_avo_pct,
        .enviro_factors = .enviro_factors
      )



      tb_fin <- left_join(tb_vmt, ann_energy_outlook, by = c("year")) %>%
        left_join(veh_occupancy, by = c("year", "ctu")) %>%
        left_join(mode_stock, by = c("ctu", "year", "mode")) %>%
        unique() %>%
        rowwise() %>%
        mutate(
          stock = .stock,
          fr_vmt = miles_traveled * aeo_adj / occupancy_adj * mode_stock_adj
        ) %>%
        select(type, stock, scenario, ctu, year, mode, aeo_mode, vmt = fr_vmt)

      # return(vmt)
    } else if (.mode %in% c("MM", "AIR", "WAT")) {
      # freight multimodal, air, water-----

      # browser()

      mode_stock <- vmt_stock_proportion(
        .tb = tb,
        .mode = .mode,
        .stock = .stock
      )

      ann_energy_outlook <- vmt_annual_energy_outlook(
        tb = tb,
        .aeo_scenario = .aeo_scenario,
        .mode = .mode
      )

      veh_occupancy <- vmt_vehicle_occupancy(
        tb = tb,
        .tb_vmt = tb_vmt,
        .mode = .mode,
        .stock = .stock,
        .transit_avo_pct = .transit_avo_pct,
        .pldv_avo_pct = .pldv_avo_pct,
        .enviro_factors = .enviro_factors
      )

      tb_fin <- left_join(tb_vmt, ann_energy_outlook, by = c("year")) %>%
        left_join(veh_occupancy, by = c("year", "ctu")) %>%
        left_join(mode_stock, by = c("ctu", "year", "mode")) %>%
        unique() %>%
        rowwise() %>%
        mutate(
          scenario = .scenario,
          stock = .stock,
          vmt = miles_traveled * aeo_adj / occupancy_adj * mode_stock_adj,
          vmt = case_when(
            vmt == Inf | is.na(vmt) | vmt < 0 ~ 0,
            TRUE ~ vmt
          )
        ) %>%
        select(type, stock, scenario, ctu, year, mode, aeo_mode, vmt) %>%
        unique()

      # return(tb_fin) # in thousands of miles
    }
  } else if (.mode == "WALK" | .mode == "BIKE") {
    # BAU and walk/bike -----
    # browser()

    # AVO for all bike and walk is 1
    avo <- tb %>%
      dplyr::filter(
        mode == .mode,
        var %in% c(
          "AVO"
        )
      ) %>%
      select(mode, ctu, year, aeo_mode, type, avo_val = value) %>%
      unique()

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
      left_join(avo, by = c("mode", "year", "aeo_mode", "type", "ctu")) %>%
      rowwise() %>%
      mutate(
        vmt = PMT / avo_val,
        scenario = .scenario,
        stock = .stock
      ) %>%
      select(type, scenario, ctu, year, mode, aeo_mode, vmt) %>%
      unique()
  } else {
    # all other BAU------
    # browser()
    mode_stock <- vmt_stock_proportion(
      .tb = tb,
      .mode = .mode,
      .stock = .stock
    )


    ann_energy_outlook <- vmt_annual_energy_outlook(
      tb = tb,
      .aeo_scenario = .aeo_scenario,
      .mode = .mode
    )


    veh_occupancy <- vmt_vehicle_occupancy(
      tb = tb,
      .tb_vmt = tb_vmt,
      .mode = .mode,
      .stock = .stock,
      .transit_avo_pct = .transit_avo_pct,
      .pldv_avo_pct = .pldv_avo_pct,
      .enviro_factors = .enviro_factors
    )

    tb_fin <- tb_vmt %>%
      left_join(veh_occupancy, by = c("ctu", "year")) %>%
      left_join(ann_energy_outlook, by = c("year")) %>%
      left_join(mode_stock, by = c("ctu", "year", "mode")) %>%
      unique() %>%
      rowwise() %>%
      mutate(
        scenario = .scenario,
        stock = .stock,
        vmt = (miles_traveled * aeo_adj) / occupancy_adj * mode_stock_adj,
        vmt = case_when(
          vmt == Inf | is.na(vmt) ~ 0,
          TRUE ~ vmt
        )
      ) %>%
      select(type, stock, scenario, ctu, year, mode, aeo_mode, vmt) %>%
      unique()
  }

  # final return -----
  return(tb_fin)
}
