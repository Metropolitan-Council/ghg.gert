#' @title  Main function to call other functions for determining VMT,
#'      direct GHG, indirect GHG, and costs
#'
#' @param pass_tb input table for passenger modes. Should have columns `mode`, `var`, `ctu`,
#'    and one for each year. Package provided dataset `transportation_data$passenger` is suitable.
#' @param freight_tb input table for freight modes. Should have columns `mode`, `var`, `ctu`,
#'    and one for each year. Package provided dataset `transportation_data$freight` is suitable.
#' @param .electric_scenario electricity scenario
#' @param .aeo_scenario selected EIA Annual Energy Outlook scenario
#' @param .drs_fuel_type input dynamic ride sharing (DRS) fuel type. Default is `0`.
#' @param .av_fuel_type input AV fuel type
#' @param .mit_bau_summary input of BAU data for calculations in MIT scenario
#' @inheritParams calc_vmt_forecast
#' @inheritParams adj_fleet_shares
#' @return A named list of four objects: `passenger`, `passenger_all`, `freight`, and `freight_all`.
#' @export
#' @importFrom dplyr filter select case_when across bind_rows cur_column
#' @importFrom tidyselect all_of
#' @importFrom tibble tibble
#' @family transportation
scenario_results <- function(pass_tb = transportation_data$passenger,
                             freight_tb = transportation_data$freight,
                             .scenario = "BAU",
                             .electric_scenario = "ER",
                             .aeo_scenario = "REF",
                             .transit_avo = 0,
                             .transit_rider_pct = 0,
                             .vmt_fee = 0,
                             .payd_fee = 0,
                             .gas_tax = 0,
                             .parking_price = 0,
                             .cong_price = 0,
                             .freight_vmt_fee = 0,
                             .drs_pct = 0,
                             .av_pct = 0,
                             .drs_fuel_type = "",
                             .av_fuel_type = "",
                             .pop_dens_pct_change = 0,
                             .emp_dens_pct_change = 0,
                             .land_use_diversity_pct_change = 0,
                             .intersection_design_pct_change = 0,
                             .job_access_pct_change = 0,
                             .transit_dist_pct_change = 0,
                             .comb_5d_impact_pct_change = 0,
                             .telework_pct = 0,
                             .bev_pct_sales = 0,
                             .phev_pct_sales = 0,
                             .hev_pct_sales = 0,
                             .mit_bau_summary = 0,
                             .enviro_factors = enviro_factors) {
  l_names <- c(
    "electric_scenario",
    "aeo_scenario",
    "vmt_fee",
    "payd_fee",
    "parking_price",
    "transit_avo",
    "transit_rider_pct",
    "av_pct",
    "drs_pct",
    "emp_dens_pct_change",
    "pop_dens_pct_change",
    "job_access_pct_change",
    "land_use_pct_change",
    "transit_dist_pct_change",
    "comb_5d_impact_pct_change",
    "telework_pct",
    "bev_pct_sales",
    "hev_pct_sales",
    "phev_pct_sales",
    "drs_pct_trip"
  )

  l_vals <- list(
    .electric_scenario,
    .aeo_scenario,
    .vmt_fee,
    .payd_fee,
    .parking_price,
    .transit_avo,
    .transit_rider_pct,
    .av_pct,
    .drs_pct,
    .emp_dens_pct_change,
    .pop_dens_pct_change,
    .job_access_pct_change,
    .land_use_diversity_pct_change,
    .transit_dist_pct_change,
    .comb_5d_impact_pct_change,
    .telework_pct,
    .bev_pct_sales,
    .hev_pct_sales,
    .phev_pct_sales,
    .drs_pct
  )

  purrr::map2(l_names, l_vals, check_inputs)


  # adjust fleet size if neccessary -----
  if (.vmt_fee > 0 |
    .payd_fee > 0 |
    .drs_pct > 0 |
    .drs_pct > 0 |
    .gas_tax > 0 |
    .av_pct > 0 |
    .bev_pct_sales > 0 |
    .hev_pct_sales > 0 |
    .phev_pct_sales > 0) {
    # browser()

    adj_fleet <- adj_fleet_shares(
      .pass_tb = pass_tb,
      .freight_tb = freight_tb,
      .bev_pct_sales = .bev_pct_sales,
      .phev_pct_sales = .phev_pct_sales,
      .hev_pct_sales = .hev_pct_sales,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .drs_pct = .drs_pct,
      .av_pct = .av_pct,
      .enviro_factors = .enviro_factors
    )

    pass_tb <- adj_fleet$pass
    freight_tb <- adj_fleet$freight
  }


  # Sequence for each
  # 1. Establish `type`, `var`, `mode`
  # 2. Establish `stock`, `mpg`, `class`
  # 3. Calculate fuel cost per mile with `calc_fuel_cost_mile()`
  # 4. Calculate VMT with `calc`

  # passenger light-duty -----
  passenger_light_duty <- scen_passenger_light_duty(
    .pass_tb = pass_tb,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo = .transit_avo,
    .transit_rider_pct = .transit_rider_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .drs_pct = .drs_pct,
    .av_pct = .av_pct,
    .drs_fuel_type = .drs_fuel_type,
    .av_fuel_type = .av_fuel_type,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary
  )


  # transit buses -----
  bus_transit <- scen_transit_bus(
    .pass_tb = pass_tb,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo = .transit_avo,
    .transit_rider_pct = .transit_rider_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .drs_pct = .drs_pct,
    .av_pct = .av_pct,
    .drs_fuel_type = .drs_fuel_type,
    .av_fuel_type = .av_fuel_type,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary
  )

  # transit rail -----

  rail_transit <- scen_transit_rail(
    .pass_tb = pass_tb,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo = .transit_avo,
    .transit_rider_pct = .transit_rider_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .drs_pct = .drs_pct,
    .av_pct = .av_pct,
    .drs_fuel_type = .drs_fuel_type,
    .av_fuel_type = .av_fuel_type,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary
  )

  # school bus-----
  school_bus <- scen_school_bus(
    .pass_tb = pass_tb,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo = .transit_avo,
    .transit_rider_pct = .transit_rider_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .drs_pct = .drs_pct,
    .av_pct = .av_pct,
    .drs_fuel_type = .drs_fuel_type,
    .av_fuel_type = .av_fuel_type,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary
  )


  # walk and bike ----
  walk_bike <- scen_walk_bike(
    .pass_tb = pass_tb,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo = .transit_avo,
    .transit_rider_pct = .transit_rider_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .drs_pct = .drs_pct,
    .av_pct = .av_pct,
    .drs_fuel_type = .drs_fuel_type,
    .av_fuel_type = .av_fuel_type,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary
  )

  # Dynamic Ride Sharing -----

  dynamic_ride_share <- scen_dynamic_ride_sharing(
    .pass_tb = pass_tb,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo = .transit_avo,
    .transit_rider_pct = .transit_rider_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .drs_pct = .drs_pct,
    .av_pct = .av_pct,
    .drs_fuel_type = .drs_fuel_type,
    .av_fuel_type = .av_fuel_type,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary
  )

  # autonomous vehicles -----

  auto_veh <- scen_autonomous_vehicle(
    .pass_tb = pass_tb,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo = .transit_avo,
    .transit_rider_pct = .transit_rider_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .drs_pct = .drs_pct,
    .av_pct = .av_pct,
    .drs_fuel_type = .drs_fuel_type,
    .av_fuel_type = .av_fuel_type,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary
  )



  # Freight -------------------------------
  # (measured in ton-miles NOT miles)

  # freight truck ------
  freight_truck <- scen_freight_truck(
    .freight_tb = freight_tb,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo = .transit_avo,
    .transit_rider_pct = .transit_rider_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .drs_pct = .drs_pct,
    .av_pct = .av_pct,
    .drs_fuel_type = .drs_fuel_type,
    .av_fuel_type = .av_fuel_type,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary
  )


  # freight rail -----

  freight_rail <- scen_freight_rail(
    .freight_tb = freight_tb,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo = .transit_avo,
    .transit_rider_pct = .transit_rider_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .drs_pct = .drs_pct,
    .av_pct = .av_pct,
    .drs_fuel_type = .drs_fuel_type,
    .av_fuel_type = .av_fuel_type,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary
  )



  # freight multi-modal, air, and water -----
  freight_multi_air_wat <- scen_air_water_multi(
    .freight_tb = freight_tb,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo = .transit_avo,
    .transit_rider_pct = .transit_rider_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .drs_pct = .drs_pct,
    .av_pct = .av_pct,
    .drs_fuel_type = .drs_fuel_type,
    .av_fuel_type = .av_fuel_type,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary
  )

  # Finish up -----
  ## passenger ------
  # browser()
  passenger <- list(
    PLDV = passenger_light_duty,
    RAIL = rail_transit,
    BU_BRT = bus_transit,
    WALK_BIKE = walk_bike,
    BS = school_bus,
    AV = auto_veh,
    DRS = dynamic_ride_share
  )


  pass_vmt <- bind_rows(
    passenger_light_duty$vmt,
    rail_transit$vmt,
    bus_transit$vmt,
    walk_bike$vmt,
    school_bus$vmt,
    auto_veh$vmt,
    dynamic_ride_share$vmt
  )

  pass_dir_ghg <- bind_rows(
    passenger_light_duty$dir_ghg,
    rail_transit$dir_ghg,
    bus_transit$dir_ghg,
    walk_bike$dir_ghg,
    school_bus$dir_ghg,
    auto_veh$dir_ghg,
    dynamic_ride_share$dir_ghg
  )

  pass_emb_ghg <- bind_rows(
    passenger_light_duty$emb_ghg,
    rail_transit$emb_ghg,
    bus_transit$emb_ghg,
    walk_bike$emb_ghg,
    school_bus$emb_ghg,
    auto_veh$emb_ghg,
    dynamic_ride_share$emb_ghg
  ) %>%
    mutate(scenario = .scenario)

  pass_fuel <- bind_rows(
    passenger_light_duty$fuel_use,
    bus_transit$fuel_use,
    rail_transit$fuel_use,
    walk_bike$fuel_use,
    school_bus$fuel_use,
    auto_veh$fuel_use,
    dynamic_ride_share$fuel_use
  )

  pass_cost <- bind_rows(
    passenger_light_duty$cost,
    bus_transit$cost,
    rail_transit$cost,
    school_bus$cost,
    walk_bike$cost,
    auto_veh$cost,
    dynamic_ride_share$cost
  )

  browser()

  pass_all <- left_join(
    pass_vmt, pass_dir_ghg,
    c("type", "scenario", "ctu", "year", "mode", "aeo_mode", "class")
  ) %>%
    anti_join(pass_cost) %>%
    left_join(pass_emb_ghg, c("type", "scenario", "ctu", "year", "mode", "aeo_mode", "class")) %>%
    left_join(pass_fuel, by = c("type", "scenario", "ctu", "year", "mode", "aeo_mode", "class")) %>%
    unique()


  ## freight -----
  freight_vmt <- bind_rows(
    freight_multi_air_wat$vmt,
    freight_rail$vmt,
    freight_truck$vmt
  )

  freight_ghg <- bind_rows(
    freight_multi_air_wat$ghg,
    freight_truck$dir_ghg,
    freight_rail$dir_ghg
  )


  freight_all <- left_join(freight_vmt, freight_ghg)

  freight <- list(
    AIR_WAT_MM = freight_multi_air_wat,
    FRAIL = freight_rail,
    SUT_CUT = freight_truck
  )

  # browser()


  return(
    list(
      "passenger" = passenger,
      "passenger_all" = pass_all,
      "freight" = freight,
      "freight_all" = freight_transpo_all
    )
  )
}
