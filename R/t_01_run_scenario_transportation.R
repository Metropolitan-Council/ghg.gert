#' @title  Main function to call other functions for determining VMT,
#'      direct GHG, indirect GHG, and costs
#'
#' @param pass_tb input table for passenger modes. Should have columns `mode`, `var`, `ctu`,
#'    and one for each year. Package provided dataset `transportation_data$passenger` is suitable.
#' @param freight_tb input table for freight modes. Should have columns `mode`, `var`, `ctu`,
#'    and one for each year. Package provided dataset `transportation_data$freight` is suitable.
#' @param .electric_scenario electricity scenario
#' @param .aeo_scenario character, selected EIA Annual Energy Outlook scenario.
#'      Default is `"REF"`
#' @param .mit_bau_summary input of BAU data for calculations in MIT scenario
#' @param .calc_transp_cost logical, whether to calculate transportation cost tables.
#'   Default is `FALSE`. Changing this value to `TRUE` increased runtime.
#' @param .calc_transp_fuel_use  logical, whether to calculate transportation fuel use tables.
#'   Default is `FALSE`. Changing this value to `TRUE` increased runtime.
#' @param .calc_transp_ghg_embodied logical, whether to calculate embodied emissions.
#'   Default is `FALSE`. Changing this value to `TRUE` increased runtime.
#'
#' @inheritParams calc_vmt_forecast
#' @inheritParams run_scenario_land_use
#' @inheritParams run_scenario_building
#' @inheritParams filter_ctu
#' @inheritParams adj_fleet_shares
#' @inheritParams filter_ctu
#' @inheritParams vmt_annual_energy_outlook
#' @inheritParams vmt_land_use_change
#' @inheritParams vmt_parking_policy
#' @inheritParams vmt_road_policy
#' @inheritParams vmt_telework
#' @inheritParams vmt_stock_proportion
#' @inheritParams vmt_transit_service
#' @inheritParams vmt_vehicle_occupancy
#'
#' @return A named list of four objects: `passenger`, `passenger_all`, `freight`, and `freight_all`.
#'
#' @export
#' @importFrom dplyr filter select case_when across bind_rows cur_column
#' @importFrom tidyselect all_of
#' @importFrom tibble tibble
#' @importFrom cli cli_warn
#'
#' @family transportation
run_scenario_transportation <- function(pass_tb = transportation_data$passenger,
                                        freight_tb = transportation_data$freight,
                                        .selected_ctu = "all",
                                        .calc_transp_cost = FALSE,
                                        .calc_transp_fuel_cost_mile = FALSE,
                                        .calc_transp_fuel_use = FALSE,
                                        .calc_transp_ghg_embodied = FALSE,
                                        .grid_decarbonization_pct = 0.6,
                                        .scenario = "BAU",
                                        .electric_scenario = "ER",
                                        .aeo_scenario = "REF",
                                        .transit_avo_pct = 0,
                                        .pldv_avo_pct = 0,
                                        .transit_service_pct = 0,
                                        .vmt_fee = 0,
                                        .payd_fee = 0,
                                        .gas_tax = 0,
                                        .parking_price = 0,
                                        .freight_parking_price = 0,
                                        .cong_price = 0,
                                        .freight_vmt_fee = 0,
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
                                        .enviro_factors = ghg.sp::enviro_factors,
                                        .elast = elast,
                                        .elast_5d = elast_5d,
                                        .factor_values = ghg.sp::factor_values) {
  # browser()
  pass_tb <- filter_ctu(pass_tb, .selected_ctu)
  freight_tb <- filter_ctu(freight_tb, .selected_ctu)

  l_names <- c(
    "electric_scenario",
    "aeo_scenario",
    "vmt_fee",
    "payd_fee",
    "parking_price",
    "transit_avo_pct",
    "transit_service_pct",
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
    "calc_transp_cost",
    "calc_transp_fuel_use",
    "calc_transp_ghg_embodied",
    "grid_decarbonization_pct",
    "freight_vmt_fee"
  )

  l_vals <- list(
    .electric_scenario,
    .aeo_scenario,
    .vmt_fee,
    .payd_fee,
    .parking_price,
    .transit_avo_pct,
    .transit_service_pct,
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
    .calc_transp_cost,
    .calc_transp_fuel_use,
    .calc_transp_ghg_embodied,
    .grid_decarbonization_pct,
    .freight_vmt_fee
  )

  purrr::map2(l_names, l_vals, check_inputs)

  if (.transit_service_pct != 0 & .transit_avo_pct < (
    .enviro_factors$TRANSIT_SERVICE_AVO_MIN * .transit_service_pct)) {
    cli::cli_warn(c(
      "Transit AVO adjustment too low for given transit service adjustment",
      paste0(
        "Changing `.transit_avo_pct` to ",
        .transit_service_pct * .enviro_factors$TRANSIT_SERVICE_AVO_MIN
      )
    ))

    .transit_avo_pct <- .enviro_factors$TRANSIT_SERVICE_AVO_MIN * .transit_service_pct
  }

  # adjust fleet size if neccessary -----
  if (.vmt_fee > 0 |
    .payd_fee > 0 |
    .gas_tax > 0 |
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
      .enviro_factors = .enviro_factors,
      .elast = .elast
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
    .selected_ctu = .selected_ctu,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo_pct = .transit_avo_pct,
    .pldv_avo_pct = .pldv_avo_pct,
    .transit_service_pct = .transit_service_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .freight_parking_price = .freight_parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary,
    .enviro_factors = .enviro_factors,
    .elast = .elast,
    .elast_5d = .elast_5d,
    .factor_values = .factor_values,
    .calc_transp_cost = .calc_transp_cost,
    .calc_transp_fuel_use = .calc_transp_fuel_use,
    .calc_transp_ghg_embodied = .calc_transp_ghg_embodied,
    .grid_decarbonization_pct = .grid_decarbonization_pct
  )


  # transit buses -----
  bus_transit <- scen_transit_bus(
    .pass_tb = pass_tb,
    .selected_ctu = .selected_ctu,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo_pct = .transit_avo_pct,
    .pldv_avo_pct = .pldv_avo_pct,
    .transit_service_pct = .transit_service_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .freight_parking_price = .freight_parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary,
    .enviro_factors = .enviro_factors,
    .elast = .elast,
    .elast_5d = .elast_5d,
    .factor_values = .factor_values,
    .calc_transp_cost = .calc_transp_cost,
    .calc_transp_fuel_use = .calc_transp_fuel_use,
    .calc_transp_ghg_embodied = .calc_transp_ghg_embodied
  )

  # transit rail -----

  rail_transit <- scen_transit_rail(
    .pass_tb = pass_tb,
    .selected_ctu = .selected_ctu,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo_pct = .transit_avo_pct,
    .pldv_avo_pct = .pldv_avo_pct,
    .transit_service_pct = .transit_service_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .freight_parking_price = .freight_parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary,
    .enviro_factors = .enviro_factors,
    .elast = .elast,
    .elast_5d = .elast_5d,
    .factor_values = .factor_values,
    .calc_transp_cost = .calc_transp_cost,
    .calc_transp_fuel_use = .calc_transp_fuel_use,
    .calc_transp_ghg_embodied = .calc_transp_ghg_embodied
  )

  # school bus-----
  school_bus <- scen_school_bus(
    .pass_tb = pass_tb,
    .selected_ctu = .selected_ctu,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo_pct = .transit_avo_pct,
    .pldv_avo_pct = .pldv_avo_pct,
    .transit_service_pct = .transit_service_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .freight_parking_price = .freight_parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary,
    .enviro_factors = .enviro_factors,
    .elast = .elast,
    .elast_5d = .elast_5d,
    .factor_values = .factor_values,
    .calc_transp_cost = .calc_transp_cost,
    .calc_transp_fuel_use = .calc_transp_fuel_use,
    .calc_transp_ghg_embodied = .calc_transp_ghg_embodied
  )


  # walk and bike ----
  walk_bike <- scen_walk_bike(
    .pass_tb = pass_tb,
    .selected_ctu = .selected_ctu,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo_pct = .transit_avo_pct,
    .pldv_avo_pct = .pldv_avo_pct,
    .transit_service_pct = .transit_service_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .freight_parking_price = .freight_parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary,
    .enviro_factors = .enviro_factors,
    .elast = .elast,
    .elast_5d = .elast_5d,
    .factor_values = .factor_values
  )

  # Freight -------------------------------
  # (measured in ton-miles NOT miles)

  # freight truck ------
  freight_truck <- scen_freight_truck(
    .freight_tb = freight_tb,
    .selected_ctu = .selected_ctu,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo_pct = .transit_avo_pct,
    .pldv_avo_pct = .pldv_avo_pct,
    .transit_service_pct = .transit_service_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .freight_parking_price = .freight_parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary,
    .enviro_factors = .enviro_factors,
    .elast = .elast,
    .elast_5d = .elast_5d,
    .factor_values = .factor_values
  )

  # freight rail -----

  freight_rail <- scen_freight_rail(
    .freight_tb = freight_tb,
    .selected_ctu = .selected_ctu,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo_pct = .transit_avo_pct,
    .pldv_avo_pct = .pldv_avo_pct,
    .transit_service_pct = .transit_service_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .freight_parking_price = .freight_parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary,
    .enviro_factors = .enviro_factors,
    .elast = .elast,
    .factor_values = .factor_values,
    .elast_5d = .elast_5d
  )

  # freight multi-modal, air, and water -----

  freight_multi_air_wat <- scen_air_water_multi(
    .freight_tb = freight_tb,
    .selected_ctu = .selected_ctu,
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo_pct = .transit_avo_pct,
    .pldv_avo_pct = .pldv_avo_pct,
    .transit_service_pct = .transit_service_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .parking_price = .parking_price,
    .freight_parking_price = .freight_parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary,
    .enviro_factors = .enviro_factors,
    .elast = .elast,
    .factor_values = .factor_values,
    .elast_5d = .elast_5d
  )

  # Finish up -----
  ## passenger ------
  # browser()
  passenger <- list(
    PLDV = passenger_light_duty,
    RAIL = rail_transit,
    BU_BRT = bus_transit,
    WALK_BIKE = walk_bike,
    BS = school_bus
  )


  pass_vmt <- dplyr::bind_rows(
    passenger_light_duty$vmt,
    rail_transit$vmt,
    bus_transit$vmt,
    walk_bike$vmt,
    school_bus$vmt
  )

  pass_dir_ghg <- dplyr::bind_rows(
    passenger_light_duty$dir_ghg,
    rail_transit$dir_ghg,
    bus_transit$dir_ghg,
    walk_bike$dir_ghg,
    school_bus$dir_ghg
  )

  pass_all <- dplyr::left_join(
    pass_vmt, pass_dir_ghg,
    c(
      "type",
      "scenario",
      "ctu",
      "year",
      "mode",
      "aeo_mode",
      "class"
    )
  )

  if (.calc_transp_ghg_embodied == TRUE) {
    pass_emb_ghg <- dplyr::bind_rows(
      passenger_light_duty$emb_ghg,
      rail_transit$emb_ghg,
      bus_transit$emb_ghg,
      # walk_bike$emb_ghg,
      school_bus$emb_ghg
    ) %>%
      mutate(scenario = .scenario)

    pass_all <- pass_all %>%
      dplyr::left_join(pass_emb_ghg, c(
        "type", "scenario", "ctu",
        "year", "mode", "aeo_mode", "class"
      ))
  }

  if (.calc_transp_fuel_use == TRUE) {
    pass_fuel <- dplyr::bind_rows(
      passenger_light_duty$fuel_use,
      bus_transit$fuel_use,
      rail_transit$fuel_use,
      # walk_bike$fuel_use,
      school_bus$fuel_use
    )

    pass_all <- pass_all %>%
      dplyr::left_join(pass_fuel, by = c(
        "type", "scenario",
        "ctu", "year", "mode",
        "aeo_mode", "class"
      ))
  }

  if (.calc_transp_cost == TRUE) {
    pass_cost <- dplyr::bind_rows(
      passenger_light_duty$cost,
      bus_transit$cost,
      rail_transit$cost,
      school_bus$cost,
      # walk_bike$cost
    )

    pass_all <- pass_all %>%
      dplyr::left_join(pass_cost, by = c(
        "type", "scenario", "ctu",
        "year", "mode", "aeo_mode", "class"
      ))
  }

  # browser()

  pass_all <- pass_all %>%
    unique()
  # %>%
  #   dplyr::select(
  #     type,
  #     scenario,
  #     ctu,
  #     year,
  #     mode,
  #     aeo_mode,
  #     stock,
  #     class,
  #     vmt,
  #     dir_ghg,
  #     ghg_embodied_source,
  #     ghg_embodied,
  #     fuel_use
  #   )


  ## freight -----
  freight_vmt <- dplyr::bind_rows(
    freight_multi_air_wat$vmt,
    freight_rail$vmt,
    freight_truck$vmt
  )

  freight_ghg <- dplyr::bind_rows(
    freight_multi_air_wat$ghg,
    freight_truck$dir_ghg,
    freight_rail$dir_ghg
  )

  freight_all <- dplyr::left_join(freight_vmt, freight_ghg,
    by = c(
      "type", "scenario", "ctu",
      "year", "mode", "aeo_mode", "class"
    )
  )

  freight <- list(
    AIR_WAT_MM = freight_multi_air_wat,
    FRAIL = freight_rail,
    SUT_CUT = freight_truck
  )

  app_output <- dplyr::bind_rows(
    pass_all %>%
      dplyr::filter(year %in% c(
        "2018",
        "2040"
      )) %>%
      dplyr::select(ctu, year, scenario,
        direct = dir_ghg,
        mode
      ) %>%
      unique() %>%
      dplyr::mutate(
        module = "transportation",
        submodule = "people",
        # tonne == metric ton
        metric = "emissions_tonnes_co2e"
      ) %>%
      tidyr::pivot_longer(cols = c("direct")),
    freight_all %>%
      dplyr::filter(year %in% c(
        "2018",
        "2040"
      )) %>%
      dplyr::select(ctu, year, scenario, mode,
        direct = dir_ghg
      ) %>%
      unique() %>%
      dplyr::mutate(
        module = "transportation",
        submodule = "freight",
        metric = "emissions_tonnes_co2e"
      ) %>%
      tidyr::pivot_longer(cols = c("direct"))
  ) %>%
    dplyr::group_by(
      ctu, year, scenario, module, submodule, mode,
      metric, name
    ) %>%
    dplyr::summarize(value = sum(value, na.rm = T))


  return(
    list(
      "passenger" = passenger,
      "passenger_all" = pass_all,
      "freight" = freight,
      "freight_all" = freight_all,
      "app_data" = app_output
    )
  )
}
