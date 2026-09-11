#' @title  Main function to call other functions for determining VMT,
#'      direct GHG, indirect GHG, and costs
#'
#' @param pass_tb input table for passenger modes. Should have columns `mode`, `var`, `geog_name`, `geog_id`,
#'    and one for each year. Package provided dataset `transportation_data$passenger` is suitable.
#' @param freight_tb input table for freight modes. Should have columns `mode`, `var`, `geog_name`, `geog_id`,
#'    and one for each year. Package provided dataset `transportation_data$freight` is suitable.
#' @param .electric_scenario electricity scenario
#' @param .aeo_scenario character, selected EIA Annual Energy Outlook scenario.
#'      Default is `"REF"`
#' @param .calc_transp_cost logical, whether to calculate transportation cost tables.
#'   Default is `FALSE`. Changing this value to `TRUE` increased runtime.
#' @param .calc_transp_fuel_use  logical, whether to calculate transportation fuel use tables.
#'   Default is `FALSE`. Changing this value to `TRUE` increased runtime.
#' @param .calc_transp_fuel_cost_mile  logical, whether to calculate transportation fuel cost per mile tables.
#'   Default is `FALSE`. Changing this value to `TRUE` increased runtime.
#' @param .calc_transp_ghg_embodied logical, whether to calculate embodied emissions.
#'   Default is `FALSE`. Changing this value to `TRUE` increased runtime.
#' @param .enviro_factors list, named list of environmental factors. Default is
#'   `ghg.gert::enviro_factors`.
#'
#' @inheritParams calc_vmt_forecast
#' @inheritParams run_scenario_land_use
#' @inheritParams run_scenario_building
#' @inheritParams filter_ctu
#' @inheritParams adj_fleet_shares
#' @inheritParams calc_ghg_direct
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
#'    Emissions returned are in metric tons.
#'
#' @export
#' @importFrom dplyr filter select case_when across bind_rows cur_column
#' @importFrom tidyselect all_of
#' @importFrom tibble tibble
#' @importFrom cli cli_warn
#' @importFrom rlang exec
#' @importFrom purrr map
#'
#' @family transportation
run_module_transportation <- function(pass_tb = transportation_data$passenger,
                                      freight_tb = transportation_data$freight,
                                      .selected_ctu = "all",
                                      .parking_cost = ghg.gert::parking_cost,
                                      .vehicle_occupancy = ghg.gert::vehicle_occupancy,
                                      .calc_transp_cost = FALSE,
                                      .calc_transp_fuel_cost_mile = FALSE,
                                      .calc_transp_fuel_use = FALSE,
                                      .calc_transp_ghg_embodied = FALSE,
                                      .scenario = "BAU",
                                      .electric_scenario = "ER",
                                      .aeo_scenario = "REF",
                                      .cbtp_prop_targeted = ghg.gert::transportation_defaults$cbtp_prop_targeted,
                                      .cbtp_start_year = ghg.gert::transportation_defaults$cbtp_start_year,
                                      .transit_avo_pct = ghg.gert::transportation_defaults$transit_avo_pct,
                                      .pldv_avo_pct = ghg.gert::transportation_defaults$pldv_avo_pct,
                                      .transit_service_pct = ghg.gert::transportation_defaults$transit_service_pct,
                                      .vmt_fee = ghg.gert::transportation_defaults$vmt_fee,
                                      .payd_fee = ghg.gert::transportation_defaults$payd_fee,
                                      .gas_tax = ghg.gert::transportation_defaults$gas_tax,
                                      .parking_price = ghg.gert::transportation_defaults$parking_price,
                                      .freight_parking_price = ghg.gert::transportation_defaults$freight_parking_price,
                                      .cong_price = ghg.gert::transportation_defaults$cong_price,
                                      .freight_vmt_fee = ghg.gert::transportation_defaults$freight_vmt_fee,
                                      .pop_dens_pct_change = ghg.gert::transportation_defaults$pop_dens_pct_change,
                                      .emp_dens_pct_change = ghg.gert::transportation_defaults$emp_dens_pct_change,
                                      .land_use_diversity_pct_change = ghg.gert::transportation_defaults$land_use_diversity_pct_change,
                                      .intersection_design_pct_change = ghg.gert::transportation_defaults$intersection_design_pct_change,
                                      .intersection_density_pct_change = ghg.gert::transportation_defaults$intersection_density_pct_change,
                                      .job_access_pct_change = ghg.gert::transportation_defaults$job_access_pct_change,
                                      .transit_dist_pct_change = ghg.gert::transportation_defaults$transit_dist_pct_change,
                                      .comb_5d_impact_pct_change = ghg.gert::transportation_defaults$comb_5d_impact_pct_change,
                                      .telework_pct = ghg.gert::transportation_defaults$telework_pct,
                                      .vmt_reduction_pct = ghg.gert::transportation_defaults$vmt_reduction_pct,
                                      .bev_pct_sales = ghg.gert::transportation_defaults$bev_pct_sales,
                                      .hev_pct_sales = ghg.gert::transportation_defaults$hev_pct_sales,
                                      .bev_pct_stock = ghg.gert::transportation_defaults$bev_pct_stock,
                                      .hev_pct_stock = ghg.gert::transportation_defaults$hev_pct_stock,
                                      .enviro_factors = ghg.gert::enviro_factors,
                                      .elast = ghg.gert::elast,
                                      .elast_5d = ghg.gert::elast_5d,
                                      .fuel_economy = ghg.gert::fuel_economy,
                                      .factor_values = ghg.gert::factor_values) {
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
    "land_use_diversity_pct_change",
    "transit_dist_pct_change",
    "comb_5d_impact_pct_change",
    # "intersection_density_pct_change",
    "intersection_design_pct_change",
    "telework_pct",
    "bev_pct_sales",
    "hev_pct_sales",
    "bev_pct_stock",
    "hev_pct_stock",
    "calc_transp_cost",
    "calc_transp_fuel_use",
    "calc_transp_ghg_embodied",
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
    # .intersection_density_pct_change,
    .intersection_design_pct_change,
    .telework_pct,
    .bev_pct_sales,
    .hev_pct_sales,
    .bev_pct_stock,
    .hev_pct_stock,
    .calc_transp_cost,
    .calc_transp_fuel_use,
    .calc_transp_ghg_embodied,
    .freight_vmt_fee
  )

  # check inputs -----
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

  if ((.bev_pct_stock > 0 & .bev_pct_sales > 0) |
    (.hev_pct_stock > 0 & .hev_pct_sales > 0)) {
    cli::cli_abort("Cannot have both sales and stock adjustment factors")
  }


  if (.vmt_fee > 0 |
    .payd_fee > 0 |
    .gas_tax > 0 |
    .bev_pct_sales > 0 |
    .hev_pct_sales > 0 |

    .bev_pct_stock > 0 |
    .hev_pct_stock > 0) {
    if (.bev_pct_stock > 0 |
      .hev_pct_stock > 0) {
      adj_fleet <- adj_fleet_shares_stock(
        .pass_tb = pass_tb,
        .freight_tb = freight_tb,
        .selected_ctu = .selected_ctu,
        .bev_pct_stock = .bev_pct_stock,
        .hev_pct_stock = .hev_pct_stock,
        .vmt_fee = .vmt_fee,
        .payd_fee = .payd_fee,
        .gas_tax = .gas_tax,
        .enviro_factors = .enviro_factors,
        .elast = .elast
      )
    } else if (
      .bev_pct_sales > 0 |
        .hev_pct_sales > 0
    ) {
      adj_fleet <- adj_fleet_shares(
        .pass_tb = pass_tb,
        .freight_tb = freight_tb,
        .selected_ctu = .selected_ctu,
        .bev_pct_sales = .bev_pct_sales,
        .hev_pct_sales = .hev_pct_sales,
        .vmt_fee = .vmt_fee,
        .payd_fee = .payd_fee,
        .gas_tax = .gas_tax,
        .enviro_factors = .enviro_factors,
        .elast = .elast
      )
    }

    pass_tb <- adj_fleet$pass
    freight_tb <- adj_fleet$freight
  }


  # Passenger-----

  pass_funs <- c(
    mode_passenger_light_duty,
    mode_transit_bus,
    # mode_transit_rail,
    mode_walk_bike,
    mode_school_bus
  )

  pass_args <- list(
    .pass_tb = pass_tb,
    .selected_ctu = .selected_ctu,
    .scenario = .scenario,
    .parking_cost = .parking_cost,
    .vehicle_occupancy = .vehicle_occupancy,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .transit_avo_pct = .transit_avo_pct,
    .pldv_avo_pct = .pldv_avo_pct,
    .transit_service_pct = .transit_service_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .vmt_reduction_pct = .vmt_reduction_pct,
    .parking_price = .parking_price,
    .freight_parking_price = .freight_parking_price,
    .cong_price = .cong_price,
    .freight_vmt_fee = .freight_vmt_fee,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_diversity_pct_change = .land_use_diversity_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .intersection_density_pct_change = .intersection_density_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .cbtp_prop_targeted = .cbtp_prop_targeted,
    .cbtp_start_year = .cbtp_start_year,
    .enviro_factors = .enviro_factors,
    .elast = .elast,
    .elast_5d = .elast_5d,
    .factor_values = .factor_values,
    .calc_transp_cost = .calc_transp_cost,
    .calc_transp_fuel_use = .calc_transp_fuel_use,
    .calc_transp_ghg_embodied = .calc_transp_ghg_embodied,
    .fuel_economy = .fuel_economy
  )

  passenger_tables <-
    purrr::map(pass_funs, rlang::exec, !!!pass_args)

  names(passenger_tables) <- c(
    "passenger_light_duty",
    "bus_transit",
    # "rail_transit",
    "walk_bike",
    "school_bus"
  )

  list2env(passenger_tables, envir = environment())

  # Freight -------------------------------
  # (measured in ton-miles NOT miles)

  freight_funs <- c(
    mode_freight_truck
    # mode_freight_rail,
    # mode_air_water_multi
  )

  freight_args <- list(
    .freight_tb = freight_tb,
    .selected_ctu = .selected_ctu,
    .parking_cost = .parking_cost,
    .scenario = .scenario,
    .vehicle_occupancy = .vehicle_occupancy,
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
    .enviro_factors = .enviro_factors,
    .elast = .elast,
    .elast_5d = .elast_5d,
    .factor_values = .factor_values,
    .fuel_economy = .fuel_economy
  )


  freight_tables <-
    purrr::map(freight_funs, rlang::exec, !!!freight_args)

  names(freight_tables) <- c(
    "freight_truck"
    # "freight_rail",
    # "freight_multi_air_wat"
  )

  list2env(freight_tables, envir = environment())

  # Finish up -----
  ## passenger ------

  passenger <- list(
    PLDV = passenger_light_duty,
    # RAIL = rail_transit,
    BU_BRT = bus_transit,
    WALK_BIKE = walk_bike,
    BS = school_bus
  )


  pass_vmt <- dplyr::bind_rows(
    passenger_light_duty$vmt,
    # rail_transit$vmt,
    bus_transit$vmt,
    walk_bike$vmt,
    school_bus$vmt
  )

  pass_dir_ghg <- dplyr::bind_rows(
    passenger_light_duty$dir_ghg,
    # rail_transit$dir_ghg,
    bus_transit$dir_ghg,
    walk_bike$dir_ghg,
    school_bus$dir_ghg
  )

  pass_all <- dplyr::left_join(
    pass_vmt, pass_dir_ghg,
    c(
      "type",
      "scenario",
      "geog_name",
      "geog_id",
      "year",
      "mode",
      "aeo_mode",
      "class"
    )
  ) %>%
    dplyr::left_join(
      ghg.gert::geog_index %>%
        dplyr::select(-tidyr::any_of(c("ctu", "ctu_name"))),
      by = c("geog_name", "geog_id")
    )


  if (.calc_transp_ghg_embodied == TRUE) {
    pass_emb_ghg <- dplyr::bind_rows(
      passenger_light_duty$emb_ghg,
      # rail_transit$emb_ghg,
      bus_transit$emb_ghg,
      # walk_bike$emb_ghg,
      school_bus$emb_ghg
    ) %>%
      mutate(scenario = .scenario)

    pass_all <- pass_all %>%
      dplyr::left_join(pass_emb_ghg, c(
        "type",
        "scenario",
        "geog_name",
        "geog_id",
        "year",
        "mode",
        "aeo_mode",
        "class"
      ))
  }

  if (.calc_transp_fuel_use == TRUE) {
    pass_fuel <- dplyr::bind_rows(
      passenger_light_duty$fuel_use_gallons_kwh,
      bus_transit$fuel_use_gallons_kwh,
      # rail_transit$fuel_use_gallons_kwh,
      # walk_bike$fuel_use_gallons_kwh,
      school_bus$fuel_use_gallons_kwh
    )

    pass_all <- pass_all %>%
      dplyr::left_join(pass_fuel, by = c(
        "type",
        "scenario",
        "geog_name",
        "geog_id",
        "year",
        "mode",
        "aeo_mode",
        "class"
      ))
  }

  if (.calc_transp_cost == TRUE) {
    pass_cost <- dplyr::bind_rows(
      passenger_light_duty$cost,
      bus_transit$cost,
      # rail_transit$cost,
      school_bus$cost,
      # walk_bike$cost
    )

    pass_all <- pass_all %>%
      dplyr::left_join(pass_cost, by = c(
        "type",
        "scenario",
        "geog_name",
        "geog_id",
        "year",
        "mode",
        "aeo_mode",
        "class"
      ))
  }


  pass_all <- pass_all %>%
    dplyr::distinct()
  # %>%
  #   dplyr::select(
  #     type,
  #     scenario,
  #     geog_name,
  #     year,
  #     mode,
  #     aeo_mode,
  #     stock,
  #     class,
  #     vmt,
  #     dir_ghg,
  #     ghg_embodied_source,
  #     ghg_embodied,
  #     fuel_use_gallons_kwh
  #   )


  ## freight -----
  freight_vmt <- dplyr::bind_rows(
    # freight_multi_air_wat$vmt,
    # freight_rail$vmt,
    freight_truck$vmt
  )

  freight_ghg <- dplyr::bind_rows(
    # freight_multi_air_wat$dir_ghg,
    freight_truck$dir_ghg
    # freight_rail$dir_ghg
  )

  freight_all <- dplyr::left_join(freight_vmt, freight_ghg,
    by = c(
      "type", "scenario", "geog_name", "geog_id",
      "year", "mode", "aeo_mode", "class"
    )
  ) %>%
    dplyr::left_join(
      ghg.gert::geog_index %>%
        dplyr::select(-tidyr::any_of(c("ctu", "ctu_name"))),
      by = c("geog_name", "geog_id")
    )

  freight <- list(
    # AIR_WAT_MM = freight_multi_air_wat,
    # FRAIL = freight_rail,
    SUT_CUT = freight_truck
  )

  return(
    list(
      "passenger" = passenger,
      "passenger_all" = pass_all,
      "freight" = freight,
      "freight_all" = freight_all,
      "pass_tb" = pass_tb,
      "freight_tb" = freight_tb
    )
  )
}
