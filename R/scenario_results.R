#' @title  Main function to call other functions for determining VMT,
#'      direct GHG, indirect GHG, and costs
#'
#' @param .electric_scenario electricity scenario
#' @param .aeo_scenario selected EIA Annual Energy Outlook scenario
#' @param .ctu chosen CTU
#' @param .drs_fuel_type input dynamic ride sharing (DRS) fuel type. Default is `0`.
#' @param .av_fuel_type input AV fuel type
#' @param .mit_bau_summary input of BAU data for calculations in MIT scenario
#' @inheritParams calc_vmt_forecast
#'
#' @return
#' @export
#' @importFrom dplyr filter select case_when across bind_rows cur_column
#' @importFrom tidyselect all_of
#' @importFrom tibble tibble
#' @family transportation
scenario_results <- function(.scenario = "BAU",
                             .electric_scenario = "ER",
                             .aeo_scenario = "REF",
                             .ctu = "",
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
                             .land_use_pct_change = 0,
                             .intersection_design_pct_change = 0,
                             .job_access_pct_change = 0,
                             .transit_dist_pct_change = 0,
                             .comb_5d_impact_pct_change = 0,
                             .telework_pct = 0,
                             .mit_bau_summary = 0) {
  # If the user has specified DRS, then reduce the PMT for non-DRS trips
  # browser()
  if (.drs_pct > 0) {
    browser()
    pass_transpo <- pass_transpo %>%
      dplyr::mutate(
        dplyr::across(
          all_of(YRS), ~ dplyr::case_when(
            ((mode == "PLDV") &
              var == "PMT") ~ .x *
              dplyr::case_when(
                .drs_pct > 0 ~ (1 - pass_transpo %>%
                  dplyr::filter(var == "DRSShare") %>%
                  dplyr::select(dplyr::cur_column()) %>%
                  as.numeric() * .drs_pct / 100),
                TRUE ~ 1
              ),
            TRUE ~ .x
          )
        )
      )
  }

  browser()
  # Sequence for each
  # 1. Establish `type`, `var`, `mode`
  # 2. Establish `stock`, `mpg`, `class`
  # 3. Calculate fuel cost per mile with `calc_fuel_cost_mile()`
  # 4. Calculate VMT with `calc`

  type <- "P"
  # For all passenger modes, variable = PMT
  var <- "PMT"
  mode <- "PLDV"

  # passenger light-duty -----
  passenger_light_duty <- calc_passenger_light_duty(
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .ctu = .ctu,
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
    .land_use_pct_change = .land_use_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary
  )

  # browser()
  # Caculate a fuel cost for use in DRS and transit calculations. Use gasoline PLDV value.
  mpg <- "SIMPG"
  # Calculate a fuel cost per mile rather than per gallon

  fcm <- calc_fuel_cost_mile(
    transportation_data$passenger, mode,
    .aeo_scenario, mpg, SI_FUEL_COST_GAL
  )

  # transit buses -----
  bus_transit <- calc_bus_transit(
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .ctu = .ctu,
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
    .land_use_pct_change = .land_use_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary
  )

  # transit rail -----

  rail_transit <- calc_rail_transit(
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .ctu = .ctu,
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
    .land_use_pct_change = .land_use_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary
  )

  # school bus-----
  calc_school_bus(
    .scenario = .scenario,
    .electric_scenario = .electric_scenario,
    .aeo_scenario = .aeo_scenario,
    .ctu = .ctu,
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
    .land_use_pct_change = .land_use_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct,
    .mit_bau_summary = .mit_bau_summary
  )


  # Active Modes-----
  ## Walk -----
  mode <- "WALK"
  stock <- ""
  class <- "WALK"

  walk_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$passenger, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct,
      .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>%
    mutate(class = class)

  ## Bike -----
  mode <- "BIKE"
  stock <- ""
  class <- "BIKE"

  bike_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$passenger, mode,
      stock, var, fcm, .aeo_scenario,
      .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee,
      .cong_price, .parking_price, .drs_pct,
      .av_pct, .freight_vmt_fee,
      .pop_dens_pct_change, .emp_dens_pct_change, .land_use_pct_change,
      .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>%
    mutate(class = class)


  # Add the ACTIVE data
  # out_sum <- dplyr::bind_rows(out_sum, walk_vmt, bike_vmt)
  browser()

  # Dynamic Ride Sharing -----
  mode <- "DRS"

  # Use PLDV features in some cases
  mode_1 <- "PLDV"

  # If DRS is included,
  # then perform calculations depending if fuel is BEV, HEV, or PHEV

  if (.scenario != "BAU" & .drs_pct > 0) {
    # Calculate DRS sales in each year
    drs_sales <-
      calc_drs_sales(transportation_data$passenger, .drs_pct)

    transportation_data$passenger <- dplyr::bind_rows(transportation_data$passenger, drs_sales)

    if (.drs_fuel_type == "HEV") {
      ## DRS Hybrid fuel -----
      stock <- "DRSStock"
      mpg <- "HEVMPG"
      class <- "HEV"
      fcm <- calc_fuel_cost_mile(
        transportation_data$passenger, mode,
        .aeo_scenario, mpg, SI_FUEL_COST_GAL
      )

      drs_vmt <-
        calc_drs_vmt(
          transportation_data$passenger, .drs_pct,
          class, fcm, .vmt_fee,
          .payd_fee, .gas_tax, .cong_price,
          .parking_price, .pop_dens_pct_change,
          .emp_dens_pct_change, .land_use_pct_change,
          .intersection_design_pct_change, .job_access_pct_change,
          .transit_dist_pct_change, .comb_5d_impact_pct_change
        ) %>%
        mutate(class = class)



      drs_dir_ghg <-
        calc_ghg_direct(
          drs_vmt,
          transportation_data$passenger,
          mode_1, "SI",
          .aeo_scenario, mpg, 1
        )
      .drs_fuel_type <-
        calc_fuel_use(
          drs_vmt,
          transportation_data$passenger,
          mode_1, "SI",
          .aeo_scenario, mpg, 1
        )

      temp <- calc_ghg_embodied(
        transportation_data$passenger,
        mode, "DRSSales",
        "HEV-EMB"
      )

      # out_sum <- out_sum %>%
      #   dplyr::mutate(
      #     dplyr::across(all_of(YRS), ~ dplyr::case_when(
      #       (mode == mode_1 &
      #          class == class &
      #          output == "INDIR-GHG") ~ .x + temp %>% as.numeric(),
      #       TRUE ~ .x
      #     ))
      #   )

      drs_cost <-
        calc_cost(
          drs_vmt,
          mode_1, "HEVPrice", 1
        )


      # Add the DRS data
      # out_sum <- dplyr::bind_rows(
      #   out_sum,
      #   drs_vmt, drs_dir_ghg,
      #   .drs_fuel_type, drs_cost
      # )
    } else if (.drs_fuel_type == "PHEV") {

      ## DRS Plug-in hybrid -----
      stock <- "DRSStock"
      mpg <- "PHEVMPG"
      mpe <- "PHEVElec"
      class <- "PHEV"

      # Don't apply the .gas_tax factors, etc. to PHEV for DRS
      phev_vmtg <- calc_drs_vmt(
        transportation_data$passenger, .drs_pct, class, fcm, .vmt_fee,
        .payd_fee, .gas_tax, .cong_price, .parking_price, .pop_dens_pct_change,
        .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change,
        .job_access_pct_change, .transit_dist_pct_change,
        .comb_5d_impact_pct_change
      ) * (
        1 - transportation_data$passenger %>%
          dplyr::filter(mode == mode, var == "PHEVPr") %>%
          dplyr::select(all_of(YRS)))

      phev_vmte <- calc_drs_vmt(
        transportation_data$passenger, .drs_pct, class,
        fcm, .vmt_fee, .payd_fee, .gas_tax, .cong_price,
        .parking_price, .pop_dens_pct_change, .emp_dens_pct_change,
        .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
        .transit_dist_pct_change, .comb_5d_impact_pct_change
      ) *
        transportation_data$passenger %>%
          dplyr::filter(mode == mode, var == "PHEVPr") %>%
          dplyr::select(all_of(YRS))

      drs_vmt <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "VMT",
        phev_vmtg + phev_vmte
      )

      phev_ghgg <- calc_ghg_direct(
        phev_vmtg, transportation_data$passenger,
        mode, "SI", .aeo_scenario, mpg, 1
      )

      phev_ghge <- calc_ghg_direct(
        phev_vmte, transportation_data$passenger,
        mode, .electric_scenario, .aeo_scenario, mpe, 1
      )

      drs_dir_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "DIR-GHG",
        phev_ghgg + phev_ghge
      )

      drs_fuelg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "PETRO",
        calc_fuel_use(
          phev_vmtg,
          transportation_data$passenger, mode, "SI",
          .aeo_scenario, mpg, 1
        )
      )

      drs_fuele <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "ELEC",
        calc_fuel_use(
          phev_vmte, transportation_data$passenger,
          mode, .electric_scenario, .aeo_scenario, mpe, 1
        )
      )

      temp <- calc_ghg_embodied(
        transportation_data$passenger, mode,
        "DRSSales", "PHEV-EMB"
      ) %>% as.numeric()

      # out_sum <- out_sum %>%
      #   dplyr::mutate(
      #     dplyr::across(all_of(YRS), ~ dplyr::case_when(
      #       (mode == mode_1 &
      #          class == class &
      #          output == "INDIR-GHG") ~ .x + temp %>% as.numeric(),
      #       TRUE ~ .x
      #     ))
      #   )

      drs_cost <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "COST",
        calc_cost(
          drs_vmt,
          mode_1, "PHEVPrice", 1
        )
      )

      # Add the DRS data
      # out_sum <- dplyr::bind_rows(
      #   out_sum, drs_vmt, drs_dir_ghg, drs_fuelg,
      #   drs_fuele, drs_cost
      # )
    } else {
      ## DRS Battery Electric -----
      stock <- "DRSStock"
      mpe <- "BEVElec"
      class <- "BEV"
      drs_vmt <-
        calc_drs_vmt(
          transportation_data$passenger, .drs_pct,
          class, fcm, .vmt_fee, .payd_fee,
          .gas_tax, .cong_price, .parking_price,
          .pop_dens_pct_change, .emp_dens_pct_change,
          .land_use_pct_change, .intersection_design_pct_change,
          .job_access_pct_change, .transit_dist_pct_change,
          .comb_5d_impact_pct_change
        ) %>%
        mutate(class = class)

      drs_dir_ghg <-
        calc_ghg_direct(
          drs_vmt,
          transportation_data$passenger,
          mode_1, .electric_scenario,
          .aeo_scenario, mpe, 1
        )

      .drs_fuel_type <-
        calc_fuel_use(
          drs_vmt,
          transportation_data$passenger, mode_1, .electric_scenario,
          .aeo_scenario, mpe, 1
        )


      temp <- calc_ghg_embodied(
        transportation_data$passenger,
        mode, "DRSSales", "BEV-EMB"
      ) %>%
        as.numeric()

      # out_sum <- out_sum %>%
      #   dplyr::mutate(dplyr::across(all_of(YRS), ~ dplyr::case_when(
      #     (mode == mode_1 &
      #        class == class &
      #        output == "INDIR-GHG") ~ .x + temp %>% as.numeric(),
      #     TRUE ~ .x
      #   )))

      drs_cost <- tibble::tibble(
        type = type,
        scenario = .scenario, mode = mode,
        class = class, ctu = .ctu,
        output = "COST",
        calc_cost(
          drs_vmt,
          mode_1, "BEVPrice"
        )
      )

      # Add the DRS data
      # out_sum <- dplyr::bind_rows(
      #   out_sum,
      #   drs_vmt, drs_dir_ghg,
      #   .drs_fuel_type, drs_cost
      # )
    }
  }

  # AV-----
  mode <- "AV"
  # Use PLDV features in some cases
  mode_1 <- "PLDV"

  # If AV is included, then perform calculations depending
  # if fuel type is BEV, HEV, or PHEV
  if (.scenario != "BAU" & .av_pct > 0) {
    # Calculate DRS sales in each year
    av_sales <- tibble::tibble(
      mode = mode,
      var = "AVSales", ctu = .ctu,
      calc_av_sales(
        transportation_data$passenger,
        .av_pct
      )
    )

    transportation_data$passenger <- dplyr::bind_rows(transportation_data$passenger, av_sales)


    if (.av_fuel_type == "HEV") {
      ## AV Hybrid electric ----
      stock <- "AVStock"
      mpg <- "HEVMPG"
      class <- "HEV"

      # Calculate a fuel cost per mile rather than per gallon
      fcm <- calc_fuel_cost_mile(
        transportation_data$passenger,
        mode_1, .aeo_scenario, mpg, SI_FUEL_COST_GAL, .av_pct
      )
      av_vmt <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class, ctu = .ctu,
        output = "VMT",
        calc_vmt_forecast(
          .scenario, transportation_data$passenger,
          mode, stock, var,
          fcm, .aeo_scenario,
          .transit_avo, .transit_rider_pct,
          .vmt_fee, .payd_fee, .gas_tax,
          .cong_price, .parking_price, .drs_pct,
          .av_pct, .freight_vmt_fee,
          .pop_dens_pct_change, .emp_dens_pct_change,
          .land_use_pct_change, .intersection_design_pct_change,
          .job_access_pct_change, .transit_dist_pct_change,
          .comb_5d_impact_pct_change,
          .telework_pct
        )
      )


      av_dir_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "DIR-GHG",
        calc_ghg_direct(
          av_vmt,
          transportation_data$passenger,
          mode_1, "SI", .aeo_scenario,
          mpg, .av_pct
        )
      )


      .av_fuel_type <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "PETRO",
        calc_fuel_use(
          av_vmt, transportation_data$passenger,
          mode_1, "SI", .aeo_scenario,
          mpg, .av_pct
        )
      )

      av_emb_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "INDIR-GHG",
        calc_ghg_embodied(
          transportation_data$passenger,
          mode, "AVSales",
          "HEV-EMB"
        )
      )

      av_cost <- tibble::tibble(
        type = type, scenario = .scenario, mode = mode,
        class = class, ctu = .ctu, output = "COST",
        calc_cost(
          av_vmt,
          mode_1,
          "HEVPrice", 1
        )
      )

      # Add the AV data
      # out_sum <- dplyr::bind_rows(
      #   out_sum, av_vmt, av_dir_ghg, .av_fuel_type,
      #   av_emb_ghg, av_cost
      # )
    } else if (.av_fuel_type == "PHEV") {
      ## AV Plug-in hygbrid -----
      stock <- "AVStock"
      mpg <- "PHEVMPG"
      mpe <- "PHEVElec"
      class <- "PHEV"
      # Calculate a fuel cost per mile rather than per gallon
      fcm <- calc_fuel_cost_mile(
        transportation_data$passenger, mode_1, .aeo_scenario,
        mpg, SI_FUEL_COST_GAL, .av_pct
      )

      phev_vmtg <- calc_vmt_forecast(
        .scenario, transportation_data$passenger, mode, stock,
        var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
        .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
        .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change, .land_use_pct_change,
        .intersection_design_pct_change, .job_access_pct_change, .transit_dist_pct_change,
        .comb_5d_impact_pct_change, .telework_pct, 1
      ) * (1 - transportation_data$passenger %>%
        dplyr::filter(mode == mode, var == "PHEVPr") %>%
        dplyr::select(all_of(YRS)))

      fcm <- calc_fuel_cost_mile(
        transportation_data$passenger, mode,
        .aeo_scenario, mpe, ELEC_FUEL_COST_KWH
      )

      phev_vmte <- calc_vmt_forecast(
        .scenario, transportation_data$passenger, mode,
        stock, var, fcm, .aeo_scenario,
        .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee,
        .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
        .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
        .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
        .transit_dist_pct_change, .comb_5d_impact_pct_change,
        .telework_pct
      ) * transportation_data$passenger %>%
        dplyr::filter(mode == mode, var == "PHEVPr") %>%
        dplyr::select(all_of(YRS))

      av_vmt <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "VMT",
        phev_vmtg + phev_vmte
      )

      phev_ghgg <- calc_ghg_direct(
        phev_vmtg, transportation_data$passenger,
        mode, "SI", .aeo_scenario, mpg, .av_pct
      )

      phev_ghge <- calc_ghg_direct(
        phev_vmtg, transportation_data$passenger,
        mode, .electric_scenario, .aeo_scenario, mpe, .av_pct
      )

      av_dir_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "DIR-GHG",
        phev_ghgg + phev_ghge
      )

      av_fuelg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "PETRO",
        calc_fuel_use(
          phev_vmtg,
          transportation_data$passenger, mode, "SI",
          .aeo_scenario, mpg, .av_pct
        )
      )

      av_fuele <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "ELEC",
        calc_fuel_use(
          phev_vmte,
          transportation_data$passenger,
          mode, .electric_scenario, .aeo_scenario,
          mpe, .av_pct
        )
      )

      av_emb_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "INDIR-GHG",
        calc_ghg_embodied(
          transportation_data$passenger, mode,
          "AVSales", "PHEV-EMB"
        )
      )

      av_cost <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "COST",
        calc_cost(
          av_vmt,
          mode_1,
          "PHEVPrice", 1
        )
      )
      # Add the AV data
      # out_sum <- dplyr::bind_rows(
      #   out_sum,
      #   av_vmt, av_dir_ghg, av_fuelg, av_fuele,
      #   av_emb_ghg, av_cost
      # )
    } else {
      ## AV Battery electric -----
      stock <- "AVStock"
      mpe <- "BEVElec"
      class <- "BEV"
      fcm <- calc_fuel_cost_mile(
        transportation_data$passenger,
        mode_1, .aeo_scenario, mpe, ELEC_FUEL_COST_KWH
      )

      av_vmt <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "VMT",
        calc_vmt_forecast(
          .scenario, transportation_data$passenger,
          mode, stock, var, fcm,
          .aeo_scenario, .transit_avo, .transit_rider_pct,
          .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price,
          .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change,
          .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change,
          .job_access_pct_change, .transit_dist_pct_change,
          .comb_5d_impact_pct_change, .telework_pct
        )
      )

      av_dir_ghg <- tibble::tibble(
        type = type, scenario = .scenario, mode = mode,
        class = class, ctu = .ctu,
        output = "DIR-GHG",
        calc_ghg_direct(
          av_vmt,
          transportation_data$passenger,
          mode_1, .electric_scenario,
          .aeo_scenario, mpe, .av_pct
        )
      )

      .av_fuel_type <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "ELEC",
        calc_fuel_use(
          av_vmt, transportation_data$passenger,
          mode_1, .electric_scenario, .aeo_scenario, mpe, .av_pct
        )
      )

      av_emb_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "INDIR-GHG",
        calc_ghg_embodied(
          transportation_data$passenger,
          mode,
          "AVSales", "BEV-EMB"
        )
      )

      av_cost <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "COST",
        calc_cost(
          av_vmt,
          mode_1, "BEVPrice", 1
        )
      )

      # Add the AV data
      # out_sum <- dplyr::bind_rows(
      #   out_sum, av_vmt,
      #   av_dir_ghg, .av_fuel_type,
      #   av_emb_ghg, av_cost
      # )
    }
  }

  # Freight -------------------------------
  # (measured in ton-miles NOT miles)
  type <- "F"
  # For all freight, var = TMT
  var <- "TMT"



  ## Heavy truck (CUT) ----
  mode <- "CUT"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(
    transportation_data$freight, mode,
    .aeo_scenario, mpg, CI_FUEL_COST_GAL
  )

  ci_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$freight,
      mode, stock, var, fcm,
      .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price,
      .parking_price, .drs_pct, .av_pct, .freight_vmt_fee,
      .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change
    ) %>%
    mutate(class = class)


  ci_ghg <-
    calc_ghg_direct(
      ci_vmt, transportation_data$freight,
      mode, "CUTCI", .aeo_scenario, mpg
    )


  ### Heavy  battery electric -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"
  bev_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$freight, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>%
    mutate(class = class)

  bev_ghg <-
    calc_ghg_direct(
      bev_vmt,
      transportation_data$freight, mode,
      .electric_scenario, .aeo_scenario, mpe
    )


  # Add the CUT data
  # out_sum <- dplyr::bind_rows(
  #   out_sum, ci_vmt, ci_ghg,
  #   bev_vmt, bev_ghg
  # )

  ## Medium truck (SUT) -----
  mode <- "SUT"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"

  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(
    transportation_data$freight, mode,
    .aeo_scenario, mpg, CI_FUEL_COST_GAL
  )

  ci_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$freight, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct,
      .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>% mutate(class = class)

  ci_ghg <-
    calc_ghg_direct(
      ci_vmt,
      transportation_data$freight, mode,
      "SUTCI", .aeo_scenario, mpg
    )


  ### Medium truck battery electric -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"

  bev_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$freight, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo,
      .transit_rider_pct, .vmt_fee, .payd_fee, .gas_tax,
      .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>%
    mutate(class = class)



  bev_ghg <-
    calc_ghg_direct(
      bev_vmt, transportation_data$freight,
      mode, .electric_scenario, .aeo_scenario, mpe
    )

  # Add the SUT data
  # out_sum <- dplyr::bind_rows(
  #   out_sum, ci_vmt,
  #   ci_ghg, bev_vmt, bev_ghg
  # )

  ## Freight Rail -----
  mode <- "FR"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"

  ci_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$freight,
      mode, stock, var, fcm,
      .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price,
      .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change,
      .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>% mutate(class = class)

  ci_ghg <-
    calc_ghg_direct(
      ci_vmt,
      transportation_data$freight,
      mode, "RCI", .aeo_scenario, mpg
    )


  ### Freight rail battery electric ------
  stock <- "EVStock"
  mpe <- "EVElec"
  class <- "EV"

  ev_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$freight,
      mode, stock, var, fcm,
      .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price,
      .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change,
      .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change
    ) %>% mutate(class = class)


  ev_ghg <-
    calc_ghg_direct(
      bev_vmt,
      transportation_data$freight, mode,
      .electric_scenario, .aeo_scenario, mpe
    )

  # Add the FR data
  # out_sum <- dplyr::bind_rows(
  #   out_sum, ci_vmt, ci_ghg,
  #   ev_vmt, ev_ghg
  # )

  ## Multimodal -----
  mode <- "MM"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"

  ci_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$freight, mode,
      stock, var, fcm, .aeo_scenario,
      .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee,
      .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>% mutate(class = class)

  ci_ghg <-
    calc_ghg_direct(
      ci_vmt,
      transportation_data$freight,
      mode, "MMCI", .aeo_scenario, mpg
    )



  ### Multimodal battery electric-----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"

  bev_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$freight, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct,
      .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>% mutate(class = class)

  bev_ghg <-
    calc_ghg_direct(
      bev_vmt,
      transportation_data$freight, mode,
      .electric_scenario, .aeo_scenario, mpe
    )


  # Add the MM data
  # out_sum <- dplyr::bind_rows(out_sum, ci_vmt, ci_ghg, bev_vmt, bev_ghg)

  ## Air------
  mode <- "AIR"

  ## SI
  stock <- "SIStock"
  mpg <- "SIMPG"
  class <- "SI"

  si_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$freight, mode,
      stock, var, fcm, .aeo_scenario, .transit_avo,
      .transit_rider_pct, .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price,
      .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    ) %>% mutate(class = class)

  si_ghg <-
    calc_ghg_direct(
      si_vmt, transportation_data$freight, mode,
      "ASI",
      .aeo_scenario, mpg
    )


  # Add the AIR data
  # out_sum <- dplyr::bind_rows(out_sum, si_vmt, si_ghg)

  ## Water ------
  mode <- "WAT"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"

  ci_vmt <-
    calc_vmt_forecast(
      .scenario, transportation_data$freight, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change, .land_use_pct_change,
      .intersection_design_pct_change, .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change
    ) %>% mutate(class = class)

  ci_ghg <-
    calc_ghg_direct(
      ci_vmt, transportation_data$freight,
      mode, "WCI", .aeo_scenario, mpg
    )

  # Add the WAT data
  # out_sum <- dplyr::bind_rows(out_sum, ci_vmt, ci_ghg)

  # Return final ------
  # return(out_sum)
}
