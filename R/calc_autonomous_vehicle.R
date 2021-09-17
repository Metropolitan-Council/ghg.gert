#' Title
#'
#' Calculate scenario for autonomous vehicles (AVs)
#'
#' @inheritParams scenario_results
#' @inheritParams calc_vmt_forecast
#'
#' @family transportation results, passenger
#' @keywords passenger
#'
#' @return
#' @export
#'
#' @importFrom emo ji
calc_autonomous_vehicle <- function(.scenario = "BAU",
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
                                    .mit_bau_summary = 0,
                                    .enviro_factors = enviro_factors) {
  fcm <- calc_fuel_cost_mile(
    transportation_data$passenger,
    .mode = "PLDV",
    .aeo_scenario,
    .miles_per_gallon = "SIMPG",
    .enviro_factors$SI_FUEL_COST_GAL
  )

  type <- "P"
  # For all passenger modes, variable = PMT
  var <- "PMT"
  mode <- "PLDV"

  # browser()

  # AV-----
  mode <- "AV"
  # Use PLDV features in some cases
  mode_1 <- "PLDV"

  # If AV is included, then perform calculations depending
  # if fuel type is BEV, HEV, or PHEV
  if (.scenario != "BAU" & .av_pct > 0) {
    browser()
    # Calculate DRS sales in each year
    av_sales <- tibble::tibble(
      mode = mode,
      var = "AVSales", ctu = .ctu,
      calc_av_sales(
        transportation_data$passenger,
        .av_pct
      )
    )

    transportation_data$passenger <-
      dplyr::bind_rows(transportation_data$passenger, av_sales)


    if (.av_fuel_type == "HEV") {
      message("Autonomous vehicles, hybrid")

      ## AV Hybrid electric ----
      stock <- "AVStock"
      mpg <- "HEVMPG"
      class <- "HEV"

      # Calculate a fuel cost per mile rather than per gallon
      fcm <- calc_fuel_cost_mile(
        transportation_data$passenger,
        mode_1, .aeo_scenario, mpg, .enviro_factors$SI_FUEL_COST_GAL, .av_pct
      )

      hev_av_vmt <- tibble::tibble(
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


      hev_av_dir_ghg <- tibble::tibble(
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


      hev_av_fuel <- tibble::tibble(
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
    } else if (.av_fuel_type == "PHEV") {
      message("Autonomous vehicles, plug-in hybrid")

      ## AV Plug-in hygbrid -----
      stock <- "AVStock"
      mpg <- "PHEVMPG"
      mpe <- "PHEVElec"
      class <- "PHEV"
      # Calculate a fuel cost per mile rather than per gallon
      fcm <- calc_fuel_cost_mile(
        transportation_data$passenger, mode_1, .aeo_scenario,
        mpg, .enviro_factors$SI_FUEL_COST_GAL, .av_pct
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
        .aeo_scenario, mpe, .enviro_factors$ELEC_FUEL_COST_KWH
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
    } else {
      ## AV Battery electric -----
      stock <- "AVStock"
      mpe <- "BEVElec"
      class <- "BEV"

      message("Autonomous vehicles, battery electric")

      fcm <- calc_fuel_cost_mile(
        transportation_data$passenger,
        mode_1, .aeo_scenario, mpe, .enviro_factors$ELEC_FUEL_COST_KWH
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
    }


    # Finish up -----


    vmt_all <- dplyr::bind_rows(
      bike_vmt,
      walk_vmt
    )


    av_return <- list(
      "vmt" = vmt_all
    )

    usethis::ui_done(paste("Autonomous vehicles", emo::ji("robot")))

    return(av_return)
  } else {
    usethis::ui_done(paste("Autonomous vehicles", emo::ji("robot")))
    return()
  }
}
