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
scen_autonomous_vehicle <- function(.pass_tb = transportation_data$passenger,
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
                                    .land_use_pct_change = 0,
                                    .intersection_design_pct_change = 0,
                                    .job_access_pct_change = 0,
                                    .transit_dist_pct_change = 0,
                                    .comb_5d_impact_pct_change = 0,
                                    .telework_pct = 0,
                                    .mit_bau_summary = 0,
                                    .enviro_factors = enviro_factors) {
  fcm <- calc_fuel_cost_mile(
    .pass_tb,
    .mode = "PLDV",
    .aeo_scenario,
    .miles_per_gallon = "SIMPG",
    .enviro_factors$SI_FUEL_COST_GAL
  )

  type <- "P"
  # For all passenger modes, variable = PMT
  var <- "PMT"

  # browser()

  # AV-----
  mode <- "AV"
  # Use PLDV features in some cases
  mode_1 <- "PLDV"
  stock <- "AVStock"

  # If AV is included, then perform calculations depending
  # if fuel type is BEV, HEV, or PHEV
  if (.scenario != "BAU" & .av_pct > 0) {
    # Calculate AV sales in each year
    av_sales <- calc_av_sales(
      .pass_tb,
      .av_pct = .av_pct
    )

    av_passenger_tb <-
      dplyr::bind_rows(.pass_tb, av_sales)


    if (.av_fuel_type == "HEV") {
      message("Autonomous vehicles, hybrid")

      ## AV Hybrid electric ----
      stock <- "AVStock"
      mpg <- "HEVMPG"
      class <- "HEV"

      # Calculate a fuel cost per mile rather than per gallon
      fcm <- calc_fuel_cost_mile(
        tb =   av_passenger_tb,
        mode_1, .aeo_scenario, mpg, .enviro_factors$SI_FUEL_COST_GAL, .av_pct
      )

      hev_av_vmt <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        output = "VMT",
        calc_vmt_forecast(
          .scenario, av_passenger_tb,
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
        output = "DIR-GHG",
        calc_ghg_direct(
          av_vmt,
          av_passenger_tb,
          mode_1, "SI", .aeo_scenario,
          mpg, .av_pct
        )
      )


      hev_av_fuel <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        output = "PETRO",
        calc_fuel_use(
          av_vmt, av_passenger_tb,
          mode_1, "SI", .aeo_scenario,
          mpg, .av_pct
        )
      )

      av_emb_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        output = "INDIR-GHG",
        calc_ghg_embodied(
          av_passenger_tb,
          mode,
          .class = class,
          "AVSales",
          "HEV-EMB"
        )
      )

      av_cost <- tibble::tibble(
        type = type, scenario = .scenario, mode = mode,
        class = class, output = "COST",
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
        av_passenger_tb, mode_1, .aeo_scenario,
        mpg, .enviro_factors$SI_FUEL_COST_GAL, .av_pct
      )

      phev_vmtg <- calc_vmt_forecast(
        .scenario, av_passenger_tb, mode, stock,
        var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
        .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
        .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change, .land_use_pct_change,
        .intersection_design_pct_change, .job_access_pct_change, .transit_dist_pct_change,
        .comb_5d_impact_pct_change, .telework_pct, 1
      ) * (1 - av_passenger_tb %>%
        dplyr::filter(mode == mode, var == "PHEVPr") %>%
        dplyr::select(all_of(YRS)))

      fcm <- calc_fuel_cost_mile(
        av_passenger_tb, mode,
        .aeo_scenario, mpe, .enviro_factors$ELEC_FUEL_COST_KWH
      )

      phev_vmte <- calc_vmt_forecast(
        .scenario, av_passenger_tb, mode,
        stock, var, fcm, .aeo_scenario,
        .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee,
        .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
        .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
        .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
        .transit_dist_pct_change, .comb_5d_impact_pct_change,
        .telework_pct
      ) * av_passenger_tb %>%
        dplyr::filter(mode == mode, var == "PHEVPr") %>%
        dplyr::select(all_of(YRS))

      av_vmt <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        output = "VMT",
        phev_vmtg + phev_vmte
      )

      phev_ghgg <- calc_ghg_direct(
        phev_vmtg, av_passenger_tb,
        mode, "SI", .aeo_scenario, mpg, .av_pct
      )

      phev_ghge <- calc_ghg_direct(
        phev_vmtg, av_passenger_tb,
        mode, .electric_scenario, .aeo_scenario, mpe, .av_pct
      )

      av_dir_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        output = "DIR-GHG",
        phev_ghgg + phev_ghge
      )

      av_fuelg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        output = "PETRO",
        calc_fuel_use(
          phev_vmtg,
          av_passenger_tb, mode, "SI",
          .aeo_scenario, mpg, .av_pct
        )
      )

      av_fuele <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        output = "ELEC",
        calc_fuel_use(
          phev_vmte,
          av_passenger_tb,
          mode, .electric_scenario, .aeo_scenario,
          mpe, .av_pct
        )
      )

      av_emb_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        output = "INDIR-GHG",
        calc_ghg_embodied(
          av_passenger_tb, mode,
          .class = class,
          "AVSales", "PHEV-EMB"
        )
      )

      av_cost <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        output = "COST",
        calc_cost(
          av_vmt,
          mode_1,
          "PHEVPrice", 1
        )
      )
    } else if (.av_fuel_type == "BEV") {
      ## AV Battery electric -----
      stock <- "AVStock"
      mpe <- "BEVElec"
      class <- "BEV"

      message("Autonomous vehicles, battery electric")

      fcm <- calc_fuel_cost_mile(
        tb =   av_passenger_tb,
        mode_1, .aeo_scenario, mpe,
        .enviro_factors$ELEC_FUEL_COST_KWH
      )

      av_vmt <-
        calc_vmt_forecast(
          tb = av_passenger_tb,
          .scenario = .scenario,
          .mode = mode,
          .stock = stock,
          .variable = var,
          .tb_fuel_cost_mile = fcm,
          .aeo_scenario = .aeo_scenario,
          .av_pct = .av_pct,
          .transit_avo, .transit_rider_pct,
          .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price,
          .drs_pct,
          .freight_vmt_fee, .pop_dens_pct_change,
          .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change,
          .job_access_pct_change, .transit_dist_pct_change,
          .comb_5d_impact_pct_change, .telework_pct
        ) %>%
        dplyr::mutate(class = class)

      av_dir_ghg <-
        calc_ghg_direct(
          tb_vmt = av_vmt,
          tb = av_passenger_tb,
          .mode = mode_1,
          .fuel_type = .electric_scenario,
          .aeo_scenario = .aeo_scenario,
          .miles_per_gallon = mpe,
          .is_av = TRUE
        )


      av_fuel <-
        calc_fuel_use(
          tb_vmt = av_vmt,
          tb = av_passenger_tb,
          .mode = mode_1,
          .fuel_type = .electric_scenario,
          .aeo_scenario = .aeo_scenario,
          .miles_per_gallon = mpe,
          .is_av = TRUE,
          .enviro_factors = .enviro_factors
        )


      av_emb_ghg <-
        calc_ghg_embodied(
          tb = av_passenger_tb,
          .mode = mode_1,
          .sales_mode = "AVSales",
          .fuel_type = "BEV-EMB",
          .class = class,
          .enviro_factors = .enviro_factors
        )

      av_cost <-
        calc_cost(
          tb_vmt = av_vmt,
          .mode = mode_1,
          .price = "BEVPrice",
          .is_av = TRUE,
          .enviro_factors = .enviro_factors
        )
    }


    # Finish up -----


    fuel_use_all <- av_fuel

    vmt_all <- av_vmt

    emb_ghg_all <- av_emb_ghg

    dir_ghg_all <- av_dir_ghg

    cost_all <- av_cost


    av_return <- list(
      "vmt" = vmt_all,
      "dir_ghg" = dir_ghg_all,
      "emb_ghg" = emb_ghg_all,
      "fuel_use" = fuel_use_all,
      "cost" = cost_all
    )

    usethis::ui_done(paste("Autonomous vehicles", emo::ji("robot")))

    return(av_return)
  } else {

    # return a basic shell with NA values
    vmt_all <- .pass_tb %>%
      dplyr::select(ctu, year) %>%
      unique() %>%
      dplyr::mutate(
        type = type,
        stock = stock,
        scenario = .scenario,
        mode = mode,
        aeo_mode = "LDV",
        vmt = NA,
        class = NA
      )

    fuel_use_all <- vmt_all %>%
      dplyr::select(scenario, mode, ctu, year, aeo_mode) %>%
      dplyr::mutate(fuel_use = NA)

    dir_ghg_all <- fuel_use_all %>%
      dplyr::select(scenario, mode, ctu, year, aeo_mode,
        dir_ghg = fuel_use
      )

    emb_ghg_all <- fuel_use_all %>%
      dplyr::select(scenario, mode, ctu, year, aeo_mode,
        ghg_embodied = fuel_use
      )

    cost_all <- fuel_use_all %>%
      dplyr::select(scenario, mode, ctu, year, aeo_mode,
        cost = fuel_use
      )



    av_return <- list(
      "vmt" = vmt_all,
      "dir_ghg" = dir_ghg_all,
      "emb_ghg" = emb_ghg_all,
      "fuel_use" = fuel_use_all,
      "cost" = cost_all
    )


    usethis::ui_done(paste("Autonomous vehicles", emo::ji("robot")))
    return(av_return)
  }
}
