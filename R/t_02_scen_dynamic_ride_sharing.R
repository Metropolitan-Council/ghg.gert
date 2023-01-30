#' @title Calculate dynamic ride sharing scenario
#'
#' @inheritParams run_scenario_transportation
#' @inheritParams calc_drs_sales
#' @inheritParams calc_vmt_forecast_drs
#' @return [tibble::tibble()] with column names...
#' @export
#'
#' @family transportation, dynamic ride share
#' @keywords passenger
#'
#'
#' @importFrom emo ji
#' @importFrom usethis ui_done
scen_dynamic_ride_sharing <- function(.scenario = "BAU",
                                      .selected_ctu = "all",
                                      .pass_tb = transportation_data$passenger,
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
                                      .mit_bau_summary = 0,
                                      .enviro_factors = enviro_factors,
                                      .elast = elast,
                                      .elast_5d = elast_5d) {

  cat("** calculating dynamic ride sharing scenario \n")
  .pass_tb <- filter_ctu(.pass_tb, .selected_ctu)

  type <- "P"
  # For all passenger modes, variable = PMT
  var <- "PMT"
  # Dynamic Ride Sharing -----
  mode <- "DRS"

  # Use PLDV features in some cases
  mode_1 <- "PLDV"

  # If DRS is included,
  # then perform calculations depending if fuel is BEV, HEV, or PHEV

  if (.scenario != "BAU" & .drs_pct > 0) {
    # browser()
    # Calculate DRS sales in each year
    drs_sales <-
      calc_drs_sales(
        tb = .pass_tb,
        .drs_pct = .drs_pct
      )

    .pass_tb <- .pass_tb %>%
      filter(var != "DRSSales") %>%
      dplyr::bind_rows(.pass_tb, drs_sales)



    if (.drs_fuel_type == "HEV") {
      ## DRS Hybrid fuel -----
      stock <- "DRSStock"
      mpg <- "HEVMPG"
      class <- "HEV"


      fcm <- calc_fuel_cost_mile(
        .pass_tb,
        .mode = mode_1,
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = mpg,
        .fuel_cost_gallon = .enviro_factors$SI_FUEL_COST_GAL,
        .enviro_factors = .enviro_factors
      )


      drs_vmt <-
        calc_vmt_forecast_drs(
          .scenario = .scenario,
          tb = .pass_tb,
          .mode = mode,
          .stock = stock,
          .variable = var,
          .tb_fuel_cost_mile = fcm,
          .aeo_scenario = .aeo_scenario,
          .transit_avo_pct = .transit_avo_pct,
          .pldv_avo_pct = .pldv_avo_pct,
          .transit_service_pct = .transit_service_pct,
          .vmt_fee = .vmt_fee,
          .payd_fee = .payd_fee,
          .gas_tax = .gas_tax,
          .cong_price = .cong_price,
          .parking_price = .parking_price,
          .freight_parking_price = .freight_parking_price,
          .drs_pct = .drs_pct,
          .drs_fuel_type = .drs_fuel_type,
          .av_pct = .av_pct,
          .freight_vmt_fee = .freight_vmt_fee,
          .pop_dens_pct_change = .pop_dens_pct_change,
          .emp_dens_pct_change = .emp_dens_pct_change,
          .land_use_diversity_pct_change = .land_use_diversity_pct_change,
          .intersection_design_pct_change = .intersection_design_pct_change,
          .job_access_pct_change = .job_access_pct_change,
          .transit_dist_pct_change = .transit_dist_pct_change,
          .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
          .telework_pct = .telework_pct,
          .phev_electric = .phev_electric,
          .enviro_factors = enviro_factors
        ) %>%
        mutate(class = class)

      drs_dir_ghg <-
        calc_ghg_direct(
          tb_vmt = drs_vmt,
          tb = .pass_tb,
          .mode = mode_1,
          .fuel_type = "SI",
          .aeo_scenario = .aeo_scenario,
          .miles_per_gallon = mpg,
          .is_av = TRUE,
          .enviro_factors = .enviro_factors
        )

      drs_fuel <-
        calc_fuel_use(
          tb_vmt = drs_vmt,
          tb = .pass_tb,
          .mode = mode_1,
          # .fuel_type = "SI",
          .aeo_scenario = .aeo_scenario,
          .miles_per_gallon = mpg,
          .is_av = TRUE
        )


      drs_ghg_emb <- calc_ghg_embodied(
        tb = .pass_tb,
        .class = class,
        .mode = mode,
        .sales_mode = "DRSSales",
        .fuel_type = "HEV-EMB"
      ) %>%
        unique()

      drs_cost <-
        calc_cost(
          tb_vmt = drs_vmt,
          .mode = mode_1,
          .price = "HEVPrice",
          .is_av = TRUE,
          .enviro_factors = .enviro_factors
        )
    } else if (.drs_fuel_type == "PHEV") {
      ## DRS Plug-in hybrid -----
      stock <- "DRSStock"
      mpg <- "PHEVMPG"
      mpe <- "PHEVElec"
      class <- "PHEV"


      #### VMT gas ----

      fcm <- calc_fuel_cost_mile(
        tb = .pass_tb,
        .mode = mode,
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = mpg,
        .fuel_cost_gallon = .enviro_factors$SI_FUEL_COST_GAL,
        .av_pct = .av_pct,
        .enviro_factors = .enviro_factors
      )


      # Don't apply the .gas_tax factors, etc. to PHEV for DRS
      phev_vmt_gas <- calc_vmt_forecast_drs(
        .scenario = .scenario,
        tb = .pass_tb,
        .phev_electric = FALSE,
        .mode = mode,
        .stock = stock,
        .variable = var,
        .tb_fuel_cost_mile = fcm,
        .aeo_scenario = .aeo_scenario,
        .transit_avo_pct = .transit_avo_pct,
        .pldv_avo_pct = .pldv_avo_pct,
        .transit_service_pct = .transit_service_pct,
        .vmt_fee = .vmt_fee,
        .payd_fee = .payd_fee,
        .gas_tax = .gas_tax,
        .cong_price = .cong_price,
        .parking_price = .parking_price,
        .freight_parking_price = .freight_parking_price,
        .drs_pct = .drs_pct,
        .av_pct = .av_pct,
        .freight_vmt_fee = .freight_vmt_fee,
        .pop_dens_pct_change = .pop_dens_pct_change,
        .emp_dens_pct_change = .emp_dens_pct_change,
        .land_use_diversity_pct_change = .land_use_diversity_pct_change,
        .intersection_design_pct_change = .intersection_design_pct_change,
        .job_access_pct_change = .job_access_pct_change,
        .transit_dist_pct_change = .transit_dist_pct_change,
        .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
        .telework_pct = .telework_pct
      ) %>%
        mutate(class = class)

      #### VMT electric ----

      fcm_electric <- calc_fuel_cost_mile(
        .pass_tb, mode, .aeo_scenario,
        mpe, .enviro_factors$ELEC_FUEL_COST_KWH
      )


      phev_vmt_electric <- calc_vmt_forecast_drs(
        .scenario = .scenario,
        tb = .pass_tb,
        .phev_electric = TRUE,
        .mode = mode,
        .stock = stock,
        .variable = var,
        .tb_fuel_cost_mile = fcm_electric,
        .aeo_scenario = .aeo_scenario,
        .transit_avo_pct = .transit_avo_pct,
        .pldv_avo_pct = .pldv_avo_pct,
        .transit_service_pct = .transit_service_pct,
        .vmt_fee = .vmt_fee,
        .payd_fee = .payd_fee,
        .gas_tax = .gas_tax,
        .cong_price = .cong_price,
        .parking_price = .parking_price,
        .freight_parking_price = .freight_parking_price,
        .drs_pct = .drs_pct,
        .av_pct = .av_pct,
        .freight_vmt_fee = .freight_vmt_fee,
        .pop_dens_pct_change = .pop_dens_pct_change,
        .emp_dens_pct_change = .emp_dens_pct_change,
        .land_use_diversity_pct_change = .land_use_diversity_pct_change,
        .intersection_design_pct_change = .intersection_design_pct_change,
        .job_access_pct_change = .job_access_pct_change,
        .transit_dist_pct_change = .transit_dist_pct_change,
        .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
        .telework_pct = .telework_pct
      ) %>%
        mutate(class = class)

      # combine vmt tables
      drs_vmt <- left_join(
        phev_vmt_electric %>%
          select(everything(),
            vmt_electric = vmt
          ),
        phev_vmt_gas %>%
          select(everything(),
            vmt_gas = vmt
          ),
        c(
          "type", "stock", "class",
          "scenario", "mode", "ctu",
          "year", "aeo_mode"
        )
      ) %>%
        rowwise() %>%
        mutate(
          vmt = vmt_electric + vmt_gas,
          class = class
        ) %>%
        select(
          -vmt_electric,
          -vmt_gas
        )


      ##### gas direct, fuel, indirect -----
      phev_ghg_gas <- calc_ghg_direct(
        tb_vmt = phev_vmt_gas,
        tb = .pass_tb,
        .mode = mode,
        .fuel_type = "SI",
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = mpg,
        .is_av = TRUE,
        .enviro_factors = .enviro_factors
      ) %>%
        select(everything(),
          dir_ghg_gas = dir_ghg
        )


      phev_fuel_gas <- calc_fuel_use(
        tb_vmt = phev_vmt_gas,
        tb = .pass_tb,
        .mode = mode,
        # .fuel_type = "SI",
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = mpg,
        .enviro_factors = .enviro_factors,
        .is_av = TRUE
      )


      ##### electric direct, fuel, indirect -----
      phev_ghg_electric <- calc_ghg_direct(
        tb_vmt = phev_vmt_electric,
        tb = .pass_tb,
        .mode = mode,
        .fuel_type = .electric_scenario,
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = mpe,
        .is_av = TRUE,
        .enviro_factors = .enviro_factors
      ) %>%
        select(everything(),
          dir_ghg_electric = dir_ghg
        )

      phev_fuel_electric <-
        calc_fuel_use(
          tb_vmt = phev_vmt_electric,
          tb = .pass_tb,
          .mode = mode,
          # .electric_scenario,
          .aeo_scenario = .aeo_scenario,
          .miles_per_gallon = mpe,
          .is_av = TRUE
          # .fuel_type = .electric_scenario
        )


      # combine direct emission tables
      drs_dir_ghg <- left_join(
        phev_ghg_gas, phev_ghg_electric,
        c(
          "scenario", "mode", "ctu",
          "year", "aeo_mode", "class"
        )
      ) %>%
        mutate(dir_ghg = dir_ghg_electric + dir_ghg_gas) %>%
        select(
          -dir_ghg_electric,
          -dir_ghg_gas
        )


      # combine fuel use tables
      drs_fuel <- left_join(
        phev_fuel_electric %>%
          select(everything(),
            fuel_use_electric = fuel_use
          ),
        phev_fuel_gas %>%
          select(everything(),
            fuel_use_gas = fuel_use
          ),
        c(
          "scenario", "mode", "ctu", "year",
          "aeo_mode"
        )
      ) %>%
        rowwise() %>%
        mutate(fuel_use = fuel_use_gas + fuel_use_electric) %>%
        select(
          -fuel_use_gas,
          -fuel_use_electric
        )


      drs_ghg_emb <-
        calc_ghg_embodied(
          tb = .pass_tb,
          .mode = .mode,
          .class = class,
          .sales_mode = "DRSSales",
          .fuel_type = "PHEV-EMB",
          .enviro_factors = .enviro_factors,
          .transit_avo_pct = .transit_avo_pct
        )

      drs_cost <-
        calc_cost(
          tb_vmt = phev_vmt,
          .mode =  mode,
          .price = "PHEVPrice",
          .is_av = TRUE
        )
    } else {
      ## DRS Battery Electric -----
      stock <- "DRSStock"
      mpe <- "BEVElec"
      class <- "BEV"


      fcm <- calc_fuel_cost_mile(
        .pass_tb,
        .mode = mode_1,
        .aeo_scenario = .aeo_scenario,
        .miles_per_gallon = mpe,
        .fuel_cost_gallon = .enviro_factors$ELEC_FUEL_COST_KWH,
        .enviro_factors = .enviro_factors
      )


      drs_vmt <-
        calc_vmt_forecast_drs(
          .scenario = .scenario,
          tb = .pass_tb,
          .mode = mode,
          .stock = stock,
          .variable = var,
          .tb_fuel_cost_mile = fcm,
          .aeo_scenario = .aeo_scenario,
          .transit_avo_pct = .transit_avo_pct,
          .pldv_avo_pct = .pldv_avo_pct,
          .transit_service_pct = .transit_service_pct,
          .vmt_fee = .vmt_fee,
          .payd_fee = .payd_fee,
          .gas_tax = .gas_tax,
          .cong_price = .cong_price,
          .parking_price = .parking_price,
          .freight_parking_price = .freight_parking_price,
          .drs_pct = .drs_pct,
          .drs_fuel_type = .drs_fuel_type,
          .av_pct = .av_pct,
          .freight_vmt_fee = .freight_vmt_fee,
          .pop_dens_pct_change = .pop_dens_pct_change,
          .emp_dens_pct_change = .emp_dens_pct_change,
          .land_use_diversity_pct_change = .land_use_diversity_pct_change,
          .intersection_design_pct_change = .intersection_design_pct_change,
          .job_access_pct_change = .job_access_pct_change,
          .transit_dist_pct_change = .transit_dist_pct_change,
          .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
          .telework_pct = .telework_pct,
          .phev_electric = .phev_electric,
          .enviro_factors = enviro_factors
        ) %>%
        mutate(class = class)

      drs_dir_ghg <-
        calc_ghg_direct(
          tb_vmt = drs_vmt,
          tb = .pass_tb,
          .mode = mode_1,
          .fuel_type = .electric_scenario,
          .aeo_scenario = .aeo_scenario,
          .miles_per_gallon = mpe,
          .is_av = TRUE,
          .enviro_factors = .enviro_factors
        )

      drs_fuel <-
        calc_fuel_use(
          tb_vmt = drs_vmt,
          tb = .pass_tb,
          .mode = mode_1,
          # .fuel_type = .electric_scenario,
          .aeo_scenario = .aeo_scenario,
          .miles_per_gallon = mpe,
          .is_av = TRUE
        )


      drs_ghg_emb <- calc_ghg_embodied(
        tb = .pass_tb,
        .class = class,
        .mode = mode,
        .sales_mode = "DRSSales",
        .fuel_type = "BEV-EMB"
      )

      # out_sum <- out_sum %>%
      #   dplyr::mutate(dplyr::across(all_of(YRS), ~ dplyr::case_when(
      #     (mode == mode_1 &
      #        class == class &
      #        output == "INDIR-GHG") ~ .x + temp %>% as.numeric(),
      #     TRUE ~ .x
      #   )))

      drs_cost <-
        calc_cost(
          tb_vmt = drs_vmt,
          .mode = mode,
          .price = "BEVPrice",
          .is_av = TRUE,
          .enviro_factors = .enviro_factors
        )
    }

    fuel_use_all <- dplyr::bind_rows(drs_fuel)

    vmt_all <- dplyr::bind_rows(drs_vmt)

    emb_ghg_all <- dplyr::bind_rows(drs_ghg_emb)

    dir_ghg_all <- dplyr::bind_rows(drs_dir_ghg)

    cost_all <- dplyr::bind_rows(drs_cost)

    dynamic_ride_share <- list(
      "vmt" = vmt_all,
      "dir_ghg" = dir_ghg_all,
      "emb_ghg" = emb_ghg_all,
      "fuel_use" = fuel_use_all,
      "cost" = cost_all
    )
  } else {
    # return a basic shell with NA values
    vmt_all <- .pass_tb %>%
      dplyr::select(ctu, year) %>%
      unique() %>%
      dplyr::mutate(
        type = type,
        stock = "DRSStock",
        scenario = .scenario,
        mode = mode,
        aeo_mode = "LDV",
        vmt = NA,
        class = NA
      )

    fuel_use_all <- vmt_all %>%
      dplyr::select(type, scenario, mode, ctu, year, aeo_mode) %>%
      dplyr::mutate(fuel_use = NA)

    dir_ghg_all <- fuel_use_all %>%
      dplyr::select(type, scenario, mode, ctu, year, aeo_mode,
        dir_ghg = fuel_use
      )

    emb_ghg_all <- fuel_use_all %>%
      dplyr::select(type, scenario, mode, ctu, year, aeo_mode,
        ghg_embodied = fuel_use
      )

    cost_all <- fuel_use_all %>%
      dplyr::select(type, scenario, mode, ctu, year, aeo_mode,
        vmt_cost = fuel_use
      )


    dynamic_ride_share <- list(
      "vmt" = vmt_all,
      "dir_ghg" = dir_ghg_all,
      "emb_ghg" = emb_ghg_all,
      "fuel_use" = fuel_use_all,
      "cost" = cost_all
    )
  }



  # Finish up -----

  usethis::ui_done(paste("Dynamic ride sharing", emo::ji("fast")))

  return(dynamic_ride_share)
}
