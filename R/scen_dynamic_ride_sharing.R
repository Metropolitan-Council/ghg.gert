#' Calculate dynamic ride sharing scenario
#'
#' @inheritParams scenario_results
#' @inheritParams calc_vmt_forecast
#' @inheritParams calc_drs_sales
#' @inheritParams calc_drs_vmt
#' @return
#' @export
#'
#' @family transportation results, passenger
#' @keywords passenger
#'
#'
#' @importFrom emo ji
scen_dynamic_ride_sharing <- function(.scenario = "BAU",
                                      .pass_tb = transportation_data$passenger,
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
  browser()
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
    browser()
    # Calculate DRS sales in each year
    drs_sales <-
      calc_drs_sales(.pass_tb, .drs_pct)

    .pass_tb <-
      dplyr::bind_rows(.pass_tb, drs_sales)

    if (.drs_fuel_type == "HEV") {
      ## DRS Hybrid fuel -----
      stock <- "DRSStock"
      mpg <- "HEVMPG"
      class <- "HEV"

      fcm <- calc_fuel_cost_mile(
        .pass_tb, mode,
        .aeo_scenario, mpg, .enviro_factors$SI_FUEL_COST_GAL
      )

      drs_vmt <-
        calc_drs_vmt(
          .pass_tb, .drs_pct,
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
          .pass_tb,
          mode_1, "SI",
          .aeo_scenario, mpg, 1
        )
      .drs_fuel_type <-
        calc_fuel_use(
          drs_vmt,
          .pass_tb,
          mode_1, "SI",
          .aeo_scenario, mpg, 1
        )

      temp <- calc_ghg_embodied(
        .pass_tb,
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
    } else if (.drs_fuel_type == "PHEV") {

      ## DRS Plug-in hybrid -----
      stock <- "DRSStock"
      mpg <- "PHEVMPG"
      mpe <- "PHEVElec"
      class <- "PHEV"

      # Don't apply the .gas_tax factors, etc. to PHEV for DRS
      phev_vmtg <- calc_drs_vmt(
        .pass_tb, .drs_pct, class, fcm, .vmt_fee,
        .payd_fee, .gas_tax, .cong_price, .parking_price, .pop_dens_pct_change,
        .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change,
        .job_access_pct_change, .transit_dist_pct_change,
        .comb_5d_impact_pct_change
      ) * (
        1 - .pass_tb %>%
          dplyr::filter(mode == mode, var == "PHEVPr") %>%
          dplyr::select(all_of(YRS)))

      phev_vmte <- calc_drs_vmt(
        .pass_tb, .drs_pct, class,
        fcm, .vmt_fee, .payd_fee, .gas_tax, .cong_price,
        .parking_price, .pop_dens_pct_change, .emp_dens_pct_change,
        .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
        .transit_dist_pct_change, .comb_5d_impact_pct_change
      ) *
        .pass_tb %>%
          dplyr::filter(mode == mode, var == "PHEVPr") %>%
          dplyr::select(all_of(YRS))

      drs_vmt <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "VMT",
        phev_vmtg + phev_vmte
      )

      phev_ghgg <- calc_ghg_direct(
        phev_vmtg, .pass_tb,
        mode, "SI", .aeo_scenario, mpg, 1
      )

      phev_ghge <- calc_ghg_direct(
        phev_vmte, .pass_tb,
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
          .pass_tb, mode, "SI",
          .aeo_scenario, mpg, 1
        )
      )

      drs_fuele <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "ELEC",
        calc_fuel_use(
          phev_vmte, .pass_tb,
          mode, .electric_scenario, .aeo_scenario, mpe, 1
        )
      )

      temp <- calc_ghg_embodied(
        .pass_tb, mode,
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
        output = "COST",
        calc_cost(
          drs_vmt,
          mode_1, "PHEVPrice", 1
        )
      )
    } else {
      ## DRS Battery Electric -----
      stock <- "DRSStock"
      mpe <- "BEVElec"
      class <- "BEV"
      drs_vmt <-
        calc_drs_vmt(
          .pass_tb, .drs_pct,
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
          .pass_tb,
          mode_1, .electric_scenario,
          .aeo_scenario, mpe, 1
        )

      .drs_fuel_type <-
        calc_fuel_use(
          drs_vmt,
          .pass_tb, mode_1, .electric_scenario,
          .aeo_scenario, mpe, 1
        )


      temp <- calc_ghg_embodied(
        .pass_tb,
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
        class = class,
        output = "COST",
        calc_cost(
          drs_vmt,
          mode_1, "BEVPrice"
        )
      )
    }
  }



  # Finish up -----

  # fuel_use_all <- dplyr::bind_rows(
  #   ci_fuel,
  #   hev_fuel,
  #   bev_fuel,
  #   ci_brt_fuel,
  #   hev_brt_fuel,
  #   bev_brt_fuel
  # )
  #
  #
  # vmt_all <- dplyr::bind_rows(
  #   ci_vmt,
  #   hev_vmt,
  #   bev_vmt,
  #   ci_brt_vmt,
  #   hev_brt_vmt,
  #   bev_brt_vmt
  # )
  #
  # emb_ghg_all <- dplyr::bind_rows(
  #   ci_emb_ghg,
  #   hev_emb_ghg,
  #   bev_emb_ghg,
  #   ci_brt_emb_ghg,
  #   hev_brt_emb_ghg,
  #   bev_brt_emb_ghg
  # )
  #
  # dir_ghg_all <- dplyr::bind_rows(
  #   ci_dir_ghg,
  #   hev_dir_ghg,
  #   bev_dir_ghg,
  #   ci_brt_ghg,
  #   hev_brt_ghg,
  #   bev_brt_ghg
  # )
  #
  # cost_all <- dplyr::bind_rows(
  #   ci_cost,
  #   hev_cost,
  #   bev_cost,
  #   ci_brt_cost,
  #   hev_brt_cost,
  #   bev_brt_cost
  # )
  #
  # bus_scenario <- list(
  #   "vmt" = vmt_all,
  #   "dir_ghg" = dir_ghg_all,
  #   "emb_ghg" = emb_ghg_all,
  #   "fuel_use" = fuel_use_all,
  #   "cost" = cost_all
  # )

  usethis::ui_done(paste("Dynamic ride sharing", emo::ji("fast")))
}
