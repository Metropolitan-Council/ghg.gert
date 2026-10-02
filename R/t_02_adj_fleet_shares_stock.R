#' @title Adjust passenger light duty fleet power train stock distribution before running scenario
#' @family stock adjustments
#' @family transportation
#'
#' @description  Match what the user input for stock in the final forecast year rather
#'     than the defaults from MA3TFleet held fixed in all cases.
#'
#'     Adjusting the existing passenger vehicle stock takes into account vehicle ownership
#'     cost elasticity, gas taxes, and VMT and PAYD fees.
#'     If battery electric, plug-in hybrid, and/or hybrid percent of stock in the final forecast year
#'     (`.bev_pct_stock`, `.hev_pct_stock`) are specified,
#'     Freight data is dependent on passenger data,
#' @note This function restricts inputs to include either a PAYD fee or a VMT fee, but not both.
#'     There are differences in their acceptability and implementation, but they are essentially
#'     targeting the same thing. A VMT fee would be paid by the driver and could be varied based
#'     on the time of day and location. However, it could also be a straight fee per mile. PAYD
#'     is paid to an insurance provider in place of a flat insurance rate. The main difference is
#'     whether the driver would prefer to pay the cost to a government agency or insurance provider.
#'
#' @param .bev_pct_stock numeric,  a value between `0` and `1.`
#'     Percent of all vehicle stock that are battery electric vehicles (BEV)
#'     in the final forecast year.
#'      Default is `r ghg.gert::transportation_defaults$bev_pct_stock`
#' @param .hev_pct_stock  numeric,   a value between `0` and `1.`
#'    Percent of all vehicle stock that are hybrid electric vehicles (HEV)
#'    in the final forecast year.
#'     Default is `r ghg.gert::transportation_defaults$hev_pct_stock`.
#' @param .pass_tb [tibble::tibble()]. Passenger input table.
#'   Default is `transportation_data$passenger`.
#' @param .freight_tb [tibble::tibble()] Freight input table.
#'    Default is `transportation_data$freight`.
#' @inheritParams calc_vmt_forecast
#' @inheritParams vmt_road_policy
#' @inheritParams filter_ctu
#'
#' @return [tibble::tibble()] with column names...
#' @export
#'
#' @importFrom tidyselect all_of
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom dplyr filter select case_when mutate across summarise group_by ungroup cur_column left_join bind_rows right_join
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom purrr map2
#' @importFrom cli cli_alert_warning cli_abort
#'
adj_fleet_shares_stock <- function(.pass_tb,
                                   .freight_tb,
                                   .selected_ctu = "all",
                                   .bev_pct_stock = ghg.gert::transportation_defaults$bev_pct_stock,
                                   .hev_pct_stock = ghg.gert::transportation_defaults$hev_pct_stock,
                                   .vmt_fee = ghg.gert::transportation_defaults$vmt_fee,
                                   .payd_fee = ghg.gert::transportation_defaults$payd_fee,
                                   .gas_tax = ghg.gert::transportation_defaults$gas_tax,
                                   .elast = elast,
                                   .enviro_factors = enviro_factors) {
  # browser()
  .pass_tb <- filter_ctu(.pass_tb, .selected_ctu = .selected_ctu) %>% dplyr::distinct()
  .freight_tb <- filter_ctu(.freight_tb, .selected_ctu = .selected_ctu) %>% dplyr::distinct()

  pass_tb <- .pass_tb
  freight_tb <- .freight_tb
  # check inputs -----
  l_names <- c(
    "bev_pct_stock",
    "hev_pct_stock",
    "vmt_fee",
    "payd_fee",
    "gas_tax"
  )

  l_vals <- list(
    .bev_pct_stock,
    .hev_pct_stock,
    .vmt_fee,
    .payd_fee,
    .gas_tax
  )

  purrr::map2(l_names, l_vals, check_inputs)

  if ((.bev_pct_stock + .hev_pct_stock) > 0.9) {
    cli::cli_warn("Proportion of alternate fuel vehicle stock will exceed 90% of all vehicles.")
  }

  # vmt, payd, gas -----
  # Adjust stock based on ownership response to price elasticity

  if (.vmt_fee > 0 | .payd_fee > 0 | .gas_tax > 0) {
    if (.vmt_fee > 0 & .payd_fee > 0) {
      cli::cli_abort("Implement a VMT fee OR a pay-as-you drive insurance fee, not both.")
    }
    auto_cost <- .enviro_factors$AUTO_COST_MI
    own_elast <- .elast$vehicle_ownership_elast

    adj_si_ci_final <- (1 + ((.vmt_fee + .payd_fee) / auto_cost) * own_elast) *
      (1 + (.gas_tax / auto_cost) * own_elast)

    adj_si_ci_stock <- tibble::tibble(
      year      = .elast$year,
      adj_si_ci = c(1, 1, seq(1, adj_si_ci_final[8], by = -(1 - adj_si_ci_final[8]) / 6))
    )

    adj_alt_stock <- tibble::tibble(
      year    = .elast$year,
      adj_alt = 1 + ((.vmt_fee + .payd_fee) / auto_cost) * own_elast
    )

    pass_tb <- pass_tb %>%
      dplyr::left_join(adj_alt_stock, by = "year") %>%
      dplyr::left_join(adj_si_ci_stock, by = "year") %>%
      dplyr::mutate(value = dplyr::case_when(
        mode == "PLDV" & var %in% c("BEVExist", "HEVExist") ~ value * adj_alt,
        mode == "PLDV" & var %in% c("SIExist", "CIExist") ~ value * adj_si_ci,
        TRUE ~ value
      )) %>%
      dplyr::select(names(.pass_tb))

    new_tot_exist <- pass_tb %>%
      dplyr::filter(mode == "PLDV", var %in% c("BEVExist", "HEVExist", "SIExist", "CIExist")) %>%
      dplyr::group_by(year, mode, geog_name, geog_id, aeo_mode, type) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      dplyr::mutate(var = "TotExist") %>%
      dplyr::select(names(.pass_tb))

    pass_tb <- pass_tb %>%
      dplyr::anti_join(new_tot_exist, by = c("year", "mode", "geog_name", "geog_id", "aeo_mode", "type", "var")) %>%
      dplyr::bind_rows(new_tot_exist)

    if (nrow(.pass_tb) != nrow(pass_tb)) {
      cli::cli_abort("Passenger data did not pass VMT/PAYD and vehicle ownership elasticity adjustment")
    }
  }

  # hev/bev  -----
  if (.bev_pct_stock > 0 | .hev_pct_stock > 0) {
    ## passenger-----


    # get original stock percentages for 2050
    stock_percentages_original <- pass_tb %>%
      dplyr::filter(
        !var %in% c("TotStock"),
        stringr::str_detect(var, "Stock"),
        mode == "PLDV",
        year == 2050
      ) %>%
      tidyr::pivot_wider(names_from = var, values_from = value) %>%
      janitor::adorn_percentages()

    if (.hev_pct_stock == 0) {
      .hev_pct_stock <- round(stock_percentages_original$HEVStock, digits = 2)
    }

    all_stock_pcts <- 1 - (.bev_pct_stock + .hev_pct_stock)

    if (all_stock_pcts < 0) {
      cli::cli_warn("Maximum BEV percentage reached")
      .bev_pct_stock <- 0.98
      .hev_pct_stock <- 0.01
      all_stock_pcts <- 1 - (.bev_pct_stock + .hev_pct_stock)
    }

    # create elasticities - hoist shared length calc
    n_yrs <- length(unique(pass_tb$year))
    yrs <- unique(pass_tb$year)

    calc_stock_elast <- function(pct) {
      c(calc_elasticity(
        elas_list = rep(0, n_yrs), elas = pct,
        num_inits = 3, num_yrs = n_yrs - 3
      )[1:9])
    }

    stock_elast <- tibble::tibble(
      year      = yrs,
      bev_elast = calc_stock_elast(.bev_pct_stock),
      hev_elast = calc_stock_elast(.hev_pct_stock)
    ) %>%
      dplyr::mutate(
        si_ci_elast = 1 - (bev_elast + hev_elast),
        si_ci_elast = ifelse(year %in% c("2015", "2018", "2020"), 1, si_ci_elast)
      )

    ### stock ------
    forecast_years <- c("2025", "2030", "2035", "2040", "2045", "2050")

    pass_tb_stock_new <- pass_tb %>%
      dplyr::filter(mode == "PLDV", stringr::str_detect(var, "Stock")) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = "var", values_from = "value") %>%
      dplyr::filter(year %in% forecast_years) %>%
      dplyr::left_join(stock_elast, by = "year") %>%
      # remove rowwise() - all operations are vectorized
      dplyr::mutate(
        si_share  = SIStock / (SIStock + CIStock),
        ptb_si    = si_ci_elast * si_share,
        ptb_ci    = si_ci_elast * (1 - si_share),
        ptb_hev   = hev_elast,
        ptb_bev   = bev_elast,
        ptb_tot   = ptb_si + ptb_ci + ptb_hev + ptb_bev,
        HEVStock  = TotStock * ptb_hev / ptb_tot,
        BEVStock  = TotStock * ptb_bev / ptb_tot,
        SIStock   = TotStock * ptb_si / ptb_tot,
        CIStock   = TotStock * ptb_ci / ptb_tot
      ) %>%
      dplyr::select(dplyr::any_of(c(names(pass_tb), "SIStock", "CIStock", "HEVStock", "BEVStock", "TotStock"))) %>%
      tidyr::pivot_longer(
        cols = c("SIStock", "CIStock", "HEVStock", "BEVStock", "TotStock"),
        names_to = "var", values_to = "value"
      )

    if (any(pass_tb_stock_new$value <= 0)) {
      browser()
    }

    # replace updated stock rows using a proper join key instead of paste()
    join_cols <- c("mode", "geog_name", "geog_id", "year", "var", "aeo_mode")

    ptb_new <- pass_tb %>%
      dplyr::distinct() %>%
      dplyr::anti_join(pass_tb_stock_new, by = join_cols) %>%
      dplyr::bind_rows(pass_tb_stock_new) %>%
      dplyr::select(names(.pass_tb))

    if (nrow(ptb_new) != nrow(.pass_tb)) {
      cli::cli_abort("Passenger data did not pass HEV/BEV adjustment")
    }

    # check totals are unchanged
    check_vars <- c("TotSales", "TotStock", "TotExist", "PMT")
    test_passenger <- purrr::map_lgl(check_vars, function(x) {
      isTRUE(all.equal(
        dplyr::filter(pass_tb, mode == "PLDV", var == x),
        dplyr::filter(ptb_new, mode == "PLDV", var == x),
        tolerance = 0.01
      ))
    })

    if (!all(test_passenger)) {
      cli::cli_abort("Passenger CTU totals failed")
    }
    # freight --------
    # BAU assumes 1/3 and 2/3 change (relative to PLDV in 2025-2040) to
    #  freight sales to include BEV (as summation of BEV+HEV from PLDV)
    #  for SUT and CUT, respectively
    #
    # We do not have good stock numbers on freight so we do NOT consider
    # the embodied emissions from freight and the shift in sales,
    # etc. so  the passenger fleet is translated into a
    # total stock number for freight. The use of 1/3 and 2/3
    # helps to account for this being sales
    # not total stock (i.e., should be lower as percent of total stock)
    # # browser()
    #
    #
    # freight_battery_fin_year <- freight_tb %>%
    #   dplyr::filter(
    #     mode %in% c(
    #       "SUT",
    #       "CUT"
    #     ),
    #   ) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::filter(year == max(year)) %>%
    #   dplyr::mutate(bev_pcts_fin = .bev_pct_stock) %>% # * (BEVSales / BEVStock)) %>%
    #   dplyr::select(mode, geog_name, geog_id, bev_pcts_fin) %>%
    #   unique()
    #
    #
    # freight_battery <- stock_elast %>%
    #   dplyr::ungroup() %>%
    #   dplyr::mutate(bev_pcts = bev_elast) %>% # * (BEVSales / BEVStock)) %>%
    #   dplyr::select(year, bev_pcts) %>%
    #   unique()
    #
    #
    # ### stock ----
    # # Temporarily update any zero stock in final year to equal 1
    # freight_stock <- freight_tb %>%
    #   dplyr::ungroup() %>%
    #   dplyr::filter(
    #     mode %in% c(
    #       "SUT",
    #       "CUT"
    #     ),
    #     stringr::str_detect(var, "Stock")
    #   ) %>%
    #   dplyr::mutate(value = dplyr::case_when(
    #     ((mode == "SUT" | mode == "CUT") &
    #        stringr::str_detect(var, "Stock") & value == 0) ~ 1,
    #     TRUE ~ value
    #   )) %>%
    #   dplyr::distinct() %>%
    #   dplyr::select(-aeo_mode) %>%
    #   tidyr::pivot_wider(
    #     names_from = c(var, mode),
    #     values_from = value
    #   ) %>%
    #   dplyr::group_by(year, geog_name, geog_id) %>%
    #   dplyr::mutate(dplyr::across(4:7, function(x) {
    #     sum(x, na.rm = TRUE)
    #   })) %>%
    #   unique()
    #
    #
    #
    # freight_stock_fin_year <- freight_stock %>%
    #   dplyr::ungroup() %>%
    #   dplyr::filter(year == max(year)) %>%
    #   dplyr::rowwise() %>%
    #   dplyr::mutate(
    #     ci_sut_fin = CIStock_SUT / TotStock_SUT,
    #     bev_sut_fin = BEVStock_SUT / TotStock_SUT,
    #     ci_cut_fin = CIStock_CUT / TotStock_CUT,
    #     bev_cut_fin = BEVStock_CUT / TotStock_CUT
    #   ) %>%
    #   dplyr::select(
    #     geog_name, geog_id,
    #     ci_sut_fin,
    #     bev_sut_fin,
    #     ci_cut_fin,
    #     bev_cut_fin
    #   ) %>%
    #   dplyr::distinct() %>%
    #   filter(!is.na(ci_cut_fin))
    # # browser()
    #
    # freight_stock_new <- freight_stock %>%
    #   dplyr::left_join(freight_stock_fin_year, by = c("geog_name", "geog_id")) %>%
    #   dplyr::left_join(freight_battery, by = c("year")) %>%
    #   dplyr::left_join(freight_battery_fin_year, by = c("geog_name", "geog_id")) %>%
    #   dplyr::rowwise() %>%
    #   dplyr::mutate(
    #     ci_stock_sut = ((1 / 3) * bev_pcts) *
    #       (CIStock_SUT / TotStock_SUT) /
    #       ci_sut_fin,
    #
    #     ci_stock_cut = ((2 / 3) * bev_pcts) *
    #       (CIStock_CUT / TotStock_CUT) /
    #       ci_cut_fin,
    #
    #     bev_stock_sut = ((2 / 3) * bev_pcts) *
    #       (BEVStock_SUT / TotStock_SUT) /
    #       bev_sut_fin,
    #
    #     bev_stock_cut = ((1 / 3) * bev_pcts) *
    #       (BEVStock_CUT / TotStock_CUT) /
    #       bev_cut_fin
    #   ) %>%
    #   dplyr::mutate(
    #     sut_tot_stock = ci_stock_sut + bev_stock_sut,
    #     cut_tot_stock = ci_stock_cut + bev_stock_cut,
    #     bev_sut = bev_stock_sut / sut_tot_stock,
    #     bev_cut = bev_stock_cut / cut_tot_stock,
    #     ci_sut = ci_stock_sut / sut_tot_stock,
    #     ci_cut = ci_stock_cut / cut_tot_stock
    #   ) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::select(
    #     year, bev_sut, bev_cut,
    #     ci_sut, ci_cut
    #   ) %>%
    #   unique()
    #
    #
    # freight_tot_stock <- freight_tb %>%
    #   dplyr::ungroup() %>%
    #   dplyr::filter(
    #     var == "TotStock",
    #     mode %in% c(
    #       "SUT",
    #       "CUT"
    #     )
    #   ) %>%
    #   dplyr::distinct() %>%
    #   tidyr::pivot_wider(
    #     names_from = var,
    #     values_from = value
    #   ) %>%
    #   dplyr::select(year, geog_name, geog_id, mode, TotStock) %>%
    #   unique()
    #
    #
    # ftb_new_stocks <- freight_tb %>%
    #   dplyr::ungroup() %>%
    #   dplyr::filter(mode %in% c(
    #     "SUT",
    #     "CUT"
    #   )) %>%
    #   dplyr::left_join(freight_stock_new, by = c("year")) %>%
    #   dplyr::distinct() %>%
    #   tidyr::pivot_wider(
    #     names_from = var,
    #     values_from = value
    #   ) %>%
    #   dplyr::rowwise() %>%
    #   dplyr::mutate(
    #     BEVStock = dplyr::case_when(
    #       !year %in% c("2015", "2018", "2020") & mode == "SUT" ~ TotStock * bev_sut,
    #       !year %in% c("2015", "2018", "2020") & mode == "CUT" ~ TotStock * bev_cut
    #     ),
    #     CIStock = dplyr::case_when(
    #       !year %in% c("2015", "2018", "2020") & mode == "SUT" ~ TotStock * ci_sut,
    #       !year %in% c("2015", "2018", "2020") & mode == "CUT" ~ TotStock * ci_cut
    #     )
    #   ) %>%
    #   dplyr::mutate(TotStock = BEVStock + CIStock) %>%
    #   tidyr::pivot_longer(
    #     cols = c(
    #       "TMT", "TotStock",
    #       "CIStock", "BEVStock"
    #     ),
    #     names_to = "var",
    #     values_to = "value",
    #     values_drop_na = TRUE
    #   ) %>%
    #   dplyr::distinct() %>%
    #   dplyr::select(names(freight_tb)) %>%
    #   dplyr::mutate(all_combos = paste(mode, geog_name, geog_id, year, var, aeo_mode,
    #                                    sep = "-"
    #   ))
    #
    #
    # ftb_new <- freight_tb %>%
    #   dplyr::mutate(all_combos = paste(mode, geog_name, geog_id, year, var, aeo_mode,
    #                                    sep = "-"
    #   )) %>%
    #   dplyr::filter(!all_combos %in% ftb_new_stocks$all_combos) %>%
    #   dplyr::bind_rows(ftb_new_stocks) %>%
    #   dplyr::select(names(freight_tb))
    #
    # if (nrow(.freight_tb) != nrow(ftb_new)) {
    #   cli::cli_abort("Freight data did not pass adjustment")
    # }
    #
    #
    #
    # test_freight <- purrr::map(
    #   c(
    #     "TotSales",
    #     "TotStock",
    #     "TotExist",
    #     "TMT"
    #   ),
    #   function(x) {
    #     all.equal(
    #       ftb_new %>%
    #         filter(
    #           var == x
    #         ),
    #       ftb_new %>%
    #         filter(
    #           var == x
    #         ),
    #       tolerance = 0.01
    #     )
    #   }
    # )
    #
    # if (any(test_freight == FALSE)) {
    #   cli::cli_abort("Freight CTU totals failed")
    # }
    ftb_new <- freight_tb
  } else {
    ptb_new <- pass_tb
    ftb_new <- freight_tb
  }

  # return-----
  return(list(pass = ptb_new, freight = ftb_new))
}
