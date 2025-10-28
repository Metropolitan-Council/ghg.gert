#' @title Adjust passenger light duty fleet power train distribution before running scenario
#' @family stock adjustments
#' @family transportation
#'
#' @description  Match what the user input for sales in the final forecast year rather
#'     than the defaults from MA3TFleet held fixed in all cases.
#'
#'     Jason has a model used to forecast vehicle fleet shares between powertrains.
#'     Distribute personal vehicle VMT between the different powertrains.
#'     Ratio of vehicle stocks by powertrain type.
#'     If the user inputs a different percent for BEV, update vehicles by existing
#'     vehicles and new sales
#'
#'     Adjusting the existing passenger vehicle stock takes into account vehicle ownership
#'     cost elasticity, gas taxes, and VMT and PAYD fees.
#'     If battery electric, plug-in hybrid, and/or hybrid percent of sales in the final forecast year
#'     (`.bev_pct_sales`, `.phev_pct_sales`, `.hev_pct_sales`) are specified,
#'     Freight data is dependent on passenger data,
#' @note This function restricts inputs to include either a PAYD fee or a VMT fee, but not both.
#'     There are differences in their acceptability and implementation, but they are essentially
#'     targeting the same thing. A VMT fee would be paid by the driver and could be varied based
#'     on the time of day and location. However, it could also be a straight fee per mile. PAYD
#'     is paid to an insurance provider in place of a flat insurance rate. The main difference is
#'     whether the driver would prefer to pay the cost to a government agency or insurance provider.
#'
#'     Stock = Exist + Sales
#'
#'
#' @param .bev_pct_sales numeric,  a value between `0` and `1.`
#'     Percent of all vehicle  sales that are battery electric vehicles (BEV)
#'     in the final forecast year.
#'     Default is `0`.
#' @param .phev_pct_sales numeric,  a value between `0` and `1.`
#'    Percent of all vehicle sales that are plug-in hybrid electric (PHEV)
#'    in the final forecast year.
#'    Default is `0`.
#' @param .hev_pct_sales  numeric,   a value between `0` and `1.`
#'    Percent of all vehicle sales that are hybrid electric vehicles (HEV)
#'    in the final forecast year.
#'    Default is `0`.
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
adj_fleet_shares <- function(.pass_tb,
                             .freight_tb,
                             .selected_ctu = "all",
                             .bev_pct_sales = 0,
                             .phev_pct_sales = 0,
                             .hev_pct_sales = 0,
                             .vmt_fee = 0,
                             .payd_fee = 0,
                             .gas_tax = 0,
                             .elast = elast,
                             .enviro_factors = enviro_factors) {
  .pass_tb <- filter_ctu(.pass_tb, .selected_ctu = .selected_ctu) %>% unique()
  .freight_tb <- filter_ctu(.freight_tb, .selected_ctu = .selected_ctu) %>% unique()

  pass_tb <- .pass_tb
  freight_tb <- .freight_tb
  # check inputs -----
  l_names <- c(
    "bev_pct_sales",
    "hev_pct_sales",
    "phev_pct_sales",
    "vmt_fee",
    "payd_fee",
    "gas_tax"
  )

  l_vals <- list(
    .bev_pct_sales,
    .hev_pct_sales,
    .phev_pct_sales,
    .vmt_fee,
    .payd_fee,
    .gas_tax
  )

  purrr::map2(l_names, l_vals, check_inputs)

  if ((.bev_pct_sales + .hev_pct_sales + .phev_pct_sales) > 0.9) {
    cli::cli_warn("Proportion of alternate fuel vehicle sales will exceed 90% of all vehicle sales.")
  }


  # vmt, payd, gas -----
  # Adjust sales based on ownership response to price elasticity

  if (.vmt_fee > 0 | .payd_fee > 0 | .gas_tax > 0) {
    if (.vmt_fee > 0 & .payd_fee > 0) {
      cli::cli_abort("Implement a VMT fee OR a pay-as-you drive insurance fee, not both.")
    }

    # adjust sales of SI/CI vehicles based on per-mile fees
    adj_si_ci_sales <- tibble::tibble(
      year = .elast$year,
      adj_si_ci =
      # (1 + vmt_fee / car cost per mile) +
      # (payd_fee /car cost per mile) *
      # vehicle ownership elasticity over time *
      # 1 + (gas tax / car cost per mile) *
      # vehicle ownership elasticity over time
        (1 + (.vmt_fee / .enviro_factors$AUTO_COST_MI +
          (.payd_fee / .enviro_factors$AUTO_COST_MI)) *
          .elast$vehicle_ownership_elast) *
          (1 + (.gas_tax / .enviro_factors$AUTO_COST_MI) *
            .elast$vehicle_ownership_elast)
    )


    adj_si_ci_sales <- tibble(
      year = .elast$year,
      adj_si_ci = c(
        1, 1,
        seq(1, adj_si_ci_sales$adj_si_ci[8],
          by = -(1 - adj_si_ci_sales$adj_si_ci[8]) / 6
        )
      )
    )


    # Assume HEV, PHEV, and BEV not affected by .gas_tax price
    # because already switched stock type
    adj_alt_sales <- tibble::tibble(
      year = .elast$year,
      adj_alt =
        (1 + (.vmt_fee / .enviro_factors$AUTO_COST_MI +
          .payd_fee / .enviro_factors$AUTO_COST_MI) *
          .elast$vehicle_ownership_elast)
    )


    # adj_alt_sales <- tibble(
    #   year = .elast$year,
    #   adj_alt = c(
    #     1, 1,
    #     seq(1, adj_alt_sales$adj_alt[8],
    #       by = -(1 - adj_alt_sales$adj_alt[8]) / 6
    #     )
    #   )
    # )


    # adjust passenger existing stock
    # pass_alt_adj will have the same number of rows and same structure as .pass_tb
    # only the value is changed
    pass_tb <- pass_tb %>%
      dplyr::left_join(adj_alt_sales, by = c("year")) %>%
      dplyr::left_join(adj_si_ci_sales, by = c("year")) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(value = dplyr::case_when(
        (mode == "PLDV" & var == "BEVExist") ~ value * adj_alt,
        (mode == "PLDV" & var == "PHEVExist") ~ value * adj_alt,
        (mode == "PLDV" & var == "HEVExist") ~ value * adj_alt,
        (mode == "PLDV" & var == "SIExist") ~ value * adj_si_ci,
        (mode == "PLDV" & var == "CIExist") ~ value * adj_si_ci,
        TRUE ~ value
      )) %>%
      dplyr::select(names(.pass_tb))

    new_tot_exist <- pass_tb %>%
      dplyr::filter(
        str_detect(var, "Exist"),
        mode == "PLDV"
      ) %>%
      tidyr::pivot_wider(names_from = var, values_from = value) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        TotExist = BEVExist + PHEVExist + HEVExist + SIExist + CIExist,
        var = "TotExist",
        value = TotExist
      ) %>%
      dplyr::select(names(.pass_tb))

    pass_tb <- pass_tb %>%
      dplyr::anti_join(new_tot_exist, by = c(
        "year", "mode", "geog_name", "geog_id", "aeo_mode",
        "type", "var"
      )) %>%
      dplyr::bind_rows(new_tot_exist)



    if (nrow(.pass_tb) != nrow(pass_tb)) {
      cli::cli_abort("Passenger data did not pass VMT/PAYD and vehicle ownership elasticity adjustment")
    }
  }

  # hev/bev/phev  -----
  if (.bev_pct_sales > 0 | .phev_pct_sales > 0 | .hev_pct_sales > 0) {
    ## passenger-----

    # spread the final increase across intermediate years
    sales_elast <- tibble::tibble(
      year = unique(pass_tb$year),
      bev_elast =
        c(calc_elasticity(
          elas_list = c(rep(0, length(unique(pass_tb$year)))),
          elas = .bev_pct_sales,
          num_inits = 3,
          num_yrs = length(unique(pass_tb$year)) - 5
        )[1:7], .bev_pct_sales, .bev_pct_sales),
      hev_elast =
        c(calc_elasticity(
          elas_list = c(rep(0, length(unique(pass_tb$year)))),
          elas = .hev_pct_sales,
          num_inits = 3,
          num_yrs = length(unique(pass_tb$year)) - 5
        )[1:7], .hev_pct_sales, .hev_pct_sales),
      phev_elast =
        c(calc_elasticity(
          elas_list = c(rep(0, length(unique(pass_tb$year)))),
          elas = .phev_pct_sales,
          num_inits = 3,
          num_yrs = length(unique(pass_tb$year)) - 5
        )[1:7], .phev_pct_sales, .phev_pct_sales)
    ) %>%
      # create si/ci elasticity by subtracting the combined alternate fuel
      # vehicle percentages from 1
      mutate(
        si_ci_elast = 1 - (bev_elast + phev_elast + hev_elast),
        si_ci_elast = ifelse(year %in% c("2015", "2018", "2020"), 1, si_ci_elast)
      )


    ### sales ------

    # fetch passenger vehicle sales (all fuel types)
    pass_tb_sales <- pass_tb %>%
      dplyr::filter(
        year %in% c(
          "2025", "2030",
          "2035", "2040",
          "2045",
          "2050"
        ),
        mode == "PLDV",
        stringr::str_detect(var, "Sales")
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(value = dplyr::case_when(
        # Temporarily update any zero sales in final year to equal 1
        value == 0 ~ 1,
        TRUE ~ value
      )) %>%
      unique() %>%
      tidyr::pivot_wider(names_from = "var", values_from = "value")


    pass_tb_sales_w_fin <- pass_tb_sales %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::mutate(
        # for each fuel type, calculate the percentage
        # of total sales it makes up in the final year
        si_fin_year = SISales / TotSales,
        ci_fin_year = CISales / TotSales,
        hev_fin_year = HEVSales / TotSales,
        phev_fin_year = PHEVSales / TotSales,
        bev_fin_year = BEVSales / TotSales
      ) %>%
      dplyr::select(
        mode, geog_name, geog_id,
        aeo_mode, type,
        si_fin_year,
        ci_fin_year,
        hev_fin_year,
        phev_fin_year,
        bev_fin_year
      ) %>%
      dplyr::right_join(
        # join back with all years
        pass_tb_sales,
        by = c("mode", "geog_name", "geog_id", "aeo_mode", "type")
      ) %>%
      dplyr::arrange(geog_name, year) %>%
      dplyr::select(
        names(pass_tb_sales),
        si_fin_year,
        ci_fin_year,
        hev_fin_year,
        phev_fin_year,
        bev_fin_year
      )

    # the total proportion of SI and CI Sales in the final year
    all_sales_pcts <- as.numeric(1 - (.bev_pct_sales + .phev_pct_sales + .hev_pct_sales))

    # SI: (portion of SI and CI sales less BEV, PHEV, HEV) *( (SI portion of SI+CI sales) * (SI portion of total sales) )/ (si portion of total sales in the final year)
    # HEV: (.hev_pct_sales) * (HEV portion of total sales) / (HEV portion of total sales in final year)
    # $$

    # SIportion_{y} = (1 - %AltFuel_{y}) \times \frac{ \left(  \frac{SISales_{y}}{SISales_{y} + CISales_{y}} \times \frac{SISales_{y}}{TotalSales_{y}}\right)}{{\frac{SISales_{y = 2050}}{TotalSales_{y=2050}} }}


    # HEVportion_{y} = %HEV_{y} \times\frac{\frac{HEVSales_{y}}{TotalSales_{y}}}{ \frac{HEVSales_{y=2050}}{TotalSales_{y=2050}}}


    # change as change in proportion-----
    # this calculates the change as a percent change on the proportion
    # so, by 2050, BEV Sales will increase by bev_pct_sales beyond the current
    # forecast
    pass_sales_portions <- pass_tb_sales_w_fin %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        ptb_si = all_sales_pcts * (SISales / (SISales + CISales)) * (SISales / TotSales),
        ptb_ci = all_sales_pcts * (CISales / (CISales + SISales)) * (CISales / TotSales),
        ptb_hev = (.hev_pct_sales) * (HEVSales / TotSales),
        ptb_phev = (.phev_pct_sales) * (PHEVSales / TotSales),
        ptb_bev = (.bev_pct_sales) * (BEVSales / TotSales),
        # total sales of each type
        ptb_tot_sales =
          ptb_si +
            ptb_ci +
            ptb_hev +
            ptb_phev +
            ptb_bev
      ) %>%
      dplyr::mutate(
        # new portion of total sales for each type
        new_si_portion = ptb_si / ptb_tot_sales,
        new_ci_portion = ptb_ci / ptb_tot_sales,
        new_hev_portion = ptb_hev / ptb_tot_sales,
        new_phev_portion = ptb_phev / ptb_tot_sales,
        new_bev_portion = ptb_bev / ptb_tot_sales
      ) %>%
      dplyr::mutate(
        sum_check =
          new_si_portion +
            new_ci_portion +
            new_bev_portion +
            new_hev_portion +
            new_phev_portion
      ) %>%
      unique()

    # change as pct -----
    # this calculates the change as a straight percent of each fuel type
    # so, by 2050, BEV Sales be bev_sales_pct of all vehicle sales
    # forecast
    pass_sales_portions_alt <- pass_tb_sales_w_fin %>%
      dplyr::rowwise() %>%
      dplyr::left_join(sales_elast, by = "year") %>%
      rowwise() %>%
      dplyr::mutate(
        ptb_si = si_ci_elast * (SISales / (SISales + CISales)), # * (SISales / TotSales),
        ptb_ci = si_ci_elast * (CISales / (CISales + SISales)), # * (CISales / TotSales),
        ptb_hev = hev_elast, # (hev_elast) * (HEVSales / TotSales),
        ptb_phev = phev_elast, # (phev_elast) * (PHEVSales / TotSales),
        ptb_bev = bev_elast, # (bev_elast) * (BEVSales / TotSales),
        # total sales of each type
        ptb_tot_sales =
          ptb_si +
            ptb_ci +
            ptb_hev +
            ptb_phev +
            ptb_bev
      ) %>%
      dplyr::mutate(
        # new portion of total sales for each type
        new_si_portion = ptb_si / ptb_tot_sales,
        new_ci_portion = ptb_ci / ptb_tot_sales,
        new_hev_portion = ptb_hev / ptb_tot_sales,
        new_phev_portion = ptb_phev / ptb_tot_sales,
        new_bev_portion = ptb_bev / ptb_tot_sales
      ) %>%
      dplyr::mutate(
        sum_check =
          new_si_portion +
            new_ci_portion +
            new_bev_portion +
            new_hev_portion +
            new_phev_portion
      ) %>%
      unique()

    ### apply portions to get actual number of vehicles sold -----
    ptb_sales_new <- pass_tb %>%
      dplyr::filter(
        # year %in% c("2025", "2030", "2035", "2040", "2045", "2050"),
        mode == "PLDV",
        stringr::str_detect(var, "Sales") | stringr::str_detect(var, "Exist")
      ) %>%
      dplyr::left_join(
        pass_sales_portions_alt %>%
          dplyr::select(
            mode, geog_name, geog_id, year, aeo_mode, type,
            TotSales,
            new_si_portion,
            new_ci_portion,
            new_bev_portion,
            new_hev_portion,
            new_phev_portion
          ) %>%
          unique(),
        by = c("mode", "geog_name", "geog_id", "year", "aeo_mode", "type")
      ) %>%
      # rowwise() %>%
      dplyr::mutate(new_val = dplyr::case_when(
        # calculate new absolute sales value for each
        !year %in% c("2015", "2018", "2020") & var == "SISales" ~ TotSales * new_si_portion,
        !year %in% c("2015", "2018", "2020") & var == "CISales" ~ TotSales * new_ci_portion,
        !year %in% c("2015", "2018", "2020") & var == "HEVSales" ~ TotSales * new_hev_portion,
        !year %in% c("2015", "2018", "2020") & var == "PHEVSales" ~ TotSales * new_phev_portion,
        !year %in% c("2015", "2018", "2020") & var == "BEVSales" ~ TotSales * new_bev_portion,
        TRUE ~ value
      )) %>%
      dplyr::select(mode, var,
        geog_name, geog_id, year,
        value = new_val,
        aeo_mode, type
      )


    # New existing stock is
    # old ratio x (exist_old+sales_new - 5yrs)/(exist_old+sales_old - 5 yrs) in each year


    ### existing is -----
    # get OLD (previous year) sales and existing values
    pass_exist_old <- pass_tb %>%
      dplyr::filter(
        mode == "PLDV",
        stringr::str_detect(var, "Sales") | stringr::str_detect(var, "Exist")
      ) %>%
      unique() %>%
      dplyr::arrange(year) %>%
      dplyr::group_by(var, geog_name, geog_id, mode, aeo_mode, type) %>%
      dplyr::mutate(
        var = paste0(var, ".old"),
        # lag by one year increment
        # if base year, keep that value (rather than NA)
        value = dplyr::lag(value,
          n = 1,
          default = value[1]
        )
      ) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(
        names_from = var,
        values_from = value
      )

    ptb_exist_new <- ptb_sales_new %>%
      dplyr::filter(
        mode == "PLDV",
        stringr::str_detect(var, "Sales") | stringr::str_detect(var, "Exist")
      ) %>%
      unique() %>%
      tidyr::pivot_wider(
        values_from = value,
        names_from = var
      ) %>%
      dplyr::left_join(
        # OLD sales, existing
        pass_exist_old,
        by = c("mode", "geog_name", "geog_id", "year", "aeo_mode", "type")
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        # (previous existing +  new sales) / (previous exist + previous sales)
        si_new_adj = ifelse(year != max(year), (SIExist.old + SISales) / (SIExist.old + SISales.old), 1),
        ci_new_adj = ifelse(year != max(year), (CIExist.old + CISales) / (CIExist.old + CISales.old), 1),
        hev_new_adj = ifelse(year != max(year), (HEVExist.old + HEVSales) / (HEVExist.old + HEVSales.old), 1),
        phev_new_adj = ifelse(year != max(year), (PHEVExist.old + PHEVSales) / (PHEVExist.old + PHEVSales.old), 1),
        bev_new_adj = ifelse(year != max(year), (BEVExist.old + BEVSales) / (BEVExist.old + BEVSales.old), 1)
      ) %>%
      dplyr::select(
        mode,
        geog_name, geog_id,
        year, aeo_mode, type,
        si_new_adj,
        ci_new_adj,
        hev_new_adj,
        phev_new_adj,
        bev_new_adj
      ) %>%
      dplyr::right_join(
        pass_tb %>%
          dplyr::filter(
            mode == "PLDV",
            stringr::str_detect(var, "Exist")
          ),
        by = c("mode", "geog_name", "geog_id", "year", "aeo_mode", "type")
      ) %>%
      dplyr::mutate(
        value = dplyr::case_when(
          # calculate absolute existing values
          (!year %in% c("2015", "2018", "2020") & mode == "PLDV" & var == "BEVExist") ~ value * bev_new_adj,
          (!year %in% c("2015", "2018", "2020") & mode == "PLDV" & var == "PHEVExist") ~ value * phev_new_adj,
          (!year %in% c("2015", "2018", "2020") & mode == "PLDV" & var == "HEVExist") ~ value * hev_new_adj,
          (!year %in% c("2015", "2018", "2020") & mode == "PLDV" & var == "SIExist") ~ value * si_new_adj,
          (!year %in% c("2015", "2018", "2020") & mode == "PLDV" & var == "CIExist") ~ value * ci_new_adj,
          TRUE ~ value
        )
      ) %>%
      dplyr::select(names(.pass_tb))


    ### stock -----

    ptb_stock_new <- ptb_exist_new %>%
      dplyr::bind_rows(ptb_sales_new %>%
        dplyr::filter(stringr::str_detect(var, "Exist", negate = T))) %>%
      dplyr::bind_rows(pass_tb %>%
        dplyr::filter(
          # year %in% c("2025", "2030", "2035", "2040", "2045",
          # "2050"),
          mode == "PLDV",
          stringr::str_detect(var, "Stock"),
          var != "TotStock"
        )) %>%
      unique() %>%
      tidyr::pivot_wider(
        names_from = var,
        values_from = value
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        # Stock  = Existing + Sales
        BEVStock = BEVExist + BEVSales,
        PHEVStock = PHEVExist + PHEVSales,
        HEVStock = HEVExist + HEVSales,
        CIStock = CIExist + CISales,
        SIStock = SIExist + SISales,
        TotStock = BEVStock + PHEVStock + HEVStock + CIStock + SIStock,
        TotExist = BEVExist + PHEVExist + HEVExist + CIExist + SIExist,
        TotSales = BEVSales + PHEVSales + HEVSales + CISales + SISales
      ) %>%
      # tidyr::pivot_longer(cols = 6:23,
      #              names_to = "var",
      #              values_to = "value") %>%
      unique()
    # Switch update any zero sales in final year to equal 1 back to 0
    # mutate(value = dplyr::case_when(stringr::str_detect(var, "Sales") & value == 1 ~ 0,
    # TRUE ~ value))


    ptb_new <- ptb_stock_new %>%
      tidyr::pivot_longer(
        cols = c(
          "TotExist", "SIExist", "CIExist", "HEVExist", "PHEVExist", "BEVExist",
          "TotSales", "SISales", "CISales", "HEVSales", "PHEVSales", "BEVSales",
          "SIStock", "CIStock", "HEVStock", "PHEVStock", "BEVStock", "TotStock"
        ),
        names_to = "var",
        values_to = "value"
      ) %>%
      unique() %>%
      dplyr::mutate(
        value =
          dplyr::case_when(
            stringr::str_detect(var, "Sales") & value == 1 ~ 0,
            TRUE ~ value
          ),
        all_combos = paste(mode, geog_name, geog_id, year, var, aeo_mode,
          sep = "-"
        )
      ) %>%
      unique()

    ptb_new <- pass_tb %>%
      unique() %>%
      dplyr::mutate(all_combos = paste(mode, geog_name, geog_id, year, var, aeo_mode,
        sep = "-"
      )) %>%
      dplyr::filter(!all_combos %in% ptb_new$all_combos) %>%
      dplyr::bind_rows(ptb_new) %>%
      dplyr::select(names(.pass_tb))

    if (nrow(ptb_new) != nrow(.pass_tb)) {
      cli::cli_abort("Passenger data did not pass HEV/PHEV/BEV adjustment")
    }



    test_passenger <- purrr::map(
      c(
        "TotSales",
        "TotStock",
        "TotExist"
      ),
      function(x) {
        all.equal(
          pass_tb %>%
            filter(
              mode == "PLDV",
              var == x
            ),
          ptb_new %>%
            filter(
              mode == "PLDV",
              var == x
            ),
          tolerance = 0.01
        )
      }
    )

    if (any(test_passenger != TRUE)) {
      cli::cli_abort("Passenger CTU totals failed")
    }


    # freight --------
    # BAU assumes 1/3 and 2/3 change (relative to PLDV in 2025-2040) to
    #  freight sales to include BEV (as summation of BEV+PHEV+HEV from PLDV)
    #  for SUT and CUT, respectively
    #
    # We do not have good stock numbers on freight so we do NOT consider
    # the embodied emissions from freight and the shift in sales,
    # etc. so  the passenger fleet is translated into a
    # total stock number for freight. The use of 1/3 and 2/3
    # helps to account for this being sales
    # not total stock (i.e., should be lower as percent of total stock)


    freight_battery_fin_year <- pass_sales_portions_alt %>%
      dplyr::ungroup() %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::mutate(bev_pcts_fin = .bev_pct_sales) %>% # * (BEVSales / BEVStock)) %>%
      dplyr::select(mode, geog_name, geog_id, bev_pcts_fin) %>%
      unique()


    freight_battery <- pass_sales_portions_alt %>%
      dplyr::mutate(bev_pcts = bev_elast) %>% # * (BEVSales / BEVStock)) %>%
      dplyr::select(geog_name, geog_id, year, bev_pcts)


    ### stock ----
    # Temporarily update any zero stock in final year to equal 1
    freight_stock <- freight_tb %>%
      dplyr::filter(
        mode %in% c(
          "SUT",
          "CUT"
        ),
        stringr::str_detect(var, "Stock")
      ) %>%
      dplyr::mutate(value = dplyr::case_when(
        ((mode == "SUT" | mode == "CUT") &
          stringr::str_detect(var, "Stock") & value == 0) ~ 1,
        TRUE ~ value
      )) %>%
      unique() %>%
      dplyr::select(-aeo_mode) %>%
      tidyr::pivot_wider(
        names_from = c(var, mode),
        values_from = value
      ) %>%
      dplyr::group_by(year, geog_name, geog_id) %>%
      dplyr::mutate(dplyr::across(4:7, function(x) {
        sum(x, na.rm = TRUE)
      })) %>%
      unique()



    freight_stock_fin_year <- freight_stock %>%
      dplyr::ungroup() %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        ci_sut_fin = CIStock_SUT / TotStock_SUT,
        bev_sut_fin = BEVStock_SUT / TotStock_SUT,
        ci_cut_fin = CIStock_CUT / TotStock_CUT,
        bev_cut_fin = BEVStock_CUT / TotStock_CUT
      ) %>%
      dplyr::select(
        geog_name, geog_id,
        ci_sut_fin,
        bev_sut_fin,
        ci_cut_fin,
        bev_cut_fin
      ) %>%
      unique() %>%
      filter(!is.na(ci_cut_fin))


    freight_stock_new <- freight_stock %>%
      dplyr::left_join(freight_stock_fin_year, by = c("geog_name", "geog_id")) %>%
      dplyr::left_join(freight_battery, by = c("geog_name", "geog_id", "year")) %>%
      dplyr::left_join(freight_battery_fin_year, by = c("geog_name", "geog_id")) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        ci_stock_sut = ((1 / 3) * bev_pcts) *
          (CIStock_SUT / TotStock_SUT) /
          ci_sut_fin,
        ci_stock_cut = ((2 / 3) * bev_pcts) *
          (CIStock_CUT / TotStock_CUT) /
          ci_cut_fin,
        bev_stock_sut = ((2 / 3) * bev_pcts) *
          (BEVStock_SUT / TotStock_SUT) /
          bev_sut_fin,
        bev_stock_cut = ((1 / 3) * bev_pcts) *
          (BEVStock_CUT / TotStock_CUT) /
          bev_cut_fin
      ) %>%
      dplyr::mutate(
        sut_tot_stock = ci_stock_sut + bev_stock_sut,
        cut_tot_stock = ci_stock_cut + bev_stock_cut,
        bev_sut = bev_stock_sut / sut_tot_stock,
        bev_cut = bev_stock_cut / cut_tot_stock,
        ci_sut = ci_stock_sut / sut_tot_stock,
        ci_cut = ci_stock_cut / cut_tot_stock
      ) %>%
      dplyr::select(
        year, geog_name, geog_id, bev_sut, bev_cut,
        ci_sut, ci_cut
      ) %>%
      unique()


    freight_tot_stock <- freight_tb %>%
      dplyr::ungroup() %>%
      dplyr::filter(
        var == "TotStock",
        mode %in% c(
          "SUT",
          "CUT"
        )
      ) %>%
      unique() %>%
      tidyr::pivot_wider(
        names_from = var,
        values_from = value
      ) %>%
      dplyr::select(year, geog_name, geog_id, mode, TotStock) %>%
      unique()


    ftb_new_stocks <- freight_tb %>%
      dplyr::filter(mode %in% c(
        "SUT",
        "CUT"
      )) %>%
      dplyr::left_join(freight_stock_new, by = c("geog_name", "geog_id", "year")) %>%
      unique() %>%
      tidyr::pivot_wider(
        names_from = var,
        values_from = value
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        BEVStock = dplyr::case_when(
          !year %in% c("2015", "2018", "2020") & mode == "SUT" ~ TotStock * bev_sut,
          !year %in% c("2015", "2018", "2020") & mode == "CUT" ~ TotStock * bev_cut
        ),
        CIStock = dplyr::case_when(
          !year %in% c("2015", "2018", "2020") & mode == "SUT" ~ TotStock * ci_sut,
          !year %in% c("2015", "2018", "2020") & mode == "CUT" ~ TotStock * ci_cut
        )
      ) %>%
      dplyr::mutate(TotStock = BEVStock + CIStock) %>%
      tidyr::pivot_longer(
        cols = c(
          "TMT", "TotStock",
          "CIStock", "BEVStock"
        ),
        names_to = "var",
        values_to = "value",
        values_drop_na = TRUE
      ) %>%
      unique() %>%
      dplyr::select(names(freight_tb)) %>%
      dplyr::mutate(all_combos = paste(mode, geog_name, geog_id, year, var, aeo_mode,
        sep = "-"
      ))


    ftb_new <- freight_tb %>%
      dplyr::mutate(all_combos = paste(mode, geog_name, geog_id, year, var, aeo_mode,
        sep = "-"
      )) %>%
      dplyr::filter(!all_combos %in% ftb_new_stocks$all_combos) %>%
      dplyr::bind_rows(ftb_new_stocks) %>%
      dplyr::select(names(freight_tb))

    if (nrow(.freight_tb) != nrow(ftb_new)) {
      cli::cli_abort("Freight data did not pass adjustment")
    }



    test_freight <- purrr::map(
      c(
        "TotSales",
        "TotStock",
        "TotExist"
      ),
      function(x) {
        all.equal(
          ftb_new %>%
            filter(
              var == x
            ),
          ftb_new %>%
            filter(
              var == x
            ),
          tolerance = 0.01
        )
      }
    )

    if (any(test_freight == FALSE)) {
      cli::cli_abort("Freight CTU totals failed")
    }
  } else {
    ptb_new <- pass_tb
    ftb_new <- freight_tb
  }

  # return-----
  return(list(pass = ptb_new, freight = ftb_new))
}
