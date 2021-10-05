#' @title Adjust Fleet
#'
#' @description  Match what the user input for sales in 2050 rather
#'     than the defaults from MA3TFleet held fixed in all cases.
#'     AV/DRS scenarios adjust the sales
#'     total up, but they adjust the existing stock down to match total stock
#'     in each year.
#'
#'     Jason has a model used to forecast vehicle fleet shares between powertrains.
#'     Distribute personal vehicle VMT between the different powertrains.
#'     Ratio of vehicle stocks by powertrain type.
#'     If the user inputs a different percent for BEV, update vehicles by existing
#'     vehicles and new sales
#'     DRS will take away from VMT for passenger vehicles.
#'     Run this before anything else
#'
#' @param .bev_pct_sales percent of sales that are battery electric vehicles (BEV) in 2050
#' @param .phev_pct_sales percent of sales that are plug-in hybrid electric (PHEV) in 2050
#' @param .hev_pct_sales percent of sales that are hybrid electric vehicles (HEV) in 2050
#' @param .pass_tb passenger input table. Default is `transportation_data$passenger`.
#' @param .freight_tb freight input table. Default is `transportation_data$freight`.
#' @param .drs_pct_trip percent of trips/fleet that is dynamic ride sharing (DRS) Default is `0`.
#' @param .ctu the chosen CTU for dplyr::filter of tables
#' @inheritParams calc_vmt_forecast
#'
#' @family transportation
#' @return
#' @export
#'
#' @importFrom tidyselect all_of
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom dplyr filter select case_when mutate across summarise group_by ungroup cur_column
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom purrr map2
#'
adj_fleet_shares <- function(.bev_pct_sales,
                             .phev_pct_sales,
                             .hev_pct_sales,
                             .pass_tb = transportation_data$passenger,
                             .freight_tb = transportation_data$freight,
                             .vmt_fee = 0,
                             .payd_fee = 0,
                             .gas_tax = 0,
                             .drs_pct_trip = 0,
                             .av_pct = 0,
                             .ctu,
                             .enviro_factors = enviro_factors) {

  # check inputs -----
  l_names <- c(
    "bev_pct_sales",
    "hev_pct_sales",
    "phev_pct_sales",
    "drs_pct_trip"
  )

  l_vals <- list(
    .bev_pct_sales,
    .hev_pct_sales,
    .phev_pct_sales,
    .drs_pct_trip
  )

  purrr::map2(l_names, l_vals, check_inputs)

  if (sum(.bev_pct_sales, .hev_pct_sales, .phev_pct_sales) > 90) {
    stop("Values will not add to less than 90 for BEV, PHEV, and HEV percent sales.")
  }

  browser()
  # calculation -----
  # Adjust sales based on ownership response to price elasticity
  adj_si_ci_sales <- (1 + (.vmt_fee / .enviro_factors$AUTO_COST_MI +
                             .payd_fee / .enviro_factors$AUTO_COST_MI) *
                        elast$vehicle_ownership_elast) *
    (1 + (.gas_tax / .enviro_factors$AUTO_COST_MI) * elast$vehicle_ownership_elast)

  # Assume HEV, PHEV, and BEV not affected by .gas_tax price because already switched stock type
  adj_alt_sales <- tibble::tibble(year = elast$year,
                   adj_alt = (1 + (.vmt_fee / .enviro_factors$AUTO_COST_MI + .payd_fee /
                                     .enviro_factors$AUTO_COST_MI) * elast$vehicle_ownership_elast))

  pass_adj <- .pass_tb %>%
    left_join(adj_alt_sales, by = c("year")) %>%
    dplyr::mutate(mul = dplyr::case_when(
      (mode == "PLDV" & var == "BEVExist") ~ value * adj_alt,
      (mode == "PLDV" & var == "PHEVExist") ~ value * adj_alt,
      (mode == "PLDV" & var == "HEVExist") ~ value * adj_alt,
      (mode == "PLDV" & var == "SIExist") ~ value * adj_alt,
      (mode == "PLDV" & var == "CIExist") ~ value * adj_alt,
      TRUE ~ value
    ))

  # drs-----
  # DRS adjustment of all Sales, Existing, and Stock in each year regardless of passenger mode
  if (.drs_pct_trip > 0) {
    .pass_tb <- .pass_tb %>%
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(FOR_YRS), ~ dplyr::case_when(
            ((stringr::str_detect(var, "Sales")) |
               (stringr::str_detect(var, "Exist")) |
               (stringr::str_detect(var, "Stock"))) ~ value *
              (1 - .pass_tb %>% dplyr::filter(var == "DRSShare") %>%
                 dplyr::select(tidyselect::all_of(FOR_YRS)) %>%
                 as.numeric() * .drs_pct_trip / 100),
            TRUE ~ value
          )
        )
      )
  }

  # av -----
  # AV adjustment of all Sales, Existing, and Stock in each year regardless of passenger mode
  # Non-AV portion continues as before and AV treated separately
  if (.av_pct > 0) {
    # Add a row for stock to pivot AV analysis off
    av_stock <- tibble::tibble(
      mode = "AV", var = "AVStock",
      ctu = .ctu, .pass_tb %>%
        dplyr::filter(
          mode == "PLDV",
          var == "TotStock"
        ) %>%
        dplyr::select(tidyselect::all_of(YRS))
    )

    av_stock <- av_stock %>%
      dplyr::mutate(dplyr::across(setdiff(YRS, FOR_YRS), ~0))

    .pass_tb <- dplyr::bind_rows(.pass_tb, av_stock)

    .pass_tb <- .pass_tb %>%
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(
            FOR_YRS
          ), ~ dplyr::case_when(
            ((stringr::str_detect(var, "Sales") |
                stringr::str_detect(var, "Exist") |
                stringr::str_detect(var, "Stock")) & mode == "PLDV") ~ value *
              (1 - .pass_tb %>%
                 dplyr::filter(var == "AVShare") %>%
                 dplyr::select(tidyselect::all_of(FOR_YRS)) %>%
                 as.numeric() * .av_pct / 100), # Remove AV from non-AV stock
            TRUE ~ value
          )
        )
      )
  }

  if (.bev_pct_sales > 0 | .phev_pct_sales > 0 | .hev_pct_sales > 0) {
    # Temporarily update any zero sales in final year to equal 1
    pass_tb_1 <- .pass_tb %>%
      filter(
        mode == "PLDV",
        stringr::str_detect(var, "Sales")
      ) %>%
      rowwise() %>%
      mutate(value = dplyr::case_when(
        value == 0 ~ 1,
        TRUE ~ value
      ))


    pass_tb_sales <- pass_tb_1 %>%
      filter(
        mode == "PLDV",
        stringr::str_detect(var, "Sales")
      ) %>%
      unique() %>%
      pivot_wider(names_from = "var", values_from = "value")


    pass_tb_sales_w_fin <- pass_tb_sales %>%
      filter(year == max(year)) %>%
      mutate(
        si_fin_year = SISales / TotSales,
        ci_fin_year = CISales / TotSales,
        hev_fin_year = HEVSales / TotSales,
        phev_fin_year = PHEVSales / TotSales,
        bev_fin_year = BEVSales / TotSales
      ) %>%
      select(
        mode, ctu, aeo_mode, type,
        si_fin_year,
        ci_fin_year,
        hev_fin_year,
        phev_fin_year,
        bev_fin_year
      ) %>%
      right_join(pass_tb_sales) %>%
      arrange(ctu, year) %>%
      select(
        names(pass_tb_sales),
        si_fin_year,
        ci_fin_year,
        hev_fin_year,
        phev_fin_year,
        bev_fin_year
      )


    all_pcts <- as.numeric(100 - .bev_pct_sales - .phev_pct_sales - .hev_pct_sales)

    pass_adj_tab <- pass_tb_sales_w_fin %>%
      rowwise() %>%
      mutate(
        ptb_si = all_pcts / 100 * (SISales / SISales + CISales) * (SISales / TotSales) / si_fin_year,
        ptb_ci = all_pcts / 100 * (CISales / CISales + SISales) * (CISales / TotSales) / ci_fin_year,
        ptb_hev = (.hev_pct_sales / 100) * (HEVSales / TotSales) / hev_fin_year,
        ptb_phev = (.phev_pct_sales / 100) * (PHEVSales / TotSales) / phev_fin_year,
        ptb_bev = (.bev_pct_sales / 100) * (BEVSales / TotSales) / bev_fin_year
      ) %>%
      mutate(
        ptb_tot_sales = sum(
          ptb_si,
          ptb_ci,
          ptb_hev,
          ptb_phev,
          ptb_bev
        ),
        new_si = ptb_si / ptb_tot_sales,
        new_ci = ptb_ci / ptb_tot_sales,
        new_hev = ptb_hev / ptb_tot_sales,
        new_phev = ptb_phev / ptb_tot_sales,
        new_bev = ptb_bev / ptb_tot_sales
      ) %>%
      mutate(sum_check = sum(
        new_si,
        new_ci,
        new_bev,
        new_hev,
        new_phev
      ))



    # adjust sales -----
   ptb_sales_new <-  .pass_tb %>%
      filter(
        mode == "PLDV",
        stringr::str_detect(var, "Sales")
      ) %>%
      left_join(pass_adj_tab %>%
                  select(
                    mode, ctu, year, aeo_mode, type,
                    new_si,
                    new_ci,
                    new_bev,
                    new_hev,
                    new_phev
                  )) %>%
      mutate(new_val = case_when(
        var == "BEVSales" ~ value * new_bev,
        var == "CISales" ~ value * new_ci,
        var == "SISales" ~ value * new_si,
        var == "PHEVSales" ~ value * new_phev,
        var == "HEVSales" ~ value * new_hev,
        TRUE ~ value
      )) %>%
     select(mode, var,
            ctu, year,
            value = new_val,
            aeo_mode, type)

  } else {
    ptb_sales_new <- .pass_tb
  }

  ftb_new <- .freight_tb

  # New existing stock is old ratio x (exist_old+sales_new - 5yrs)/(exist_old+sales_old - 5 yrs) in each year

  # existing stock -----
 ptb_exist_new <-  .pass_tb %>%
    filter(
      mode == "PLDV",
      stringr::str_detect(var, "Exist")) %>%
    unique() %>%
    pivot_wider(values_from = value,
                names_from = var) %>%
    left_join(ptb_sales_new %>%
                unique() %>%
                pivot_wider(values_from = value,
                            names_from = var),
              by = c("mode", "ctu", "year", "aeo_mode", "type")) %>%
    left_join(.pass_tb %>%
                filter(
                  mode == "PLDV",
                  stringr::str_detect(var, "Sales") | stringr::str_detect(var, "Exist")
                ) %>%
                unique() %>%
                pivot_wider(names_from = var,
                            values_from = value),
              suffix = c("", ".orig"),
              by = c("mode", "ctu", "year", "aeo_mode", "type")
    ) %>%
    rowwise() %>%
    mutate(si_new_adj = SIExist + SISales / (SIExist.orig + SISales.orig),
           ci_new_adj = CIExist +  CISales / (CIExist.orig + CISales.orig),
           hev_new_adj = HEVExist +  HEVSales / (HEVExist.orig + HEVSales.orig),
           phev_new_adj = PHEVExist + PHEVSales / (PHEVExist.orig + PHEVSales.orig),
           bev_new_adj = BEVExist + BEVSales / (BEVExist.orig + BEVSales.orig)) %>%
    select(mode, ctu, year, aeo_mode, type,
           si_new_adj,
           ci_new_adj,
           hev_new_adj,
           phev_new_adj,
           bev_new_adj) %>%
    right_join(.pass_tb %>%
                 filter(mode == "PLDV",
                        stringr::str_detect(var, "Exist") | stringr::str_detect(var, "Stock")),
               by = c("mode", "ctu", "year", "aeo_mode", "type")) %>%
    mutate(value = case_when(
      (var == "BEVExist") ~ value * bev_new_adj,
      (var == "PHEVExist") ~ value * phev_new_adj,
      (var == "HEVExist") ~ value * hev_new_adj,
      (var == "SIExist") ~ value * si_new_adj,
      (var == "CIExist") ~ value * ci_new_adj,
      TRUE ~ value
    )) %>%
   select(names(.pass_tb))


  # total stock -----

 ptb_stock_new <-  ptb_exist_new %>%
    filter(mode == "PLDV",
           stringr::str_detect(var, "Exist") | stringr::str_detect(var, "Stock")) %>%
    bind_rows(ptb_sales_new) %>%
    unique() %>%
    pivot_wider(names_from = var,
                values_from = value) %>%
    rowwise() %>%
    mutate(
      BEVStock = BEVExist + BEVStock,
      PHEVStock = PHEVExist + PHEVStock,
      HEVStock = HEVExist + HEVStock,
      CIStock = CIExist + CIStock,
      SIStock = SIExist + SIStock,
      TotStock = BEVStock + PHEVStock + HEVStock + CIStock + SIStock,
      TotExist = BEVExist + PHEVExist + HEVExist + CIExist + SIExist,
      TotSales = BEVSales + PHEVSales + HEVSales + CISales + SISales
    ) %>%
    pivot_longer(cols = 6:23,
                 names_to = "var",
                 values_to = "value") %>%
    # Switch update any zero sales in final year to equal 1 back to 0
    mutate(value = case_when(stringr::str_detect(var, "Sales") & value == 1 ~ 0,
                             TRUE ~ value))

  if (.bev_pct_sales > 0 | .phev_pct_sales > 0 | .hev_pct_sales > 0) {
    # freight -----
    # FREIGHT - BAU assumes 1/3 and 2/3 change (relative to PLDV in 2025-2040) to freight sales to include BEV (as summation of BEV+PHEV+HEV from PLDV) for SUT and CUT, respectively
    # We do not have good stock numbers on freight so we do not consider the embodied emissions from freight and the shift in sales, etc. from the passenger
    # fleet is translated into a total stock number for freight. The use of 1/3 and 2/3 helps to account for this being sales not total stock (i.e., should be lower as percent of total stock)
    fbev <- .bev_pct_sales * ptb_new %>%
      dplyr::filter(mode == "PLDV", var == "BEVSales") %>%
      dplyr::select(FIN_YR) /
      ptb_new %>%
      dplyr::filter(mode == "PLDV", var == "BEVStock") %>%
      dplyr::select(FIN_YR)

    freight_bev <- ptb_stock_new %>%
      filter(var %in% c("BEVSales", "BEVStock")) %>%
      unique() %>%
      pivot_wider(names_from = var,
                  values_from = value) %>%
      mutate(bev_pcts = .bev_pct_sales * BEVSales/BEVStock) %>%
      select(ctu, year, bev_pcts)


    # Temporarily update any zero stock in final year to equal 1
    freight_tb_1 <- .freight_tb %>%
      dplyr::mutate(value =  dplyr::case_when(
          ((mode == "SUT" | mode == "CUT") &
             stringr::str_detect(var, "Stock") & value == 0) ~ 1,
          TRUE ~ value
        )
      )

    freight_sut_cut <- freight_tb_1 %>%
      filter(mode %in% c("SUT",
                         "CUT"),
             stringr::str_detect(var, "Stock")) %>%
      unique() %>%
      pivot_wider(names_from = c(var, mode),
                  values_from = value) %>%
      group_by(year, ctu) %>%
      mutate(across(3:8, sum, na.rm = T)) %>%
      unique()



    freight_fin_year <- freight_sut_cut %>%
      ungroup() %>%
      filter(year == max(year)) %>%
      rowwise() %>%
      mutate(ci_sut_fin = CIStock_SUT/TotStock_SUT,
             bev_sut_fin = BEVStock_SUT/TotStock_SUT,
             ci_cut_fin = CIStock_CUT/TotStock_CUT,
             bev_cut_fin = BEVStock_CUT/TotStock_CUT) %>%
      select(ctu,
             ci_sut_fin,
             bev_sut_fin,
             ci_cut_fin,
             bev_cut_fin) %>%
      unique()
      # group_by(year, ctu) %>%
      # mutate(across(1:4, sum, na.rm = T)) %>%
      # unique()


    freight_sut_cut %>%
      left_join(freight_bev) %>%
      left_join(freight_fin_year) %>%
      rowwise() %>%
      mutate(ci_stock_sut = ((100 - 2/3 * bev_pcts)/100) *
               (CIStock_SUT/TotStock_SUT) /
               ci_sut_fin,
             ci_stock_cut = ((100 - 1/3 * bev_pcts)/100) *
               (CIStock_CUT/TotStock_CUT)/
               ci_cut_fin,
             bev_stock_sut = (2/3 * bev_pcts/100) *
               (BEVStock_SUT/TotStock_SUT)/
               bev_sut_fin,
             bev_stock_cut = ((1/3 * bev_pcts)/100) *
               (BEVStock_CUT/TotStock_CUT)/
               bev_cut_fin
             ) %>%
      mutate(sut_tot_stock = ci_stock_sut + bev_stock_sut,
             cut_tot_stock = ci_stock_cut + bev_stock_cut,

             bev_sut = bev_stock_sut/sut_tot_stock,
             bev_cut = bev_stock_cut/cut_tot_stock,

             ci_sut = ci_stock_sut/sut_tot_stock,
             ci_cut = ci_stock_cut/cut_tot_stock) %>%
      select(year, ctu, bev_sut, bev_cut,
             ci_sut, ci_cut) %>%
      unique()


    # dplyr::filter out the ratios in the BAU and compare with user input for alternative scenario




    # Readjust sales totals to sum to 100% in each year
    ftb_tot_stock_sut <- ftb_ci_stock_sut + ftb_bev_stock_sut
    ftb_ci_stock_sut <- ftb_ci_stock_sut / ftb_tot_stock_sut
    ftb_bev_stock_sut <- ftb_bev_stock_sut / ftb_tot_stock_sut

    ftb_tot_stock_cut <- ftb_ci_stock_cut + ftb_bev_stock_cut
    ftb_ci_stock_cut <- ftb_ci_stock_cut / ftb_tot_stock_cut
    ftb_bev_stock_cut <- ftb_bev_stock_cut / ftb_tot_stock_cut

    # Updated stock distribution
    ftb_new <- .freight_tb %>% dplyr::mutate(
      dplyr::across(tidyselect::all_of(
        FOR_YRS
      ), ~ dplyr::case_when(
        (mode == "SUT" & var == "BEVStock") ~
          as.numeric(.freight_tb %>%
                       dplyr::filter(
                         mode == "SUT",
                         var == "TotStock"
                       ) %>%
                       dplyr::select(dplyr::cur_column()) *
                       ftb_bev_stock_sut %>%
                       dplyr::select(dplyr::cur_column())),
        (mode == "SUT" & var == "CIStock") ~
          as.numeric(.freight_tb %>%
                       dplyr::filter(mode == "SUT", var == "TotStock") %>%
                       dplyr::select(dplyr::cur_column()) * ftb_ci_stock_sut %>%
                       dplyr::select(dplyr::cur_column())),
        (mode == "CUT" & var == "BEVStock") ~
          as.numeric(.freight_tb %>%
                       dplyr::filter(mode == "CUT", var == "TotStock") %>%
                       dplyr::select(dplyr::cur_column()) * ftb_bev_stock_cut %>%
                       dplyr::select(dplyr::cur_column())),
        (mode == "CUT" & var == "CIStock") ~
          as.numeric(.freight_tb %>%
                       dplyr::filter(
                         mode == "CUT",
                         var == "TotStock"
                       ) %>%
                       dplyr::select(
                         dplyr::cur_column()
                       ) * ftb_ci_stock_cut %>%
                       dplyr::select(dplyr::cur_column())),
        TRUE ~ value
      ))
    )


    temp <- ftb_new %>%
      dplyr::group_by(mode, ctu, var) %>%
      dplyr::summarise(dplyr::across(everything(), sum)) %>%
      tidyr::pivot_longer(YRS, names_to = "YRS") %>%
      tidyr::pivot_wider(names_from = var)
    temp <- temp %>% dplyr::mutate(TotStock = ifelse((
      mode == "CUT" | mode == "SUT"), BEVStock + CIStock, TotStock))
    ftb_new <- temp %>%
      tidyr::pivot_longer(
        col = !tidyselect::all_of(c(
          "mode",
          "ctu", "YRS"
        )),
        names_to = "var", values_drop_na = TRUE
      ) %>%
      tidyr::pivot_wider(names_from = YRS) %>%
      dplyr::ungroup()
  } else {
    ftb_new <- .freight_tb
  }

  return(list(pass = ptb_new, freight = ftb_new))
}
