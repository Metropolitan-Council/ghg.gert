#' @title Adjust the fleet
#'
#' @describeIn Match what the user input for sales in 2050 rather
#'     than the defaults from MA3TFleet held fixed in all cases.
#'     AV/DRS scenarios adjust the sales
#'     total up, but they adjust the existing stock down to match total stock
#'     in each year.
#'
#' @param bev percent of sales that are battery electric vehicles (BEV) in 2050
#' @param phev percent of sales that are plug-in hybrid electric (PHEV) in 2050
#' @param hev percent of sales that are hybrid electric vehicles (HEV) in 2050
#' @param ptb passenger input table
#' @param ftb freight input table
#' @param drs percent of trips/fleet that is dynamic ride sharing (DRS) Default is `0`.
#' @param av_pct percent of trips/fleet that are autonomous vehicles (AV). Default is `0`.
#' @param ch_ctu the chosen CTU for dplyr::filter of tables
#' @inheritParams calc_vmt
#'
#' @family transportation
#' @return
#' @export
#'
#' @importFrom tidyselect all_of
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom dplyr filter select case_when mutate across
adj_fleet_shares <- function(bev,
                             phev,
                             hev,
                             ptb,
                             ftb,
                             vmt = 0,
                             payd = 0,
                             gas = 0,
                             drs = 0,
                             av = 0,
                             ch_ctu) {

  # Adjust sales based on ownership response to price elasticity
  adj_si_ci_sales <- (1 + (vmt / AUTO_COST_MI + payd / AUTO_COST_MI) * ELAST_OWN_PRICE) *
    (1 + (gas / AUTO_COST_MI) * ELAST_OWN_PRICE)
  # Assume HEV, PHEV, and BEV not affected by gas price because already switched stock type
  adj_alt_sales <- (1 + (vmt / AUTO_COST_MI + payd / AUTO_COST_MI) * ELAST_OWN_PRICE)
  ptb <- ptb %>% dplyr::mutate(dplyr::across(tidyselect::all_of(FOR_YRS), ~ dplyr::case_when(
    (mode == "PLDV" & var == "BEVExist") ~ .x * adj_alt_sales,
    (mode == "PLDV" & var == "PHEVExist") ~ .x * adj_alt_sales,
    (mode == "PLDV" & var == "HEVExist") ~ .x * adj_alt_sales,
    (mode == "PLDV" & var == "SIExist") ~ .x * adj_si_ci_sales,
    (mode == "PLDV" & var == "CIExist") ~ .x * adj_si_ci_sales,
    TRUE ~ .x
  )))

  # DRS adjustment of all Sales, Existing, and Stock in each year regardless of passenger mode
  if (drs > 0) {
    ptb <- ptb %>% dplyr::mutate(dplyr::across(tidyselect::all_of(FOR_YRS), ~ dplyr::case_when(
      ((stringr::str_detect(var, "Sales")) | (stringr::str_detect(var, "Exist")) | (stringr::str_detect(var, "Stock"))) ~ .x *
        (1 - ptb %>% dplyr::filter(var == "DRSShare") %>% dplyr::select(tidyselect::all_of(FOR_YRS)) %>% as.numeric() * drs / 100),
      TRUE ~ .x
    )))
  }

  # AV adjustment of all Sales, Existing, and Stock in each year regardless of passenger mode
  # Non-AV portion continues as before and AV treated separately
  if (av > 0) {
    # Add a row for stock to pivot AV analysis off
    av_stock <- tibble::tibble(mode = "AV", var = "AVStock", ctu = ch_ctu, ptb %>% dplyr::filter(mode == "PLDV", var == "TotStock") %>% dplyr::select(tidyselect::all_of(YRS)))
    av_stock <- av_stock %>% dplyr::mutate(dplyr::across(setdiff(YRS, FOR_YRS), ~0))
    ptb <- dplyr::bind_rows(ptb, av_stock)
    ptb <- ptb %>% dplyr::mutate(dplyr::across(tidyselect::all_of(FOR_YRS), ~ dplyr::case_when(
      ((stringr::str_detect(var, "Sales") | stringr::str_detect(var, "Exist") | stringr::str_detect(var, "Stock")) & mode == "PLDV") ~ .x *
        (1 - ptb %>% dplyr::filter(var == "AVShare") %>% dplyr::select(tidyselect::all_of(FOR_YRS)) %>% as.numeric() * av / 100), # Remove AV from non-AV stock
      TRUE ~ .x
    )))
  }

  if (bev > 0 | phev > 0 | hev > 0) {
    # Temporarily update any zero sales in final year to equal 1
    ptb <- ptb %>% dplyr::mutate(dplyr::across(tidyselect::all_of(FIN_YR), ~ dplyr::case_when(
      (mode == "PLDV" & stringr::str_detect(var, "Sales") & .x == 0) ~ 1,
      TRUE ~ .x
    )))

    # dplyr::filter out the ratios in the BAU and compare with user input for alternative scenario
    # PASSENGER
    ptb_si_sales <- as.numeric(100 - bev - phev - hev) / 100 * (ptb %>% dplyr::filter(mode == "PLDV", var == "SISales") %>%
      dplyr::select(FOR_YRS) /
      (ptb %>% dplyr::filter(mode == "PLDV", var == "SISales") %>%
        dplyr::select(FOR_YRS) +
        ptb %>% dplyr::filter(mode == "PLDV", var == "CISales") %>%
        dplyr::select(FOR_YRS))) *
      (ptb %>% dplyr::filter(mode == "PLDV", var == "SISales") %>%
        dplyr::select(FOR_YRS) /
        ptb %>%
          dplyr::filter(mode == "PLDV", var == "TotSales") %>%
          dplyr::select(FOR_YRS)) / (ptb %>% dplyr::filter(mode == "PLDV", var == "SISales") %>%
        dplyr::select(FIN_YR) /
        ptb %>%
          dplyr::filter(mode == "PLDV", var == "TotSales") %>%
          dplyr::select(FIN_YR)) %>% as.numeric()
    ptb_ci_sales <- as.numeric(100 - bev - phev - hev) / 100 * (ptb %>% dplyr::filter(mode == "PLDV", var == "CISales") %>%
      dplyr::select(FOR_YRS) /
      (ptb %>% dplyr::filter(mode == "PLDV", var == "SISales") %>%
        dplyr::select(FOR_YRS) +
        ptb %>% dplyr::filter(mode == "PLDV", var == "CISales") %>%
        dplyr::select(FOR_YRS))) *
      (ptb %>% dplyr::filter(mode == "PLDV", var == "CISales") %>%
        dplyr::select(FOR_YRS) /
        ptb %>%
          dplyr::filter(mode == "PLDV", var == "TotSales") %>%
          dplyr::select(FOR_YRS)) / (ptb %>% dplyr::filter(mode == "PLDV", var == "CISales") %>%
        dplyr::select(FIN_YR) /
        ptb %>%
          dplyr::filter(mode == "PLDV", var == "TotSales") %>%
          dplyr::select(FIN_YR)) %>% as.numeric()
    ptb_hev_sales <- as.numeric(hev / 100) * (ptb %>% dplyr::filter(mode == "PLDV", var == "HEVSales") %>%
      dplyr::select(FOR_YRS) /
      ptb %>%
        dplyr::filter(mode == "PLDV", var == "TotSales") %>%
        dplyr::select(FOR_YRS)) / (ptb %>% dplyr::filter(mode == "PLDV", var == "HEVSales") %>%
      dplyr::select(FIN_YR) /
      ptb %>%
        dplyr::filter(mode == "PLDV", var == "TotSales") %>%
        dplyr::select(FIN_YR)) %>% as.numeric()
    ptb_phev_sales <- as.numeric(phev / 100) * (ptb %>% dplyr::filter(mode == "PLDV", var == "PHEVSales") %>%
      dplyr::select(FOR_YRS) /
      ptb %>%
        dplyr::filter(mode == "PLDV", var == "TotSales") %>%
        dplyr::select(FOR_YRS)) / (ptb %>% dplyr::filter(mode == "PLDV", var == "PHEVSales") %>%
      dplyr::select(FIN_YR) /
      ptb %>%
        dplyr::filter(mode == "PLDV", var == "TotSales") %>%
        dplyr::select(FIN_YR)) %>% as.numeric()
    ptb_bev_sales <- as.numeric(bev / 100) * (ptb %>% dplyr::filter(mode == "PLDV", var == "BEVSales") %>%
      dplyr::select(FOR_YRS) /
      ptb %>%
        dplyr::filter(mode == "PLDV", var == "TotSales") %>%
        dplyr::select(FOR_YRS)) / (ptb %>% dplyr::filter(mode == "PLDV", var == "BEVSales") %>%
      dplyr::select(FIN_YR) /
      ptb %>%
        dplyr::filter(mode == "PLDV", var == "TotSales") %>%
        dplyr::select(FIN_YR)) %>% as.numeric()

    # Readjust sales totals to sum to 100% in each year
    ptb_tot_sales <- ptb_si_sales + ptb_ci_sales + ptb_hev_sales + ptb_phev_sales + ptb_bev_sales
    ptb_si_sales <- ptb_si_sales / ptb_tot_sales
    ptb_ci_sales <- ptb_ci_sales / ptb_tot_sales
    ptb_hev_sales <- ptb_hev_sales / ptb_tot_sales
    ptb_phev_sales <- ptb_phev_sales / ptb_tot_sales
    ptb_bev_sales <- ptb_bev_sales / ptb_tot_sales

    # Updated sales distribution
    ptb_new <- ptb %>% dplyr::mutate(dplyr::across(tidyselect::all_of(FOR_YRS), ~ dplyr::case_when(
      (mode == "PLDV" & var == "BEVSales") ~ as.numeric(ptb %>% dplyr::filter(mode == "PLDV", var == "TotSales") %>% dplyr::select(cur_column()) * ptb_bev_sales %>% dplyr::select(cur_column())),
      (mode == "PLDV" & var == "PHEVSales") ~ as.numeric(ptb %>% dplyr::filter(mode == "PLDV", var == "TotSales") %>% dplyr::select(cur_column()) * ptb_phev_sales %>% dplyr::select(cur_column())),
      (mode == "PLDV" & var == "HEVSales") ~ as.numeric(ptb %>% dplyr::filter(mode == "PLDV", var == "TotSales") %>% dplyr::select(cur_column()) * ptb_hev_sales %>% dplyr::select(cur_column())),
      (mode == "PLDV" & var == "SISales") ~ as.numeric(ptb %>% dplyr::filter(mode == "PLDV", var == "TotSales") %>% dplyr::select(cur_column()) * ptb_si_sales %>% dplyr::select(cur_column())),
      (mode == "PLDV" & var == "CISales") ~ as.numeric(ptb %>% dplyr::filter(mode == "PLDV", var == "TotSales") %>% dplyr::select(cur_column()) * ptb_ci_sales %>% dplyr::select(cur_column())),
      TRUE ~ .x
    )))
  } else {
    ptb_new <- ptb
  }

  ftb_new <- ftb

  # New existing stock is old ratio x (exist_old+sales_new - 5yrs)/(exist_old+sales_old - 5 yrs) in each year
  ptb_si_exist <- as.numeric((ptb %>% dplyr::filter(mode == "PLDV", var == "SIExist") %>%
    dplyr::select(tidyselect::all_of(ADJ_YRS)) + ptb_new %>% dplyr::filter(mode == "PLDV", var == "SISales") %>%
    dplyr::select(tidyselect::all_of(ADJ_YRS))) /
    (ptb %>% dplyr::filter(mode == "PLDV", var == "SIExist") %>%
      dplyr::select(tidyselect::all_of(ADJ_YRS)) + ptb %>% dplyr::filter(mode == "PLDV", var == "SISales") %>%
      dplyr::select(tidyselect::all_of(ADJ_YRS))))
  ptb_ci_exist <- as.numeric((ptb %>% dplyr::filter(mode == "PLDV", var == "CIExist") %>%
    dplyr::select(tidyselect::all_of(ADJ_YRS)) + ptb_new %>% dplyr::filter(mode == "PLDV", var == "CISales") %>%
    dplyr::select(tidyselect::all_of(ADJ_YRS))) /
    (ptb %>% dplyr::filter(mode == "PLDV", var == "CIExist") %>%
      dplyr::select(tidyselect::all_of(ADJ_YRS)) + ptb %>% dplyr::filter(mode == "PLDV", var == "CISales") %>%
      dplyr::select(tidyselect::all_of(ADJ_YRS))))
  ptb_hev_exist <- as.numeric((ptb %>% dplyr::filter(mode == "PLDV", var == "HEVExist") %>%
    dplyr::select(tidyselect::all_of(ADJ_YRS)) + ptb_new %>% dplyr::filter(mode == "PLDV", var == "HEVSales") %>%
    dplyr::select(tidyselect::all_of(ADJ_YRS))) /
    (ptb %>% dplyr::filter(mode == "PLDV", var == "HEVExist") %>%
      dplyr::select(tidyselect::all_of(ADJ_YRS)) + ptb %>% dplyr::filter(mode == "PLDV", var == "HEVSales") %>%
      dplyr::select(tidyselect::all_of(ADJ_YRS))))
  ptb_phev_exist <- as.numeric((ptb %>% dplyr::filter(mode == "PLDV", var == "PHEVExist") %>%
    dplyr::select(tidyselect::all_of(ADJ_YRS)) + ptb_new %>% dplyr::filter(mode == "PLDV", var == "PHEVSales") %>%
    dplyr::select(tidyselect::all_of(ADJ_YRS))) /
    (ptb %>% dplyr::filter(mode == "PLDV", var == "PHEVExist") %>%
      dplyr::select(tidyselect::all_of(ADJ_YRS)) + ptb %>% dplyr::filter(mode == "PLDV", var == "PHEVSales") %>%
      dplyr::select(tidyselect::all_of(ADJ_YRS))))
  ptb_bev_exist <- as.numeric((ptb %>% dplyr::filter(mode == "PLDV", var == "BEVExist") %>%
    dplyr::select(tidyselect::all_of(ADJ_YRS)) + ptb_new %>% dplyr::filter(mode == "PLDV", var == "BEVSales") %>%
    dplyr::select(tidyselect::all_of(ADJ_YRS))) /
    (ptb %>% dplyr::filter(mode == "PLDV", var == "BEVExist") %>%
      dplyr::select(tidyselect::all_of(ADJ_YRS)) + ptb %>% dplyr::filter(mode == "PLDV", var == "BEVSales") %>%
      dplyr::select(tidyselect::all_of(ADJ_YRS))))

  ptb_new <- ptb_new %>% dplyr::mutate(dplyr::across(tidyselect::all_of(FOR_YRS), ~ dplyr::case_when(
    (mode == "PLDV" & var == "BEVExist") ~ .x * ptb_bev_exist,
    (mode == "PLDV" & var == "PHEVExist") ~ .x * ptb_phev_exist,
    (mode == "PLDV" & var == "HEVExist") ~ .x * ptb_hev_exist,
    (mode == "PLDV" & var == "SIExist") ~ .x * ptb_si_exist,
    (mode == "PLDV" & var == "CIExist") ~ .x * ptb_ci_exist,
    TRUE ~ .x
  )))

  temp <- ptb_new %>%
    group_by(mode, ctu, var) %>%
    summarise(dplyr::across(everything(), sum)) %>%
    pivot_longer(YRS, names_to = "YRS") %>%
    pivot_wider(names_from = var)
  temp <- temp %>% dplyr::mutate(
    BEVStock = ifelse(mode == "PLDV", BEVExist + BEVSales, BEVStock),
    PHEVStock = ifelse(mode == "PLDV", PHEVExist + PHEVSales, PHEVStock),
    HEVStock = ifelse(mode == "PLDV", HEVExist + HEVSales, HEVStock),
    CIStock = ifelse(mode == "PLDV", CIExist + CISales, CIStock),
    SIStock = ifelse(mode == "PLDV", SIExist + SISales, SIStock)
  )
  temp <- temp %>% dplyr::mutate(
    TotStock = ifelse(mode == "PLDV", BEVStock + PHEVStock + HEVStock + CIStock + SIStock, TotStock),
    TotExist = ifelse(mode == "PLDV", BEVExist + PHEVExist + HEVExist + CIExist + SIExist, TotExist),
    TotSales = ifelse(mode == "PLDV", BEVSales + PHEVSales + HEVSales + CISales + SISales, TotSales)
  )
  ptb_new <- temp %>%
    pivot_longer(col = !tidyselect::all_of(c("mode", "ctu", "YRS")), names_to = "var", values_drop_na = TRUE) %>%
    pivot_wider(names_from = YRS) %>%
    ungroup()

  # Switch update any zero sales in final year to equal 1 back to 0
  ptb_new <- ptb_new %>% dplyr::mutate(dplyr::across(tidyselect::all_of(FIN_YR), ~ dplyr::case_when(
    (mode == "PLDV" & stringr::str_detect(var, "Sales") & .x == 1) ~ 0,
    TRUE ~ .x
  )))
  if (bev > 0 | phev > 0 | hev > 0) {
    # FREIGHT - BAU assumes 1/3 and 2/3 change (relative to PLDV in 2025-2040) to freight sales to include BEV (as summation of BEV+PHEV+HEV from PLDV) for SUT and CUT, respectively
    # We do not have good stock numbers on freight so we do not consider the embodied emissions from freight and the shift in sales, etc. from the passenger
    # fleet is translated into a total stock number for freight. The use of 1/3 and 2/3 helps to account for this being sales not total stock (i.e., should be lower as percent of total stock)
    fbev <- bev * ptb_new %>%
      dplyr::filter(mode == "PLDV", var == "BEVSales") %>%
      dplyr::select(FIN_YR) /
      ptb_new %>%
        dplyr::filter(mode == "PLDV", var == "BEVStock") %>%
        dplyr::select(FIN_YR)
    # Temporarily update any zero stock in final year to equal 1
    ftb <- ftb %>% dplyr::mutate(dplyr::across(tidyselect::all_of(FIN_YR), ~ dplyr::case_when(
      ((mode == "SUT" | mode == "CUT") & stringr::str_detect(var, "Stock") & .x == 0) ~ 1,
      TRUE ~ .x
    )))
    # dplyr::filter out the ratios in the BAU and compare with user input for alternative scenario
    ftb_ci_stock_sut <- as.numeric((100 - 2 / 3 * fbev) / 100) * (ftb %>% dplyr::filter(mode == "SUT", var == "CIStock") %>%
      dplyr::select(FOR_YRS) /
      ftb %>%
        dplyr::filter(mode == "SUT", var == "TotStock") %>%
        dplyr::select(FOR_YRS)) / (ftb %>% dplyr::filter(mode == "SUT", var == "CIStock") %>%
      dplyr::select(FIN_YR) /
      ftb %>%
        dplyr::filter(mode == "SUT", var == "TotStock") %>%
        dplyr::select(FIN_YR)) %>% as.numeric()

    ftb_bev_stock_sut <- as.numeric(2 / 3 * fbev / 100) * (ftb %>% dplyr::filter(mode == "SUT", var == "BEVStock") %>%
      dplyr::select(FOR_YRS) /
      ftb %>%
        dplyr::filter(mode == "SUT", var == "TotStock") %>%
        dplyr::select(FOR_YRS)) / (ftb %>% dplyr::filter(mode == "SUT", var == "BEVStock") %>%
      dplyr::select(FIN_YR) /
      ftb %>%
        dplyr::filter(mode == "SUT", var == "TotStock") %>%
        dplyr::select(FIN_YR)) %>% as.numeric()
    ftb_ci_stock_cut <- as.numeric(100 - 1 / 3 * fbev) / 100 * (ftb %>% dplyr::filter(mode == "CUT", var == "CIStock") %>%
      dplyr::select(FOR_YRS) /
      ftb %>%
        dplyr::filter(mode == "CUT", var == "TotStock") %>%
        dplyr::select(FOR_YRS)) / (ftb %>% dplyr::filter(mode == "SUT", var == "CIStock") %>%
      dplyr::select(FIN_YR) /
      ftb %>%
        dplyr::filter(mode == "CUT", var == "TotStock") %>%
        dplyr::select(FIN_YR)) %>% as.numeric()

    ftb_bev_stock_cut <- as.numeric(1 / 3 * fbev / 100) * (ftb %>% dplyr::filter(mode == "CUT", var == "BEVStock") %>%
      dplyr::select(FOR_YRS) /
      ftb %>%
        dplyr::filter(mode == "CUT", var == "TotStock") %>%
        dplyr::select(FOR_YRS)) / (ftb %>% dplyr::filter(mode == "CUT", var == "BEVStock") %>%
      dplyr::select(FIN_YR) /
      ftb %>%
        dplyr::filter(mode == "CUT", var == "TotStock") %>%
        dplyr::select(FIN_YR)) %>% as.numeric()

    # Readjust sales totals to sum to 100% in each year
    ftb_tot_stock_sut <- ftb_ci_stock_sut + ftb_bev_stock_sut
    ftb_ci_stock_sut <- ftb_ci_stock_sut / ftb_tot_stock_sut
    ftb_bev_stock_sut <- ftb_bev_stock_sut / ftb_tot_stock_sut
    ftb_tot_stock_cut <- ftb_ci_stock_cut + ftb_bev_stock_cut
    ftb_ci_stock_cut <- ftb_ci_stock_cut / ftb_tot_stock_cut
    ftb_bev_stock_cut <- ftb_bev_stock_cut / ftb_tot_stock_cut

    # Updated stock distribution
    ftb_new <- ftb %>% dplyr::mutate(dplyr::across(tidyselect::all_of(FOR_YRS), ~ dplyr::case_when(
      (mode == "SUT" & var == "BEVStock") ~ as.numeric(ftb %>% dplyr::filter(mode == "SUT", var == "TotStock") %>% dplyr::select(cur_column()) * ftb_bev_stock_sut %>% dplyr::select(cur_column())),
      (mode == "SUT" & var == "CIStock") ~ as.numeric(ftb %>% dplyr::filter(mode == "SUT", var == "TotStock") %>% dplyr::select(cur_column()) * ftb_ci_stock_sut %>% dplyr::select(cur_column())),
      (mode == "CUT" & var == "BEVStock") ~ as.numeric(ftb %>% dplyr::filter(mode == "CUT", var == "TotStock") %>% dplyr::select(cur_column()) * ftb_bev_stock_cut %>% dplyr::select(cur_column())),
      (mode == "CUT" & var == "CIStock") ~ as.numeric(ftb %>% dplyr::filter(mode == "CUT", var == "TotStock") %>% dplyr::select(cur_column()) * ftb_ci_stock_cut %>% dplyr::select(cur_column())),
      TRUE ~ .x
    )))


    temp <- ftb_new %>%
      group_by(mode, ctu, var) %>%
      summarise(dplyr::across(everything(), sum)) %>%
      pivot_longer(YRS, names_to = "YRS") %>%
      pivot_wider(names_from = var)
    temp <- temp %>% dplyr::mutate(TotStock = ifelse((mode == "CUT" | mode == "SUT"), BEVStock + CIStock, TotStock))
    ftb_new <- temp %>%
      pivot_longer(col = !tidyselect::all_of(c("mode", "ctu", "YRS")), names_to = "var", values_drop_na = TRUE) %>%
      pivot_wider(names_from = YRS) %>%
      ungroup()
  } else {
    ftb_new <- ftb
  }

  return(list(pass = ptb_new, freight = ftb_new))
}
