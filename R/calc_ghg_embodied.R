#' @title Calculate embodied (indirect) emissions
#'
#'
#' @param .sales_mode character, sales name for calculation of embodied emissions of new vehicles.
#'     Options include `"SISales"`, `"CISales"`, `"HEVSales"`, `"PHEVSales"`, `"BEVSales"`,
#' @param .fuel_type fuel source for current mode
#' @param .class vehicle class. One of `"SI"`, `"CI"`, `"HEV"`, `"PHEV"`,  or `"BEV"`
#' @param .transit_avo_pct percent change in transit AVO. Default is `0`
#' @param .mitigation_tb mitigation output table for results. Default is `0`
#' @param .bau_tb output table for results. Default is `0`
#' @inheritParams calc_ghg_direct
#'
#' @family transportation
#'
#' @note  `ER` or `EM` doesn't impact results because we can assume electricity is
#'  a) generated in a different jurisdiction and/or
#'  b) similarly affected by changes in local grid mix
#'
#' @return
#' @export
#' @importFrom dplyr filter select case_when mutate across rowwise everything
#' @importFrom tidyselect all_of
calc_ghg_embodied <- function(tb,
                              .mode,
                              .sales_mode,
                              .fuel_type,
                              .class,
                              .transit_avo_pct = 0,
                              .mitigation_tb = 0,
                              .bau_tb = 0,
                              .enviro_factors = enviro_factors) {
  # browser()
  if ((.mode == "BU") | (.mode == "BRT")) {
    ghg_factor_current <- factor_values$ghg %>%
      dplyr::filter(source == .fuel_type) %>%
      dplyr::select(source,
        year,
        ghg_value = value,
      )

    tb_current <- tb %>%
      dplyr::filter(mode == .mode, var == .sales_mode) %>%
      dplyr::select(dplyr::everything(),
        sales_value = value
      ) %>%
      dplyr::mutate(class = .class)


    ghg <- dplyr::left_join(tb_current,
      ghg_factor_current,
      by = c("year")
    ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ghg_embodied = (sales_value * ghg_value) / 1000) %>%
      dplyr::select(
        type,
        ghg_embodied_source = var,
        # scenario,
        mode,
        class,
        ctu = ctu,
        year,
        # aeo_scen,
        aeo_mode,
        ghg_embodied
      )



    # adjust stock for changes made in VMT between BAU and MIT scenarios
    # If .transit_avo_pct given then use it, else assume all additional PMT handled by vehicle purchases
    # Update bau_vmt and mit_vmt to equal 1 if they are zero (to avoid division error)
    if (.bau_tb != 0) {
      # browser()


      .mitigation_tb <- .mitigation_tb %>%
        dplyr::mutate(
          dplyr::across(
            tidyselect::all_of(YRS), ~ case_when(
              (mode == .mode & class == .class & .x == 0) ~ 1,
              TRUE ~ .x / 10^5
            )
          )
        )



      .bau_tb <- .bau_tb %>%
        dplyr::mutate(
          dplyr::across(
            tidyselect::all_of(YRS), ~ case_when(
              (mode == .mode & class == .class & .x == 0) ~ 1,
              TRUE ~ .x
            )
          )
        )

      bau_vals <- .bau_tb %>%
        dplyr::filter(
          mode == .mode,
          class == .class,
          output == "VMT"
        ) %>%
        dplyr::select(tidyselect::all_of(YRS))

      mit_vals <- .mitigation_tb %>%
        dplyr::select(
          tidyselect::all_of(YRS)
        ) / bau_vals



      ghg <- ghg * (mit_vals) *
        (ifelse(.transit_avo_pct > 0, (1 - 1 / (1 + .transit_avo_pct)), 0) + 1)
    }
  } else if (.mode %in% c(
    "MM",
    "SUT",
    "CUT",
    "AIR",
    "WAT"
  )) {
    stop("Embodied emissions only calculated for passenger type")
  } else {
    # browser()

    ghg_factors_current <- factor_values$ghg %>%
      dplyr::filter(source == .fuel_type) %>%
      select(year, ghg_value = value)


    sales <- tb %>%
      dplyr::filter(
        mode == .mode,
        var == .sales_mode
      ) %>%
      unique() %>%
      mutate(class = .class) %>%
      dplyr::select(
        type,
        class,
        ghg_embodied_source = var,
        year,
        mode,
        ctu,
        sales_value = value,
        aeo_mode
      )
    if (.mode == "FR") {
      # browser()
    }

    ghg <- dplyr::left_join(sales,
      ghg_factors_current,
      by = "year"
    ) %>%
      dplyr::mutate(ghg_embodied = (sales_value * ghg_value) / 1000) %>%
      dplyr::select(
        type,
        ghg_embodied_source,
        # scenario,
        mode,
        class,
        ctu = ctu,
        year,
        # aeo_scen,
        aeo_mode,
        ghg_embodied
      ) %>%
      unique()
  }

  return(ghg)
}
