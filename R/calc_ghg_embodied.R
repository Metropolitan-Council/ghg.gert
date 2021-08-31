#' Calculate embodied/indirect emissions
#'
#' @describeIn ER/EM doesn't impact results because we can assume electricity is
#'  a) generated in a different jurisdiction and/or
#'  b) similarly affected by changes in local grid mix
#'
#' @param tb input table for embodied ghg emissions
#' @param .sales_mode current mode .sales_mode name for calculation of embodied emissions of new vehicles
#' @param .fuel_type fuel source for current mode
#' @param .class vehicle class (passenger or freight)
#' @param .transit_avo_pct percent change in transit AVO. Default is `0`
#' @param .mitigation_tb mitigation output table for results. Default is `0`
#' @param .bau_tb output table for results. Default is `0`
#' @inheritParams calc_ghg_direct
#'
#' @family transportation
#'
#' @return
#' @export
#' @importFrom dplyr filter select case_when mutate across
#' @importFrom tidyselect all_of
calc_ghg_embodied <- function(tb,
                              .mode,
                              .sales_mode,
                              .fuel_type,
                              .class,
                              .transit_avo_pct = 0,
                              .mitigation_tb = 0,
                              .bau_tb = 0) {
  # browser()
  if ((.mode == "BU") | (.mode == "BRT")) {
    ghg <- tb %>%
      dplyr::filter(mode == .mode, var == .sales_mode) %>%
      dplyr::select(all_of(YRS)) *
      ghg_factors %>%
        dplyr::filter(source == .fuel_type) %>%
        dplyr::select(tidyselect::all_of(YRS))

    # adjust stock for changes made in VMT between BAU and MIT scenarios
    # If .transit_avo_pct given then use it, else assume all additional PMT handled by vehicle purchases
    # Update bau_vmt and mit_vmt to equal 1 if they are zero (to avoid division error)
    if (.bau_tb != 0) {
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
  } else {
    ghg_factors_current <- ghg_factors %>%
      dplyr::filter(source == .fuel_type) %>%
      dplyr::select(tidyselect::all_of(YRS))

    ghg <- tb %>%
      dplyr::filter(mode == .mode, var == .sales_mode) %>%
      dplyr::select(tidyselect::all_of(YRS)) %>%
      dplyr::rowwise() %>%
      mutate(
        `2015` = `2015` * (ghg_factors_current)$`2015`,
        `2018` = `2018` * (ghg_factors_current)$`2018`,
        `2020` = `2020` * (ghg_factors_current)$`2020`,
        `2025` = `2025` * (ghg_factors_current)$`2025`,
        `2030` = `2030` * (ghg_factors_current)$`2030`,
        `2035` = `2035` * (ghg_factors_current)$`2035`,
        `2040` = `2040` * (ghg_factors_current)$`2040`,
      )
  }

  return(ghg)
}
