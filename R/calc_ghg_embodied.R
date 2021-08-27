#' Calculate embodied/indirect emissions
#'
#' @describeIn ER/EM doesn't impact results because we can assume electricity is
#'  a) generated in a different jurisdiction and/or
#'  b) similarly affected by changes in local grid mix
#'
#' @param tb input table for embodied ghg emissions
#' @param sales current mode sales name for calculation of embodied emissions of new vehicles
#' @param sour fuel source for current mode
#' @param c vehicle class (passenger or freight)
#' @param t_avo percent change in transit AVO. Default is `0`
#' @param mit mitigation output table for results. Default is `0`
#' @param bau bau output table for results. Default is `0`
#' @inheritParams calc_ghg_direct
#'
#' @return
#' @export
#' @importFrom dplyr filter select case_when mutate across
#' @importFrom tidyselect all_of
calc_ghg_embodied <- function(tb,
                              .mode,
                              sales,
                              sour,
                              c,
                              t_avo = 0,
                              mit = 0,
                              bau = 0) {
  # browser()
  if ((.mode == "BU") | (.mode == "BRT")) {
    ghg <- tb %>%
      dplyr::filter(mode == .mode, var == sales) %>%
      dplyr::select(all_of(YRS)) *
      ghg_factors %>%
        dplyr::filter(source == sour) %>%
        dplyr::select(tidyselect::all_of(YRS))

    # adjust stock for changes made in VMT between BAU and MIT scenarios
    # If t_avo given then use it, else assume all additional PMT handled by vehicle purchases
    # Update bau_vmt and mit_vmt to equal 1 if they are zero (to avoid division error)
    if (bau != 0) {
      mit <- mit %>%
        dplyr::mutate(
          dplyr::across(
            tidyselect::all_of(YRS), ~ case_when(
              (mode == .mode & class == c & .x == 0) ~ 1,
              TRUE ~ .x / 10^5
            )
          )
        )



      bau <- bau %>%
        dplyr::mutate(
          dplyr::across(
            tidyselect::all_of(YRS), ~ case_when(
              (mode == .mode & class == c & .x == 0) ~ 1,
              TRUE ~ .x
            )
          )
        )

      bau_vals <- bau %>%
        dplyr::filter(
          mode == .mode,
          class == c,
          output == "VMT"
        ) %>%
        dplyr::select(tidyselect::all_of(YRS))

      mit_vals <- mit %>%
        dplyr::select(
          tidyselect::all_of(YRS)
        ) / bau_vals



      ghg <- ghg * (mit_vals) *
        (ifelse(t_avo > 0, (1 - 1 / (1 + t_avo)), 0) + 1)
    }
  } else {
    ghg_factors_current <- ghg_factors %>%
      dplyr::filter(source == sour) %>%
      dplyr::select(tidyselect::all_of(YRS))

    ghg <- tb %>%
      dplyr::filter(mode == .mode, var == sales) %>%
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
