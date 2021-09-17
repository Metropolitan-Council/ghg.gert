#' Calculate cost estimates in millions of dollars
#'
#' @param tb_vmt VMT input table
#' @param .price character, price variable. Options include `"SIPrice"`
#' @inheritParams calc_ghg_direct
#'
#' @family transportation
#' @return
#' @export
#' @importFrom dplyr filter select
calc_cost <- function(tb_vmt,
                      .mode,
                      .price,
                      .is_av = 0,
                      .enviro_factors = enviro_factors) {
  # browser()
  tb_cost_current <- factor_values$cost %>%
    dplyr::filter(
      mode == .mode,
      var == .price,
      is_av == .is_av
    ) %>%
    dplyr::mutate(cost_value = value / 1000)


  vmt_cost <- dplyr::left_join(
    tb_vmt,
    tb_cost_current,
    by = c("mode", "year")
  ) %>%
    dplyr::mutate(vmt_cost = vmt * cost_value) %>%
    dplyr::select(
      scenario,
      # type,
      mode,
      ctu,
      year,
      aeo_mode,
      class,
      vmt_cost
    )


  return(vmt_cost)
}
