#' @title Calculate cost estimates in dollars
#'
#' @param tb_vmt VMT input table
#' @param .price character, price variable. Options include `"SIPrice"`
#' @inheritParams calc_ghg_direct
#'
#' @family transportation
#' @export
#' @importFrom dplyr filter select mutate
#'
calc_cost <- function(tb_vmt,
                      .selected_ctu = "all",
                      .mode,
                      .price,
                      .enviro_factors = enviro_factors,
                      .factor_values = factor_values) {
  # browser()

  tb_vmt <- filter_ctu(tb_vmt, .selected_ctu)

  tb_cost_current <- .factor_values$cost %>%
    dplyr::filter(
      mode == .mode,
      var == .price
    ) %>%
    # cost is in dollars
    # convert to thousands of dollars
    dplyr::mutate(cost_value = value / 1000)


  vmt_cost_fin <- dplyr::left_join(
    tb_vmt,
    tb_cost_current,
    by = c("mode", "year")
  ) %>%
    # vmt reported in thousands
    # cost is now in thousands
    # vmt_cost is in _dollars_
    dplyr::mutate(vmt_cost = vmt * cost_value) %>%
    dplyr::select(
      scenario,
      type,
      mode,
      ctu,
      year,
      aeo_mode,
      class,
      vmt_cost
    )


  return(vmt_cost_fin)
}
