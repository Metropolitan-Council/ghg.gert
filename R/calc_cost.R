#' Calculate cost estimates in millions of dollars
#'
#' @param tb_vmt VMT input table
#' @param tb_cost cost input table
#' @param .price price variable
#' @inheritParams calc_ghg_direct
#'
#' @family transportation
#' @return
#' @export
#' @importFrom dplyr filter select
calc_cost <- function(tb_vmt,
                      tb_cost,
                      .mode,
                      .price,
                      is_av = 0) {
  cost_input <- tb_cost %>%
    dplyr::filter(
      mode == .mode,
      var == .price,
      AV == is_av
    ) %>%
    dplyr::select(all_of(YRS)) / 1000

  cost <- tb_vmt %>%
    dplyr::select(all_of(YRS)) %>%
    dplyr::rowwise() %>%
    mutate_all(., function(col) {
      col * cost_input
    })


  return(cost)
}
