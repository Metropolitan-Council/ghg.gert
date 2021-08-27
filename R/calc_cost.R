#' Calculate cost estimates in millions of dollars
#'
#' @param tb_vmt VMT input table
#' @param tb cost input table
#' @param pr price variable
#' @param is_av whether the mode is av, which affects price. Default is `0`.
#' @inheritParams calc_ghg_direct
#'
#' @family transportation
#' @return
#' @export
#' @importFrom dplyr filter select
calc_cost <- function(tb_vmt,
                      tb,
                      m,
                      pr,
                      is_av = 0) {
  cost_input <- tb %>%
    dplyr::filter(mode == m, var == pr, AV == is_av) %>%
    dplyr::select(all_of(YRS)) / 1000

  cost <- tb_vmt %>%
    dplyr::select(all_of(YRS)) %>%
    dplyr::rowwise() %>%
    mutate_all(., function(col) {
      col * cost_input
    })


  return(cost)
}
