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
    dplyr::mutate(
      `2015` = `2015` * (cost_input)$`2015`,
      `2018` = `2018` * (cost_input)$`2018`,
      `2020` = `2020` * (cost_input)$`2020`,
      `2025` = `2025` * (cost_input)$`2025`,
      `2030` = `2030` * (cost_input)$`2030`,
      `2035` = `2035` * (cost_input)$`2035`,
      `2040` = `2040` * (cost_input)$`2040`,
    )


  # mutate_all(., function(col) {
  #   col * cost_input
  # })


  return(cost)
}
