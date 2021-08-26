#' @title Calculate DRS stock sales
#'
#' @param tb input table
#' @param drs percent of trips by DRS and therefore percent of sales
#'
#' @return
#' @export
#' @importFrom dplyr select filter case_when
#'
calc_drs_sales <- function(tb, drs) {
  # Calculate basic sales (initial + 1/3 fleet replacement) and store as temp variable
  sales <- tb %>%
    dplyr::filter(var == "POP") %>%
    dplyr::select(all_of(YRS)) *
    dplyr::case_when(
      drs > 0 ~ tb %>%
        dplyr::filter(var == "SAVSales") %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      TRUE ~ 1
    )
  return(sales)
}
