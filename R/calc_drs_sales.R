#' @title Calculate dynamic ride sharing stock sales
#'
#' @param tb input table
#' @param .drs_pct_trip percent of trips by dynamic ride sharing and therefore percent of sales
#'
#' @family transportation
#'
#' @return
#' @export
#' @importFrom dplyr select filter case_when
#'
calc_drs_sales <- function(tb,
                           .drs_pct_trip) {
  # Calculate basic sales (initial + 1/3 fleet replacement) and store as temp variable
  sales <- tb %>%
    dplyr::filter(var == "POP") %>%
    dplyr::select(all_of(YRS)) *
    dplyr::case_when(
      .drs_pct_trip > 0 ~ tb %>%
        dplyr::filter(var == "SAVSales") %>%
        dplyr::select(all_of(YRS)) %>%
        as.numeric(),
      TRUE ~ 1
    )
  return(sales)
}
