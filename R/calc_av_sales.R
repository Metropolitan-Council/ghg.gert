#' @title Calculate AV stock sales
#'
#' @describeIn Replaces both sales and stock because need to replace
#'     faster than previous stock turnover in order to match
#'     AV market penetration
#' @param tb input table
#' @param .av_pct percent of trips by AV
#'
#' @family transportation. autonomous vehicles
#'
#' @return
#' @export
#' @importFrom dplyr select filter across cur_column
#'
calc_av_sales <- function(tb,
                          .av_pct) {
  browser()


  # Calculate basic sales (initial + 1/3 fleet replacement) and store as temp variable
  # Initial population of tibble with zeros
  temp <- tb %>%
    dplyr::filter(var == "AVStock") %>%
    dplyr::select(all_of(YRS)) * 0

  # Sales are equal to the change in total stock between years
  # (TotStock=AVStock) x AVShare in current year + additional sales
  # due to turnover (1/3 in 2030-2050)
  temp <- temp %>%
    dplyr::mutate(dplyr::across(
      all_of(FOR_YRS),
      ~ unlist((tb %>%
        dplyr::filter(var == "AVStock") %>%
        dplyr::select(dplyr::cur_column()) - tb %>%
        dplyr::filter(var == "AVStock") %>%
        dplyr::select(which(colnames(tb) == dplyr::cur_column()) - 1)) *
        tb %>%
          dplyr::filter(mode == "AV", var == "AVShare") %>%
          dplyr::select(dplyr::cur_column()) * av / 100 +
        tb %>%
        dplyr::filter(mode == "AV", var == "AVStock") %>%
        dplyr::select(which(colnames(tb) == dplyr::cur_column()) - 1) *
        tb %>%
          dplyr::filter(mode == "AV", var == "AVShare") %>%
          dplyr::select(dplyr::cur_column()) * av / 100 * 1 / 3)
    ))
  return(temp)
}
