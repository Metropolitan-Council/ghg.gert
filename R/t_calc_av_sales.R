#' @title Calculate AV stock sales
#' @family transportation
#' @family Autonomous Vehicles
#'
#' @description Replaces both sales and stock because need to replace
#'     faster than previous stock turnover in order to match
#'     AV market penetration.
#'
#' @inheritParams calc_vmt_forecast
#' @param tb input table for passenger modes. Should have columns `mode`, `var`, `ctu`, `value`,
#'    and one for each year. Package provided dataset `transportation_data$passenger` is suitable.
#'    Must have `"AVStock"` variable.
#'
#' @return [tibble::tibble()] with column names...
#' @export
#' @importFrom dplyr select filter across cur_column
#'
calc_av_sales <- function(tb,
                          .av_pct) {
  # browser()

  if (!"AVStock" %in% unique(tb$var)) {
    stop("No AVStock provided. Run `adj_fleet_shares()` on `tb` prior to calculating AV sales")
  }

  check_inputs("av_pct", .av_pct)

  # Calculate basic sales (initial + 1/3 fleet replacement) and store as temp variable
  # Initial population of tibble with zeros

  tb_av_stock <- tb %>%
    dplyr::filter(
      var == "AVStock"
    ) %>%
    dplyr::mutate(
      var = "AVStock",
      mode = "PLDV",
      AVStock = value
    ) %>%
    dplyr::select(-value, -var) %>%
    unique()

  av_shares <- tb %>%
    dplyr::filter(
      mode == "AV",
      var == "AVShare"
    ) %>%
    dplyr::select(year,
      AVShare = value
    ) %>%
    unique()

  tb_av_sales <-
    tb_av_stock %>%
    dplyr::left_join(av_shares, by = c("year")) %>%
    dplyr::group_by(mode, ctu, aeo_mode, type) %>%
    dplyr::arrange(ctu, year) %>%
    dplyr::mutate(
      lag_avstock = dplyr::lag(AVStock, 1, default = 0),
      lag_diff = AVStock - lag_avstock,
      mult_by_share = lag_diff * AVShare * .av_pct,
      additional_sales = lag_avstock * AVShare * .av_pct * (1 / 3),
      av_sales = mult_by_share + additional_sales
    ) %>%
    dplyr::mutate(
      var = "AVSales",
      value = av_sales
    ) %>%
    dplyr::select(names(tb))


  # temp <- tb %>%
  #   dplyr::filter(var == "AVStock") %>%
  #   dplyr::select(all_of(YRS)) * 0

  # Sales are equal to the change in total stock between years
  # (TotStock-AVStock) x AVShare in current year + additional sales
  # due to turnover (1/3 in 2030-2050)

  # ((AVStock - AVStock (prev year)) * AVShare (current year) * .av_pct) + (AVStock (prev year) * AVShare * .av_pct * 1/3)
  # temp <- temp %>%
  #   dplyr::mutate(dplyr::across(
  #     all_of(FOR_YRS),
  #     ~ unlist((tb %>%
  #                 dplyr::filter(var == "AVStock") %>%
  #                 dplyr::select(dplyr::cur_column()) -
  #                 tb %>%
  #                 dplyr::filter(var == "AVStock") %>%
  #                 dplyr::select(which(colnames(tb) == dplyr::cur_column()) - 1)) *
  #                tb %>%
  #                dplyr::filter(mode == "AV", var == "AVShare") %>%
  #                dplyr::select(dplyr::cur_column()) *
  #                .av_pct +
  #                tb %>%
  #                dplyr::filter(mode == "AV", var == "AVStock") %>%
  #                dplyr::select(which(colnames(tb) == dplyr::cur_column()) - 1) *
  #                tb %>%
  #                dplyr::filter(mode == "AV", var == "AVShare") %>%
  #                dplyr::select(dplyr::cur_column()) *
  #                .av_pct *
  #                1 / 3)
  #   ))
  return(tb_av_sales)
}
