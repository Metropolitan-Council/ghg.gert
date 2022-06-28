#' @title Calculate dynamic ride sharing stock sales
#' @family Transportation
#'
#' @inheritParams calc_vmt_forecast
#' @param tb input table
#'
#'
#' @return [tibble::tibble()] with column names...
#' @export
#' @importFrom dplyr select filter case_when
#'
calc_drs_sales <- function(tb,
                           .drs_pct,
                           .enviro_factors = enviro_factors) {
  # browser()

  pop <- tb %>%
    filter(var == "POP") %>%
    select(year, ctu, population = value) %>%
    unique()

  sales <- tb %>%
    filter(var == "DRSSales") %>%
    select(everything(), DRSSales = value) %>%
    left_join(pop, by = c("year", "ctu")) %>%
    mutate(
      DRSSales = population * DRSSales,
      var = "DRSSales",
      value = DRSSales
    ) %>%
    select(names(tb))

  # Calculate basic sales (initial + 1/3 fleet replacement) and store as temp variable
  return(sales)
}
