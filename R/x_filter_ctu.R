#' @title Filter CTU
#'
#' @param df
#' @param .selected_ctu
#'
#' @return
#' @export
#'
#' @examples
filter_ctu <- function(df, .selected_ctu = "all") {
  if (.selected_ctu == "all") {
    return(df)
  }
  else if ("ctu" %in% colnames(df) && .selected_ctu != "all")    {
    df %>% filter(ctu == .selected_ctu)

  } else  {
    df %>% filter(ctu_name == .selected_ctu)
  }
}
