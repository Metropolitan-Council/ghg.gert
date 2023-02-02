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
    return(df %>% filter(!!ctu:= .selected_ctu))

  } else  {
    return(df %>% filter(ctu_name == .selected_ctu))
  }
}



#' @title Filter Building Energy Data
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
filter_building_energy_data <-
  function(data_list = building_energy_data, .selected_ctu = "all") {
    if (.selected_ctu == "all") {
      return(data_list)
    }
    filtered_list <- lapply(data_list, function(df) {
      if ("ctu_name" %in% colnames(df)) {
        df <- df[df$ctu_name == .selected_ctu,]
      }
      df
    })
    filtered_list
  }
