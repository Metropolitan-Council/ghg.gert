#' @title Filter CTU
#'
#' @param df Tibble. A data frame to filter by CTU
#' @param .selected_ctu Character. A string that indicates whether to use all ctus available
#' or only one selected ctu.
#'      default is "all"
#'
#' @return Tibble. a filter version of the input dataframe.
#' @export
#'
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



#' @title Filter Building Energy Data
#'
#' @param df
#'
#' @return Tibble. Returns a filtered version of the input dataset.
#' @export
#'
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
