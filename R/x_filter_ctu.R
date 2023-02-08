#' @title Filter CTU
#'
#' @param df table
#' @param .selected_ctu character, selected city. Default is `"all"`.
#'
#' @return tibble. Input df with only selected CTU
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
#' @inheritParams filter_ctu
#'
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
