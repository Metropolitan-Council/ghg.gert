#' @title Interpolate treatments across years
#'
#' @param treat_list TODO
#' @param treat TODO
#' @param num_inits TODO
#' @param num_yrs number of forecast years
#'
#' @return
#' @export
#' @family transportation
#'
calc_treatment <- function(treat_list,
                           treat,
                           num_inits,
                           num_yrs) {
  for (i in 1:num_yrs) {
    treat_list[i + num_inits] <- (treat / num_yrs) * i
  }
  return(treat_list)
}
