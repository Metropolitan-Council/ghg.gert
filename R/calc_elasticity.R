
#' @title Interpolate elasticities across years
#'
#' @param elas_list list, elasticities in each year included in scenario
#' @param elas elasticity value from literature for effect on VMT
#' @param num_inits TODO
#' @param num_yrs number of forecast years
#'
#' @return
#' @export
#'
#' @family transportation
calc_elasticity <- function(elas_list,
                            elas,
                            num_inits,
                            num_yrs) {

  # browser()
  for (i in 1:num_yrs) {
    elas_list[i + num_inits] <- (elas / num_yrs) * i
  }
  return(elas_list)
}
