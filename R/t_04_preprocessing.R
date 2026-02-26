#' Interpolate treatments across years
#'
#' @param treat_list TODO
#' @param treat TODO
#' @param num_inits TODO
#' @param num_yrs number of forecast years
#'
#' @return [tibble::tibble()] with column names...
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



#'  Interpolate elasticities across years
#'
#' @param elas_list list, elasticities in each year included in scenario
#' @param elas elasticity value from literature for effect on VMT
#' @param num_inits TODO
#' @param num_yrs number of forecast years
#'
#' @return list
#' @export
#' @keywords internal
#'
#' @family transportation, pre-processing
calc_elasticity <- function(elas_list,
                            elas,
                            num_inits,
                            num_yrs) {
  # cli::cli_progress_message("* calculating elasticities \n")

  elas_list[(1:num_yrs) + num_inits] <- (elas / num_yrs) * (1:num_yrs)

  return(elas_list)
}
