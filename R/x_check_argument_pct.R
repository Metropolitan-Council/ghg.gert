#' @title Check Argument Numeric Range
#' @description `check_argument_pct` checks that a given input argument is between a
#'      certain range.
#'
#' @param arg the object to be checked.
#' @param left **Numeric**.
#'      The smallest number allowed.
#' @param right **Numeric**.
#'      The largest number allowed.
#'
#' @return
#' @export
#'
check_argument_pct <- function(arg, left, right) {

  arg_name <- deparse(substitute(arg))

  if (dplyr::between(arg, left, right) == FALSE) {
    stop(paste("argument", arg_name, "must be between", print(left), print(right)))
  }
}
