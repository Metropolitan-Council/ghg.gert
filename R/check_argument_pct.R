#' Title
#'
#' @param arg
#' @param left
#' @param right
#'
#' @return
#' @export
#'
#' @examples
check_argument_pct <- function(arg, left, right) {

  arg_name <- deparse(substitute(arg))

  if (dplyr::between(arg, left, right) == FALSE) {
    stop(paste("argument", arg_name, "must be between", print(left), print(right)))
  }
}
