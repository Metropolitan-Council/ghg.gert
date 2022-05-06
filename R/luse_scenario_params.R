#' Title
#'
#' @param tb
#'
#' @return
#' @export
#'
#' @examples
luse_scenario_params <- function(tb = land_use_data,
                                 .scenario =  "bau") {
  tb$scenario_parameters %>%
    dplyr::filter(scenario_description_2 == .scenario)
}
