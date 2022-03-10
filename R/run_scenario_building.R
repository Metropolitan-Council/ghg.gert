#' Run building energy scenarios
#'
#' @inheritParams scen_building_residential
#' @inheritParams scen_building_commercial
#' @inheritParams scen_building_industrial
#'
#' @family buildings
#'
#' @return
#' @export
#'
run_scenario_building <- function(res_tb = building_data$residential,
                                  non_res_tb = building_data$non_residential){

  scen_building_residential()
  scen_building_commercial()
  scen_building_industrial()

}
