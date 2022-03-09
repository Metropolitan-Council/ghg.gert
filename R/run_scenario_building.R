#' Run building energy scenarios
#'
#' @param res_tb
#' @param non_res_tb
#'
#' @return
#' @export
#'
#' @examples
run_scenario_building <- function(res_tb = building_data$residential,
                                  non_res_tb = building_data$non_residential){

  scen_building_residential()


  scen_building_commercial()
  scen_building_industrial()

}
