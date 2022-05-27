#' @title Calculate Scenario Building Non-Residential
#'
#' @return
#' @export
scen_building_non_residential <-
  function(tb = building_data$non_residential) {

    tb01 <- calc_ghg_non_residential(non_res_tb = tb)

    tb02 <- calc_electrify_commercial_heating(tb = tb01)

    tb03 <- calc_non_res_renewable_ng(tb = tb02)

    return(tb03)

  }
