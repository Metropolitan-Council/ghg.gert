#' @title Calculate Scenario Building Non-Residential
#'
#' @return
#' @export
scen_building_non_residential <-
  function(tb = building_data$non_residential) {

    #B.C1
    tb01 <- calc_existing_comm_building_efficiency(non_res_tb = tb,
                                                   .existing_high_efficiency_buildings_pct = 0.8)
    #B.C4
    calc_electrify_commercial_heating()

  }
