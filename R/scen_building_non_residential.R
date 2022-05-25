#' Title
#'
#' @return
#' @export
scen_building_non_residential <-
  function() {
    #B.C1
    calc_existing_comm_building_efficiency()
    #B.C2 + B.C3
    calc_non_res_smart_grid()
    #B.C4
    calc_electrify_commercial_heating()

  }
