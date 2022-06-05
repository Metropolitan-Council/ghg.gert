#' @title Check Input Parameters
#' @family Transportation
#'
#' @param name parameter name
#' @param value parameter value
#'
#' @return Error if values do not pass
#' @export
#'
#' @examples
#'
#' check_inputs("electric_scenario", "ER")
#' check_inputs("transit_avo_pct", 0)
check_inputs <- function(name, value) {
  if (name == "electric_scenario") {
    if (!value %in% c("ER", "EM")) {
      stop("Enter a valid electricity scenario: 'ER' or 'EM'.")
    }
  } else if (name == "aeo_scenario") {
    if (!value %in% c(
      "REF", "HM", "HOGS",
      "LM", "HP", "LP", "LOGS"
    )) {
      stop("Enter a valid aeo scenario: 'REF', 'HM', 'LM', 'HP', 'LP','HOGS', or 'LOGS'")
    }
  } else if (name %in% c("transit_avo_pct")) {
    if (!is.numeric(value)) {
      stop("Enter a valid transit AVO value between 0 and 500")
    } else if (value < 0 | value > 500) {
      stop("Enter a valid transit AVO value between 0 and 500")
    }
  } else if (name %in% c(
    "vmt_fee",
    "payd_fee",
    "freight_vmt_fee"
  )) {
    if (value > 1) {
      stop(paste("Enter a valid", name, "value between 0 and 1 dollars per mile"))
    }
  } else if (name %in% c("parking_price")) {
    if (value > 200 | value < 0) {
      stop("Enter a valid parking price between 0 and 200 dollars per hour")
    }
  } else if (name %in% c(
    "bev_pct_sales",
    "hev_pct_sales",
    "phev_pct_sales",
    "drs_pct_trip",
    "transit_rider_pct",
    "av_pct",
    "drs_pct",
    "emp_dens_pct_change",
    "pop_dens_pct_change",
    "job_access_pct_change",
    "land_use_pct_change",
    "transit_dist_pct_change",
    "comb_5d_impact_pct_change",
    "telework_pct"
  )) {
    if (value > 1 | value < -1) {
      stop(paste("Enter a valid", name, "value between -1 and 1"))
    }
  } else {
    return()
  }
}
