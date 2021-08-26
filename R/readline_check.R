#' @title Check user inputs
#'
#' @param prompt user input
#' @param type type of question to determine the set of applied logic
#' @inheritParams adj_fleet_shares
#' @family transportation
#' @return
#' @export
#'
readline_check <- function(
  prompt,
  type,
  bev = 0,
  phev = 0
) {
  input <- readline(prompt)
  if (type == "elec_scen") {
    input <- toupper(input)
    if (input == "ER" | input == "EM") {
      return(input)
    } else {
      stop("Enter a valid electricity scenario ID ('ER' or 'EM').")
    }
  } else if (type == "aeo_scen") {
    input <- toupper(input)
    if (input == "REF" | input == "HM" | input == "HOGS" |
      input == "LM" | input == "LOGS" | input == "LP" | input == "HP") {
      return(input)
    } else {
      stop("Enter a valid electricity scenario ID ('ER' or 'EM').")
    }
  } else if (type == "ctu") {
    if (input == "Lake Elmo" | input == "Shoreview" | input == "St. Paul") {
      return(input)
    } else {
      stop("Enter a valid ctu name.")
    }
  }
  # Don't allow negative or unreasonable numbers for AVO or ridership increase on transit
  else if ((type == "transit_avo") | (type == "transit_rider")) {
    if ((suppressWarnings(!is.na(as.numeric(input)))) & (input >= 0) &
      (input < 500)) {
      return(as.numeric(input))
    } else {
      stop("Please enter a numeric value between 0 and 500.")
    }
  }
  # Don't allow negative payd/vmt prices or values above $1 per mile
  else if ((type == "vmt_price") | (type == "payd_ins") |
    (type == "fvmt_price")) {
    if ((suppressWarnings(!is.na(as.numeric(input)))) & (input >= 0) &
      (input < 100)) {
      return(as.numeric(input))
    } else {
      stop("Please enter a numeric value between 0 and 100.")
    }
  }
  # Park price
  else if (type == "park_price") {
    if ((suppressWarnings(!is.na(as.numeric(input)))) &
      (input >= 0) & (input < 20)) {
      return(as.numeric(input))
    } else {
      stop("Please enter a numeric value between 0 and 20.")
    }
  }
  # BEV/PHEV/HEV
  else if ((type == "bev_share") | (type == "phev_share") |
    (type == "hev_share")) {
    if ((suppressWarnings(!is.na(as.numeric(input)))) & (input >= 0)) {
      bev <- as.numeric(bev)
      phev <- as.numeric(phev)
      if (((type == "bev_share") &
        (input < 90)) |
        ((type == "phev_share") &
          (input < 90 - bev)) | ((type == "hev_share") &
        (input < 90 - bev - phev))) {
        return(as.numeric(input))
      } else {
        stop("Values will not add to less than 90 for bev, phev, and hev.")
      }
    } else {
      stop("Please enter a numeric value between 0 and 90.")
    }
  }
  # shared/automated
  else if ((type == "shared") | (type == "automated")) {
    if ((suppressWarnings(!is.na(as.numeric(input)))) &
      (input >= 0) & (input <= 100)) {
      return(as.numeric(input))
    } else {
      stop("Please enter a numeric value between 0 and 100.")
    }
  } else {
    stop("Unrecongized adjustment parameter. Error!")
  }
}
