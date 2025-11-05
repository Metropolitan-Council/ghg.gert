#' @title Check input parameters
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
      cli::cli_abort("Enter a valid electricity scenario: 'ER' or 'EM'.")
    }
  } else if (name == "aeo_scenario") {
    if (!value %in% c(
      "REF", "HM", "HOGS",
      "LM", "HP", "LP", "LOGS"
    )) {
      cli::cli_abort("Enter a valid aeo scenario: 'REF', 'HM', 'LM', 'HP', 'LP','HOGS', or 'LOGS'")
    }
  } else if (name %in% c("transit_avo_pct")) {
    if (!is.numeric(value)) {
      cli::cli_abort("Enter a valid transit AVO value between 0 and 500")
    } else if (value < 0 | value > 500) {
      cli::cli_abort("Enter a valid transit AVO value between 0 and 500")
    }
  } else if (name %in% c(
    "vmt_fee",
    "payd_fee",
    "freight_vmt_fee"
  )) {
    if (!is.numeric(value)) {
      cli::cli_abort(paste("Enter a valid", name, "value between 0 and 1 dollars per mile"))
    }
    if (value > 1) {
      cli::cli_abort(paste("Enter a valid", name, "value between 0 and 1 dollars per mile"))
    }
  } else if (name %in% c("parking_price")) {
    if (value > 200 | value < 0) {
      cli::cli_abort("Enter a valid parking price between 0 and 200 dollars per hour, not ", value)
    }
  } else if (name %in% c(
    "bev_pct_sales",
    "hev_pct_sales",
    "bev_pct_stock",
    "hev_pct_stock",
    "transit_service_pct",
    "emp_dens_pct_change",
    "pop_dens_pct_change",
    "job_access_pct_change",
    "land_use_pct_change",
    "transit_dist_pct_change",
    "comb_5d_impact_pct_change",
    "telework_pct"
  )) {
    if ((!is.numeric(value)) | value > 1 | value < -1) {
      cli::cli_abort(paste("Enter a valid", name, "value between -1 and 1, not ", value))
    }
  } else if (name %in% c(
    # non-residential
    # electrification
    "electrified_buildings_pct",
    "non_res_natural_gas_for_water_heating_pct",
    "non_res_natural_gas_for_space_heating_pct",
    # smartgrid
    "commercial_smart_grid_pct",
    "industrial_smart_grid_pct",
    "smart_grid_energy_reduction_pct",
    # residential
    # floor_area
    "new_homes_to_multifamily_pct",
    "existing_high_efficiency_buildings_pct",
    "home_behavior_change_pct",
    "single_family_floor_area_growth_pct",
    "new_homes_affected_pct",
    "new_sf_homes_leed_gold_pct",
    "new_mf_homes_leed_gold_pct",
    "existing_sf_retrofit_pct",
    "existing_mf_retrofit_pct",
    # electrification
    "single_family_heat_pump_pct",
    "multifamily_heat_pump_pct",
    "additional_electrified_residential_buildings_pct",
    # grid
    "grid_decarbonization_pct",
    "parking_lot_reduction_percentage"
  )) {
    if (value > 1 | value < 0) {
      cli::cli_abort(paste("Enter a valid", name, "value between 0 and 1"))
    }
  } else if (name %in% c(
    "leed_start_year",
    "retrofit_start_year",
    "heatpump_start_year"
  )) {
    if (value < 2025 | value > 2045) {
      cli::cli_abort(paste("Enter a valid", name, "value between 2025 and 2045"))
    }
  } else if (name == "mode") {
    if (!value %in% c(
      "BU",
      # "BRT",
      "RU", "RI",
      "SUT", "CUT", "BIKE", "WALK",
      "BS", "FR", "PLDV",
      "MM", "AIR", "WAT"
    )) {
      cli::cli_abort(c(
        "Enter a valid mode",
        paste(
          "BU",
          # "BRT",
          "RU", "RI",
          "SUT", "CUT", "BIKE", "WALK",
          "BS", "FR", "PLDV",
          "MM", "AIR", "WAT"
        )
      ))
    }
  } else if (name == "selected_ctu") {
    if (value == "all") {
      return()
    } else if (!value %in% c(
      unique(ghg.ccap::geog_index$geog_name),
      "Twin Cities Region",
      "CCAP Region"
    )) {
      cli::cli_abort(c(
        "Enter a valid geog_name name"
      ))
    }
  } else if (name == "fuel_type") {
    if (!value %in% unique(ghg.ccap::factor_values$ghg$source)) {
      cli::cli_abort("Enter a valid fuel type: ", paste0(unique(ghg.ccap::factor_values$ghg$source), collapse = ", "))
    }
  } else if (name == "miles_per_gallon") {
    if (!value %in% unique(ghg.ccap::fuel_economy$var)) {
      cli::cli_abort("Enter a valid miles per gallon: ", paste0(unique(ghg.ccap::fuel_economy$var), collapse = ", "))
    }
  } else {
    return()
  }
}
