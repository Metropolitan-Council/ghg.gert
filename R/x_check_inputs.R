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
  # Groups checked once at function start, not re-evaluated each branch
  pct_neg1_to_1 <- c(
    "bev_pct_sales", "hev_pct_sales", "bev_pct_stock", "hev_pct_stock",
    "transit_service_pct", "emp_dens_pct_change", "pop_dens_pct_change",
    "job_access_pct_change", "land_use_pct_change", "transit_dist_pct_change",
    "comb_5d_impact_pct_change", "telework_pct"
  )

  pct_0_to_1 <- c(
    "electrified_buildings_pct", "non_res_natural_gas_for_water_heating_pct",
    "non_res_natural_gas_for_space_heating_pct", "commercial_smart_grid_pct",
    "industrial_smart_grid_pct", "smart_grid_energy_reduction_pct",
    "new_homes_to_multifamily_pct", "existing_high_efficiency_buildings_pct",
    "home_behavior_change_pct", "single_family_floor_area_growth_pct",
    "new_homes_affected_pct", "new_sf_homes_leed_gold_pct",
    "new_mf_homes_leed_gold_pct", "existing_sf_retrofit_pct",
    "existing_mf_retrofit_pct", "single_family_heat_pump_pct",
    "multifamily_heat_pump_pct", "additional_electrified_residential_buildings_pct",
    "grid_decarbonization_pct", "parking_lot_reduction_percentage",
    "manure_management", "smart_fertilizer_application",
    "cover_crops", "no_till_agriculture"
  )

  year_2025_2045 <- c(
    "leed_start_year", "retrofit_start_year", "heatpump_start_year",
    "manure_management_start_year", "smart_fertilizer_start_year",
    "cover_crops_start_year", "no_till_start_year"
  )

  vmt_fees <- c("vmt_fee", "payd_fee", "freight_vmt_fee")

  # Handle grouped numeric range checks first (fast vector lookup)
  if (name %in% pct_0_to_1) {
    if (value > 1 | value < 0) {
      cli::cli_abort(paste("Enter a valid", name, "value between 0 and 1"))
    }
    return()
  }

  if (name %in% pct_neg1_to_1) {
    if ((!is.numeric(value)) | value > 1 | value < -1) {
      cli::cli_abort(paste("Enter a valid", name, "value between -1 and 1, not ", value))
    }
    return()
  }

  if (name %in% year_2025_2045) {
    if (value < 2025 | value > 2045) {
      cli::cli_abort(paste("Enter a valid", name, "value between 2025 and 2045"))
    }
    return()
  }

  if (name %in% vmt_fees) {
    if (!is.numeric(value) | value > 1) {
      cli::cli_abort(paste("Enter a valid", name, "value between 0 and 1 dollars per mile"))
    }
    return()
  }

  # switch() for exact-match cases — O(1) hashed lookup vs sequential if/else
  switch(name,
    electric_scenario = {
      if (!value %in% c("ER", "EM")) {
        cli::cli_abort("Enter a valid electricity scenario: 'ER' or 'EM'.")
      }
    },
    aeo_scenario = {
      if (!value %in% c("REF", "HM", "HOGS", "LM", "HP", "LP", "LOGS")) {
        cli::cli_abort("Enter a valid aeo scenario: 'REF', 'HM', 'LM', 'HP', 'LP','HOGS', or 'LOGS'")
      }
    },
    transit_avo_pct = {
      if (!is.numeric(value) | value < 0 | value > 500) {
        cli::cli_abort("Enter a valid transit AVO value between 0 and 500")
      }
    },
    parking_price = {
      if (value > 200 | value < 0) {
        cli::cli_abort("Enter a valid parking price between 0 and 200 dollars per hour, not ", value)
      }
    },
    mode = {
      valid_modes <- c("BU", "RU", "RI", "SUT", "CUT", "BIKE", "WALK", "BS", "FR", "PLDV", "MM", "AIR", "WAT")
      if (!value %in% valid_modes) {
        cli::cli_abort(c("Enter a valid mode", paste(valid_modes)))
      }
    },
    selected_ctu = {
      if (value != "all" && !value %in% c(unique(ghg.ccap::geog_index$geog_name), "Twin Cities Region", "CCAP Region")) {
        cli::cli_abort("Enter a valid geog_name name")
      }
    },
    fuel_type = {
      if (!value %in% unique(ghg.ccap::factor_values$ghg$source)) {
        cli::cli_abort("Enter a valid fuel type: ", paste0(unique(ghg.ccap::factor_values$ghg$source), collapse = ", "))
      }
    },
    miles_per_gallon = {
      if (!value %in% unique(ghg.ccap::fuel_economy$var)) {
        cli::cli_abort("Enter a valid miles per gallon: ", paste0(unique(ghg.ccap::fuel_economy$var), collapse = ", "))
      }
    }
    # default: do nothing (equivalent to your final `else return()`)
  )
}
