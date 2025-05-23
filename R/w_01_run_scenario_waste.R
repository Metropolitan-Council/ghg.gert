#' @title Execute waste scenarios
#' @family waste
#'
#' @description This function generates the outputs of the waste module
#'    for the given scenario at the city/township level. It incorporates various
#'    parameters to evaluate and analyze different waste scenarios for landfill,
#'    organics, recycling, and waste to energy. Outputs are
#'    provided as a tibble with columns `ctu_name`, `var`, `scen`, `year`, and `value`.
#'
#' @inheritParams calc_landfill_emissions
#'
#' @return [tibble::tibble()].
#'       Returns a table with columns `ctu_name`, `var`, `scen`, `year`, and `value`.
#'       The table is the output of the waste module, any modification to
#'       the inputs of the waste module must be specified as an argument
#'       to the function `run_scenario_waste()`
#'
#' @export
#' @importFrom cli cli_progress_message
#'
run_scenario_waste <- function(waste_tb = waste_data$ctu,
                               waste_char = waste_data$characterization,
                               .selected_ctu = "all",
                               .methane_recovery_pct = 0,
                               .anaerobic_digestion_pct = 0
                               # later: add vars for waste reduction and source diversion
                               # .waste_reduction_pct = 0,
                               # .diverted_to_recycle_pct = 0,
                               # .diverted_to_organics_pct = 0,
                               # .diverted_to_wte_pct = 0
){
  waste_tb <- filter_ctu(waste_tb, .selected_ctu = .selected_ctu)

  l_names <- c(
    "methane_recovery_pct",
    "anaerobic_digestion_pct"
    # "waste_reduction_pct",
    # "diverted_to_recycle_pct",
    # "diverted_to_organics_pct"
    # "diverted_to_wte_pct"
  )

  l_vals <- list(
    .methane_recovery_pct,
    .anaerobic_digestion_pct
    # .waste_reduction_pct,
    # .diverted_to_recycle_pct,
    # .diverted_to_organics_pct,
    # .diverted_to_wte_pct
  )

  purrr::map2(l_names, l_vals, check_inputs)

  # modify waste activity projections:
  # first: is overall waste reduced?
  # is landfill waste diverted?
  # to recycling
  # to organics
  # to wte
  # return: modified activity data tb

  # calculate landfill emissions
  landfill_emis <- calculate_landfill_emissions(
    waste_tb = waste_tb,
    waste_char = waste_char,
    .methane_recovery_pct = .methane_recovery_pct
  )

  incin_emis <- calculate_incin_emissions(
    waste_tb = waste_tb
  )

  organic_emis <- calculate_organic_emissions(
    waste_tb = waste_tb,
    .anaerobic_digestion_pct = .anaerobic_digestion_pct,
    .methane_recovery_pct = .methane_recovery_pct
  )

  # define gwp - MOVE THIS TO A BETTER PLACE later
  gwp <-
    list(
      "co2" = 1,
      "ch4" = 27.9,
      "n2o" = 273,
      "cf4" = 7380,
      "HFC-152a" = 164
    )

  # bind emissions together and translate to mt co2e
  waste_emissions <- landfill_emis %>%
    dplyr::bind_rows(incin_emis, organic_emis) %>%
    #give each row a unique id to avoid pivoting error
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::group_by(id) %>%
    tidyr::pivot_wider(
      names_from = units_emissions,
      values_from = value_emissions
    ) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(
      ch4_co2e = `Metric tons CH4` * gwp$ch4,
      n2o_co2e = `Metric tons N2O` * gwp$n2o,
      sector = "Waste",
      category = "Solid waste",
      data_source = "MPCA SCORE Report",
      factor_source = "IPCC solid waste methodology"
    ) %>%
    dplyr::mutate(
      value_emissions = ch4_co2e + n2o_co2e + `Metric tons CO2`,
      units_emissions = "Metric tons CO2e"
    ) %>%
    dplyr::ungroup() %>%
    # select(
    #   geoid,
    #   inventory_year,
    #   sector,
    #   category,
    #   source,
    #   data_source,
    #   factor_source,
    #   value_activity,
    #   units_activity,
    #   value_emissions,
    #   units_emissions
    # )
    # the above doesn't work bc county uses geoid and ctu uses ctu_id
    dplyr::select(
      -c(
        `Metric tons CH4`,
        `Metric tons CO2`,
        `Metric tons N2O`,
        ch4_co2e,
        n2o_co2e,
        id
      )
    )

  return(waste_emissions)
}
