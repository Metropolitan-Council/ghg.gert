#' @title Execute waste scenarios
#' @family waste
#'
#' @description This function generates the outputs of the waste module
#'    for the given scenario at the city/township level. It incorporates various
#'    parameters to evaluate and analyze different waste scenarios for landfill,
#'    organics, recycling, and waste to energy. Outputs are
#'    provided as a tibble with columns `inventory_year`, `geog_id`, `geog_name`,
#'       `geog_level`, `source`, `value_activity`, `units_activity`, `data_type`,
#'       `sector`, `category`, `data_source`, `factor_source`, `value_emissions`,
#'       and `units_emissions`.
#'
#' @inheritParams filter_ctu
#' @inheritParams calculate_waste_reduction
#' @inheritParams divert_to_recycling
#' @inheritParams divert_to_organics
#' @inheritParams calculate_landfill_emissions
#' @inheritParams calculate_incin_emissions
#' @inheritParams calculate_organic_emissions
#'
#'
#' @return [tibble::tibble()].
#'       Returns a table with columns `inventory_year`, `geog_id`, `geog_name`,
#'       `geog_level`, `source`, `value_activity`, `units_activity`, `data_type`,
#'       `sector`, `category`, `data_source`, `factor_source`, `value_emissions`,
#'       and `units_emissions`.
#'       The table is the output of the waste module, any modification to
#'       the inputs of the waste module must be specified as an argument
#'       to the function `run_module_waste()`
#'
#' @export
#' @importFrom cli cli_progress_message
#'
run_module_waste <- function(tb_inv = waste_data$inventory,
                             tb_future = waste_data$projections,
                             tb_base = waste_data$baseline,
                             tb_char = waste_data$characterization,
                             tb_target = waste_data$mpca,

                             .selected_ctu = "all",

                             # user inputs below
                             .waste_reduction_pct = 0,
                             .waste_reduction_start = 2025,
                             .waste_reduction_end = 2050,

                             .diverted_to_recycle_pct = 0,
                             .diverted_to_recycle_start = 2025,
                             .diverted_to_recycle_end = 2050,

                             .diverted_to_organics_pct = 0,
                             .diverted_to_organics_start = 2025,
                             .diverted_to_organics_end = 2050,

                             .diverted_to_wte_pct = 0,
                             .diverted_to_wte_start = 2025,
                             .diverted_to_wte_end = 2050,

                             .methane_recovery_pct = 0,
                             .methane_recovery_start = 2025,
                             .methane_recovery_end = 2050,

                             .anaerobic_digestion_pct = 0,
                             .anaerobic_digestion_start = 2025,
                             .anaerobic_digestion_end = 2050

){
  tb_inv <- filter_ctu(tb_inv, .selected_ctu = .selected_ctu)
  tb_future <- filter_ctu(tb_future, .selected_ctu = .selected_ctu)
  tb_base <- filter_ctu(tb_base, .selected_ctu = .selected_ctu)



  l_names <- c(
    "waste_reduction_pct",
    "waste_reduction_start",
    "waste_reduction_end",
    "diverted_to_recycle_pct",
    "diverted_to_recycle_start",
    "diverted_to_recycle_end",
    "diverted_to_organics_pct",
    "diverted_to_organics_start",
    "diverted_to_organics_end",
    "diverted_to_wte_pct",
    "diverted_to_wte_start",
    "diverted_to_wte_end",
    "methane_recovery_pct",
    "methane_recovery_start",
    "methane_recovery_end",
    "anaerobic_digestion_pct",
    "anaerobic_digestion_start",
    "anaerobic_digestion_end"
  )

  l_vals <- list(
    .waste_reduction_pct,
    .waste_reduction_start,
    .waste_reduction_end,
    .diverted_to_recycle_pct,
    .diverted_to_recycle_start,
    .diverted_to_recycle_end,
    .diverted_to_organics_pct,
    .diverted_to_organics_start,
    .diverted_to_organics_end,
    .diverted_to_wte_pct,
    .diverted_to_wte_start,
    .diverted_to_wte_end,
    .methane_recovery_pct,
    .methane_recovery_start,
    .methane_recovery_end,
    .anaerobic_digestion_pct,
    .anaerobic_digestion_start,
    .anaerobic_digestion_end
  )


  purrr::map2(l_names, l_vals, check_inputs)


  # Module 1 - Waste reduction
  if (.waste_reduction_pct == 0) {
    tb_future <- tb_future
  } else {
    tb_future <- ghg.ccap::calculate_waste_reduction(
      waste_tb = tb_future,
      .waste_reduction_pct = .waste_reduction_pct,
      .waste_reduction_start = .waste_reduction_start,
      .waste_reduction_end = .waste_reduction_end
    )
  }


  # Module 2 - Source diversion: Landfills to Recycling
  if (.diverted_to_recycle_pct == 0) {
    tb_future <- tb_future
  } else {
    tb_future <- ghg.ccap::divert_to_recycling(
      waste_tb = tb_future,
      .diverted_to_recycle_pct = .diverted_to_recycle_pct,
      .diverted_to_recycle_start = .diverted_to_recycle_start,
      .diverted_to_recycle_end = .diverted_to_recycle_end
    )
  }


  # Module 3 - Source diversion: Landfills to Organics
  if (.diverted_to_organics_pct == 0) {
    tb_future <- tb_future
  } else {
    tb_future <- ghg.ccap::divert_to_organics(
      waste_tb = tb_future,
      .diverted_to_organics_pct = .diverted_to_organics_pct,
      .diverted_to_organics_start = .diverted_to_organics_start,
      .diverted_to_organics_end = .diverted_to_organics_end
    )
  }



  # calculate landfill emissions
  landfill_emis <- calculate_landfill_emissions(
    waste_inv = tb_inv,
    waste_future = tb_future,
    waste_char = tb_char,
    .methane_recovery_pct = .methane_recovery_pct,
    .methane_recovery_start = .methane_recovery_start,
    .methane_recovery_end = .methane_recovery_end
  )

  incin_emis <- calculate_incin_emissions(
    waste_inv = tb_inv,
    waste_future = tb_future
  )

  organic_emis <- calculate_organic_emissions(
    waste_inv = tb_inv,
    waste_future = tb_future,
    .anaerobic_digestion_pct = .anaerobic_digestion_pct,
    .anaerobic_digestion_start = .anaerobic_digestion_start,
    .anaerobic_digestion_end = .anaerobic_digestion_end,
    .methane_recovery_pct = .methane_recovery_pct,
    .methane_recovery_start = .methane_recovery_start,
    .methane_recovery_end = .methane_recovery_end
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



  compile_waste_emis <- function(df) {
    df %>%
      arrange(inventory_year, source, units_emissions) %>%
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
        co2_co2e = `Metric tons CO2`,
        value_emissions = ch4_co2e + n2o_co2e + co2_co2e,
        units_emissions = "Metric tons CO2e"
      ) %>% ungroup() %>%
      dplyr::select(
        -c(
          `Metric tons CH4`,
          `Metric tons CO2`,
          `Metric tons N2O`,
          ch4_co2e,
          n2o_co2e,
          co2_co2e,
          id
        )
      ) %>%
      group_by(inventory_year, source) %>%
      summarize(
        value_emissions = sum(value_emissions)
      ) %>%
      ungroup() %>%
  mutate(
    units_emissions = "Metric tons CO2e",
    sector = "Waste",
    category = "Solid waste",
    data_source = "MPCA SCORE Report",
    factor_source = "IPCC solid waste methodology"
  )
  }


  waste_emissions <- list()


  waste_emissions$activity$inv <- tb_inv
  waste_emissions$activity$future <- tb_future

  waste_emissions$emissions$inv <- landfill_emis$inv %>%
    dplyr::bind_rows(incin_emis$inv, organic_emis$inv) %>%
    compile_waste_emis()

  waste_emissions$emissions$future <- landfill_emis$future %>%
    dplyr::bind_rows(incin_emis$future, organic_emis$future) %>%
    compile_waste_emis()


  return(waste_emissions)
}
