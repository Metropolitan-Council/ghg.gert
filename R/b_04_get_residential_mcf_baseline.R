#' @title Get Residential Natural Gas Baseline
#'
#' @family buildings
#' @family residential
#' @family emissions
#'
#' @description This function calculates the residential natural gas baseline by
#'    processing building type data and building type natural gas demand data for
#'    the specified CTU.
#'
#' @param tb A data frame containing building energy data (default is building_energy_data).
#' @inheritParams filter_ctu
#' @return A data frame containing residential energy baseline data,
#' natural gas consumption, CO2 emissions, and energy intensity per square
#' foot and per household for each community in the specified CTU.
#'
#' @export
#' @importFrom dplyr case_when mutate select group_by
#' @importFrom tidyr pivot_wider pivot_longer
get_residential_mcf_baseline <-
  function(tb = building_energy_data,
           .selected_ctu = "all",
           mcf_coef = mcf_coefficients) {
    # cli::cli_progress_message("* estimating residential energy baseline \n")

    geog_tb <- filter_building_energy_data(data_list = tb, .selected_ctu = .selected_ctu)

    ctu_characteristics <- bind_rows(
      geog_tb$ctu_mfh,
      geog_tb$ctu_sfh_attached,
      geog_tb$ctu_sfh_large_lot,
      geog_tb$ctu_sfh_small_lot
    )

    # RESIDENTIAL NATURAL GAS BASELINE -----
    ## ----- get nat gas by ctu from 'Emissions' ------
    nat_gas_residential_ctu_baseline <-
      ctu_characteristics %>%
      left_join(mcf_coefficients,
        by = c("sp_categories" = "var")
      ) %>%
      dplyr::mutate(
        mcf_delta = mcf_per_unit_eia * value_change_from_base,
        mcf = mcf_per_unit_eia * value
      ) %>%
      dplyr::group_by(geog_name, geog_id, inventory_year) %>%
      summarize(
        mcf_residential = sum(mcf),
        mcf_residential_delta = sum(mcf_delta)
      )

    return(electricity_residential_ctu_baseline)
  }
