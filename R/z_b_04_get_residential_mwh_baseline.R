#' @title DEPRECATED Get Residential Electricity Baseline
#'
#' @family buildings
#' @family residential
#' @family emissions
#' @family deprecaetd
#'
#' @description This function calculates the residential electricity baseline by
#'    processing building type data and building type electricity demand data for
#'    the specified CTU.
#'
#' @param tb A data frame containing building energy data (default is building_energy_data).
#' @inheritParams filter_ctu
#' @return A data frame containing residential energy baseline data, including electricity
#' consumption, natural gas consumption, CO2 emissions, and energy intensity per square
#' foot and per household for each community in the specified CTU.
#'
#' @export
#' @importFrom dplyr case_when mutate select group_by
#' @importFrom tidyr pivot_wider pivot_longer
get_residential_mwh_baseline <-
  function(tb = building_energy_data,
           .selected_ctu = "all",
           mwh_coef = mwh_coefficients) {
    # cli::cli_progress_message("* estimating residential energy baseline \n")

    tb <- filter_building_energy_data(data_list = tb, .selected_ctu = .selected_ctu)

    ctu_characteristics <- bind_rows(
      tb$ctu_mfh,
      tb$ctu_sfh_attached,
      tb$ctu_sfh_large_lot,
      tb$ctu_sfh_small_lot
    )

    # RESIDENTIAL ENERGY BASELINE -----
    ## ----- get electricity by ctu from 'Emissions' ------
    electricity_residential_ctu_baseline <-
      ctu_characteristics %>%
      left_join(mwh_coefficients,
        by = c("sp_categories" = "var")
      ) %>%
      dplyr::mutate(
        mwh_delta = mwh_per_unit * value_change_from_base,
        mwh = mwh_per_unit * value
      ) %>%
      dplyr::group_by(geog_name, geog_id, inventory_year) %>%
      summarize(
        mwh_residential = sum(mwh),
        mwh_residential_delta = sum(mwh_delta)
      )

    return(electricity_residential_ctu_baseline)
  }
