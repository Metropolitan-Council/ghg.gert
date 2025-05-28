#' @title Get Residential Energy Baseline
#'
#' @family buildings
#' @family residential
#' @family emissions
#'
#' @description This function calculates the residential energy baseline by
#'    processing building energy data and demographic data for the specified CTU.
#'    It estimates residential electricity consumption, natural gas consumption,
#'    CO2 emissions, energy intensity per square foot (kWh/sqft and therms/sqft),
#'    and energy intensity per household (kWh/household and therms/household) for
#'    each community. The resulting data is returned in a combined format,
#'    including energy consumption, emissions, and energy intensity values.
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
get_residential_energy_baseline <-
  function(tb = building_energy_data, .selected_ctu = "all") {
    # cli::cli_progress_message("* estimating residential energy baseline \n")

    tb <- filter_building_energy_data(data_list = tb, .selected_ctu = .selected_ctu)

    ctu_characteristics <- get_demographic_baseline(.selected_ctu = .selected_ctu)$ctu

    # RESIDENTIAL ENERGY BASELINE -----
    ## ----- get electricity by ctu from 'Emissions' ------
    electricity_residential_ctu <-
      tb$electricity_residential_ctu %>%
      dplyr::mutate(
        residential_mwh = dplyr::case_when(
          (actual_residential_mwh > 1) ~ actual_residential_mwh,
          (is.na(actual_residential_mwh)) ~ est_residential_mwh
        ),
        residential_elec_emis_t_co2e = dplyr::case_when(
          (actual_residential_mwh > 1) ~ actual_residential_electricity_emis_t_co2e,
          (is.na(actual_residential_mwh)) ~ est_residential_electricity_emis_t_co2e
        )
      ) %>%
      dplyr::select(
        geog_name, geog_id,
        year,
        residential_mwh,
        residential_elec_emis_t_co2e
      ) %>%
      dplyr::group_by(geog_name, geog_id, year) %>%
      tidyr::pivot_longer(
        cols = c(
          "residential_mwh",
          "residential_elec_emis_t_co2e"
        ),
        names_to = "var"
      )

    ## ----- get nat gas by ctu from 'Emissions'------
    natural_gas_residential_ctu <-
      tb$natural_gas_residential_ctu %>%
      dplyr::mutate(
        residential_ng_therms = dplyr::case_when(
          (actual_residential_ng_therms > 1) ~ actual_residential_ng_therms,
          (is.na(actual_residential_ng_therms)) ~ est_residential_ng_therms
        ),
        residential_ng_emis_t_co2e = dplyr::case_when(
          (actual_residential_ng_therms > 1) ~ actual_residential_ng_emis_t_co2e,
          (is.na(actual_residential_ng_therms)) ~ est_residential_ng_emis_t_co2e
        )
      ) %>%
      dplyr::select(
        geog_name, geog_id,
        year,
        residential_ng_therms,
        residential_ng_emis_t_co2e
      ) %>%
      dplyr::group_by(geog_name, geog_id, year) %>%
      tidyr::pivot_longer(
        cols = c("residential_ng_therms", "residential_ng_emis_t_co2e"),
        names_to = "var"
      )

    ## ----- estimate residential kwh/sqft -----
    residential_kwh_per_sqft <-
      dplyr::bind_rows(
        electricity_residential_ctu,
        ctu_characteristics %>%
          dplyr::filter(
            var %in% c(
              "single_family_units",
              "single_family_average_floor_area_sqft_ctu",
              "multifamily_units",
              "multifamily_average_floor_area_sqft_county"
            )
          )
      ) %>%
      unique() %>%
      dplyr::group_by(geog_name, geog_id, year) %>%
      tidyr::pivot_wider(
        names_from = "var",
        values_from = "value",
        values_fn = mean
      ) %>%
      dplyr::mutate(residential_kwh_per_floor_area = (residential_mwh /
        ((single_family_units *
          single_family_average_floor_area_sqft_ctu) +
          (multifamily_units * multifamily_average_floor_area_sqft_county)
        )) *
        1000) %>%
      dplyr::select(geog_name, geog_id, year, residential_kwh_per_floor_area) %>%
      dplyr::group_by(geog_name, geog_id, year) %>%
      tidyr::pivot_longer(
        cols = c("residential_kwh_per_floor_area"),
        names_to = "var"
      )

    ## ----- estimate residential therms/sqft ----
    residential_therms_per_sqft <-
      dplyr::bind_rows(
        natural_gas_residential_ctu,
        ctu_characteristics
      ) %>%
      tidyr::pivot_wider(
        names_from = "var",
        values_from = "value",
        values_fn = mean
      ) %>%
      dplyr::mutate(residential_therms_per_floor_area = (residential_ng_therms /
        ((single_family_units *
          single_family_average_floor_area_sqft_ctu) +
          (multifamily_units * multifamily_average_floor_area_sqft_county)
        ))) %>%
      dplyr::select(geog_name, geog_id, year, residential_therms_per_floor_area) %>%
      dplyr::group_by(geog_name, geog_id, year) %>%
      tidyr::pivot_longer(
        cols = c("residential_therms_per_floor_area"),
        names_to = "var"
      )

    ## ---- estimate residential kwh/household -----
    residential_kwh_per_household <-
      dplyr::bind_rows(
        electricity_residential_ctu,
        ctu_characteristics
      ) %>%
      tidyr::pivot_wider(
        names_from = "var",
        values_from = "value",
        values_fn = mean
      ) %>%
      dplyr::mutate(residential_mwh_per_households = residential_mwh / households) %>%
      dplyr::select(geog_name, geog_id, year, residential_mwh_per_households) %>%
      dplyr::group_by(geog_name, geog_id, year) %>%
      tidyr::pivot_longer(
        cols = c("residential_mwh_per_households"),
        names_to = "var"
      )

    ## ----- estimate residnetial  therms/household ----
    residential_therms_per_household <-
      dplyr::bind_rows(
        natural_gas_residential_ctu,
        ctu_characteristics
      ) %>%
      tidyr::pivot_wider(
        names_from = "var",
        values_from = "value",
        values_fn = mean
      ) %>%
      dplyr::mutate(residential_therms_per_households = residential_ng_therms / households) %>%
      dplyr::group_by(geog_name, geog_id, year) %>%
      dplyr::select(geog_name, geog_id, year, residential_therms_per_households) %>%
      tidyr::pivot_longer(
        cols = c("residential_therms_per_households"),
        names_to = "var"
      )

    ## ---- return variables residential energy baseline -----
    ctu_residential_energy_baseline <-
      dplyr::bind_rows(
        electricity_residential_ctu,
        natural_gas_residential_ctu,
        residential_kwh_per_sqft,
        residential_therms_per_sqft,
        residential_kwh_per_household,
        residential_therms_per_household
      ) %>%
      dplyr::ungroup()

    return(ctu_residential_energy_baseline)
  }
