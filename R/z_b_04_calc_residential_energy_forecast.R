#' @title DEPRECATED Calculate Residential Energy Forecast
#' @export
#' @family deprecated
calc_residential_energy_forecast <-
  function(tb = building_energy_data, .selected_ctu = "all") {
    # cli::cli_progress_message("* calculating residential energy forecast \n")

    ctu_residential_energy_baseline <- get_residential_energy_baseline(.selected_ctu = .selected_ctu)
    ctu_characteristics_forecast <- calc_demographic_forecast(.selected_ctu = .selected_ctu)$ctu

    # RESIDENTIAL ENERGY FORECAST ----

    ## ----- estimate residential electricity use from floor area energy intensity -----
    ctu_residential_electricity_forecast <-
      ctu_residential_energy_baseline %>%
      dplyr::filter(var == "residential_kwh_per_floor_area") %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(year)) %>%
      dplyr::bind_rows(ctu_characteristics_forecast %>%
        dplyr::select(-c(year)) %>%
        dplyr::filter(
          var %in% c(
            "single_family_units",
            "single_family_average_floor_area_sqft_ctu",
            "multifamily_units",
            "multifamily_average_floor_area_sqft_county"
          )
        )) %>%
      tidyr::pivot_wider(
        names_from = "var",
        values_from = "value",
        values_fn = mean
      ) %>%
      dplyr::mutate(
        residential_kwh_per_floor_area = residential_kwh_per_floor_area * 0.8,
        total_residential_kwh_forecast = (((
          single_family_units * single_family_average_floor_area_sqft_ctu
        ) +
          (
            multifamily_units * multifamily_average_floor_area_sqft_county
          )
        ) * residential_kwh_per_floor_area),
        year = 2040
      ) %>%
      dplyr::select(
        geog_name, geog_id,
        year,
        residential_kwh_per_floor_area,
        total_residential_kwh_forecast
      ) %>%
      tidyr::pivot_longer(
        cols = c(
          "residential_kwh_per_floor_area",
          "total_residential_kwh_forecast"
        ),
        names_to = "var"
      )


    ## ----- estimate residential natural gas use from floor area energy intensity -----
    residential_natural_gas_forecast_ctu <-
      ctu_residential_energy_baseline %>%
      dplyr::filter(var == "residential_therms_per_floor_area") %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(year)) %>%
      dplyr::bind_rows(., ctu_characteristics_forecast %>%
        dplyr::select(-c(year))) %>%
      dplyr::group_by(geog_name, geog_id, var) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = "var", values_from = "value") %>%
      # assumption that natural gas per floor area stays static
      dplyr::mutate(residential_therms_per_floor_area = residential_therms_per_floor_area * 1) %>%
      dplyr::mutate(
        total_residential_therms_forecast = (((
          single_family_units * single_family_average_floor_area_sqft_ctu
        ) +
          (
            multifamily_units * multifamily_average_floor_area_sqft_county
          )
        ) * residential_therms_per_floor_area)
      ) %>%
      dplyr::mutate(year = 2040) %>%
      dplyr::select(
        geog_name, geog_id,
        year,
        residential_therms_per_floor_area,
        total_residential_therms_forecast
      ) %>%
      tidyr::pivot_longer(
        cols = c(
          "residential_therms_per_floor_area",
          "total_residential_therms_forecast"
        ),
        names_to = "var"
      )


    ## ---- compile residential energy use forecast ----
    ctu_residential_energy_forecast <-
      dplyr::bind_rows(
        ctu_residential_electricity_forecast,
        residential_natural_gas_forecast_ctu
      ) %>%
      dplyr::ungroup()

    return(ctu_residential_energy_forecast)
  }
