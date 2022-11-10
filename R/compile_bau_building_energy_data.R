#' @title Compile Building Energy Data
#'
#' @param tb
#'
compile_bau_building_energy <-
  function(tb = building_energy_data, ctu_selection = "all") {
    building_data <- c()

    demographic_baseline <- ghg.sp::get_demographic_baseline()$ctu
    demographic_forecast <- ghg.sp::calc_demographic_forecast()$ctu
    residential_energy_baseline <- ghg.sp::get_residential_energy_baseline(tb = tb)
    residential_energy_forecast <- ghg.sp::calc_residential_energy_forecast(tb = tb)
    non_residential_energy_baseline <- ghg.sp::get_non_residential_energy_baseline(tb = tb)
    non_residential_energy_forecast <- ghg.sp::calc_non_residential_energy_forecast(tb = tb)


    if (ctu_selection == "all"){


      building_data$residential <- dplyr::bind_rows(
        demographic_baseline,
        demographic_forecast,
        residential_energy_baseline,
        residential_energy_forecast
      ) %>%
        dplyr::ungroup()

      building_data$non_residential <- dplyr::bind_rows(
        demographic_baseline,
        demographic_forecast,
        non_residential_energy_baseline,
        non_residential_energy_forecast
      ) %>%
        dplyr::ungroup()
    }
    else {
      building_data$residential <- dplyr::bind_rows(
        demographic_baseline,
        demographic_forecast,
        residential_energy_baseline,
        residential_energy_forecast
      ) %>% dplyr::filter(ctu_name == ctu_selection) %>%
        dplyr::ungroup()

      building_data$non_residential <- dplyr::bind_rows(
        demographic_baseline,
        demographic_forecast,
        non_residential_energy_baseline,
        non_residential_energy_forecast
      ) %>% dplyr::filter(ctu_name == ctu_selection) %>%
        dplyr::ungroup()

    }

    return(building_data)

  }

