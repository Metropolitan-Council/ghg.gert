#' @title Compile Building Energy Data
#'
#' @param tb Tibble.
#' @export
compile_bau_building_energy <-
  function(tb = building_energy_data, .selected_ctu = "all") {

    cli::cli_progress_message("* compiling building energy data \n")

    building_data <- c()

    demographic_baseline <- ghg.sp::get_demographic_baseline(.selected_ctu = .selected_ctu)$ctu
    demographic_forecast <- ghg.sp::calc_demographic_forecast(.selected_ctu = .selected_ctu)$ctu
    residential_energy_baseline <- ghg.sp::get_residential_energy_baseline(tb = tb, .selected_ctu = .selected_ctu)
    residential_energy_forecast <- ghg.sp::calc_residential_energy_forecast(tb = tb, .selected_ctu = .selected_ctu)
    non_residential_energy_baseline <- ghg.sp::get_non_residential_energy_baseline(tb = tb, .selected_ctu = .selected_ctu)
    non_residential_energy_forecast <- ghg.sp::calc_non_residential_energy_forecast(tb = tb, .selected_ctu = .selected_ctu)


    if (.selected_ctu == "all") {
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
    } else {
      building_data$residential <- dplyr::bind_rows(
        demographic_baseline,
        demographic_forecast,
        residential_energy_baseline,
        residential_energy_forecast
      ) %>%
        dplyr::filter(ctu_name == .selected_ctu) %>%
        dplyr::ungroup()

      building_data$non_residential <- dplyr::bind_rows(
        demographic_baseline,
        demographic_forecast,
        non_residential_energy_baseline,
        non_residential_energy_forecast
      ) %>%
        dplyr::filter(ctu_name == .selected_ctu) %>%
        dplyr::ungroup()
    }

    return(building_data)
  }
