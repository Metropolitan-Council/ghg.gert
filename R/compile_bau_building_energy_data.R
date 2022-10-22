#' @title Compile Building Energy Data
#'
#' @param tb
#'
#' @return
#' @export
#'
#' @examples
compile_bau_building_energy <-
  function(tb = building_energy_data) {
    building_data <- c()

    demographic_baseline <- ghg.sp::get_demographic_baseline()$ctu
    demographic_forecast <- ghg.sp::calc_demographic_forecast()$ctu
    residential_energy_baseline <- ghg.sp::get_residential_energy_baseline(tb = tb)
    residential_energy_forecast <- ghg.sp::calc_residential_energy_forecast(tb = tb)
    non_residential_energy_baseline <- ghg.sp::get_non_residential_energy_baseline(tb = tb)
    non_residential_energy_forecast <- ghg.sp::calc_non_residential_energy_forecast(tb = tb)

    building_data$residential <- dplyr::bind_rows(
      demographic_baseline,
      demographic_forecast,
      residential_energy_baseline,
      residential_energy_forecast
    )

    building_data$non_residential <- dplyr::bind_rows(
      demographic_baseline,
      demographic_forecast,
      non_residential_energy_baseline,
      residential_energy_forecast
    )

    return(building_data)

  }

