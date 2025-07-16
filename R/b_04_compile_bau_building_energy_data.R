#' @title Compile Building Energy Data
#'
#' @param tb Tibble.
#' @inheritParams calc_parking_lot_land_cover
#' @inheritParams run_all_modules
#' @inheritParams run_scenario_land_use
#' @inheritParams filter_ctu
#'
#' @export
compile_bau_building_energy <-
  function(tb = building_energy_data, .selected_ctu = "all") {
    # cli::cli_progress_message("* compiling building energy data \n")

    building_data <- c()

    building_data$residential <- bind_rows(
      building_energy_data$ctu_mfh,
      building_energy_data$ctu_sfh_attached,
      building_energy_data$ctu_sfh_large_lot,
      building_energy_data$ctu_sfh_small_lot
    )

    building_data$non_residential <- bind_rows(
      building_energy_data$commercial_jobs,
      building_energy_data$industrial_jobs
    )

    return(building_data)
  }
