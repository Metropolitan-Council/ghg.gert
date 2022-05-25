#' @title Calculate Existing Commercial Buildings Efficiency
#' @family building_energy_module
#'
#' @description `calc_existing_comm_building_efficiency` helps to calculate the reduced
#' emissions from impementing high energy efficiency retrofits to existing buildings.
#' Given that the main determinant of energy efficiency is the number of workers, this
#' function reduces the number of workers.
#'
#' @param non_res_tb
#' @param .existing_high_efficiency_buildings_pct Numeric. A number between `0` and 1.
#'      Percent of existing commercial buildings are LEED Gold
#'      Default is `0.8`
#'
#' @return A tibble.
#' @export
#'
#' @examples
calc_existing_comm_building_efficiency <-
  function(non_res_tb = building_data$non_residential,
           .existing_high_efficiency_buildings_pct) {
    new_non_res_tb <-
      non_res_tb %>%
      dplyr::filter(var %in% c("commercial_jobs")) %>%
      tidyr::pivot_wider(
        names_from = c(var, year),
        values_from = value,
        names_sep = "."
      ) %>%
      dplyr::mutate(
        reduction_energy_use_intensity =
          .existing_high_efficiency_buildings_pct
        *  commercial_jobs.2018
        *  0.25,
        value = commercial_jobs.2040 - reduction_energy_use_intensity,
        var = "commercial_jobs",
        year = 2040
      ) %>%
      select(ctu_name, year, var, value) %>%
      bind_rows(.,
                non_res_tb %>%
                  filter(var != "commercial_jobs" &
                           year == 2040)) %>%
      bind_rows(., non_res_tb %>%
                  filter(year == 2018))

    return(new_non_res_tb)

  }
