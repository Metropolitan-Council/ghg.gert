#' @title Calculate existing commercial buildings efficiency
#' @family commercial-industrial
#' @family buildings
#'
#' @description Helps to calculate the reduced
#'      emissions from implementing high energy efficiency retrofits to existing buildings.
#'      Given that the main determinant of energy efficiency is the number of workers, this
#'      function reduces the number of workers.
#'      For more details, see `vignette("building_energy_module_outputs_non_residential")`
#'
#' @param .existing_high_efficiency_buildings_pct numeric,
#'      a value between `0` and 1.
#'      Percent of existing commercial buildings are LEED Gold.
#'      Default is `0.8`.
#'
#' @return [tibble::tibble()].
#'      The adjusted values for the non residential input table to reflect commercial building
#'      energy efficiency.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' calc_existing_comm_building_efficiency(
#'   non_res_tb = building_energy_bau_data$non_residential,
#'   .selected_ctu = "all",
#'   .existing_high_efficiency_buildings_pct = 0.80
#' )
#' }
calc_existing_comm_building_efficiency <- function(non_res_tb = building_energy_bau_data$non_residential,
                                                   .selected_ctu,
                                                   .existing_high_efficiency_buildings_pct) {
  cat("*** calculating existing commercial building energy efficiency \n")
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
        * commercial_jobs.2018
          * 0.25,
      value = commercial_jobs.2040 - reduction_energy_use_intensity,
      var = "commercial_jobs",
      year = 2040
    ) %>%
    dplyr::select(ctu_name, year, var, value) %>%
    dplyr::bind_rows(
      .,
      non_res_tb %>%
        dplyr::filter(
          var != ("commercial_jobs"),
          year == 2040
        )
    ) %>%
    dplyr::bind_rows(., non_res_tb %>%
      dplyr::filter(year == 2018))

  return(new_non_res_tb)
}
