#' @title Calculate Commercial Electricity Use with Smart Grid
#' @family building_energy_module
#'
#' @description `calc_smart_grid_comm_electr_use` calculates the impact of smart grid
#' implementation on the commercial building sector.
#'
#' @param .comm_buildings_in_smart_grid_pct
#'
#' @return Tibble.
#'      A table with the resulting electricity use in MWH/year by city/township
#'      in 2040 from
#'      smart grid implementation in the commercial sector.
#' @export
#'
#' @examples
#' \dontrun{
#' ghg.sp::calc_smart_grid_comm_electr_use(
#'      .comm_buildings_in_smart_grid_pct = 1,
#'      non_res_tb = building_data$non_residential
#'      )
#'
#' }
calc_smart_grid_comm_electr_use <-
  function(.comm_buildings_in_smart_grid_pct,
           tb_non_res) {
    smart_grid_electr_use <-
      non_res_tb %>%
      dplyr::filter(var %in% c("commercial_jobs",
                               "commercial_mwh_per_worker")) %>%
      dplyr::group_by(ctu_name, var) %>%
      tidyr::pivot_wider(
        names_from = c(var, year),
        values_from = value,
        names_sep = "."
      ) %>%
      dplyr::mutate(mwh =
                      .comm_buildings_in_smart_grid_pct
                    (
                      (commercial_jobs.2040 *  commercial_mwh_per_worker.2040) -
                        (
                          0.8 * commercial_jobs.2018 * 0.25 * commercial_mwh_per_worker.2040
                        ) *
                        0.11
                    ))

    return(smart_grid_electr_use)

  }
