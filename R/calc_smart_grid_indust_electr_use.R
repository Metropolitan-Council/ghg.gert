#' @title Calculate Industrial Electricity Use with Smart Grid
#' @family building_energy_module
#'
#' @description `calc_smart_grid_indust_electr_use()` calculates the impact of smart grid
#' implementation on the industrial building sector.
#'
#' @param .indust_buildings_in_smart_grid_pct Numeric. A number between `0` and `1`.
#'      The percentage of building that are part of the smart grid by the forecast
#'      year.
#'
#' @param non_res_tb
#'
#' @return Tibble.
#'      A table with the resulting electricity use in MWH/year in 2040 from
#'      smart grid implementation in the industrial sector.
#' @export
#'
#' @examples
#' \dontrun{
#' ghg.sp::calc_smart_grid_indust_electr_use(
#'      .indust_buildings_in_smart_grid_pct = 1,
#'      non_res_tb = building_data$non_residential
#' )
#' }
calc_smart_grid_indust_electr_use <-
  function(.indust_buildings_in_smart_grid_pct,
           non_res_tb) {
    smart_grid_electr_use <-
      non_res_tb %>%
      filter(var %in% c("industrial_jobs",
                        "industrial_mwh_per_worker")) %>%
      dplyr::group_by(ctu_name, var) %>%
      tidyr::pivot_wider(
        names_from = c(var, year),
        values_from = value,
        names_sep = "."
      ) %>%
      mutate(mwh_2040 =
               .indust_buildings_in_smart_grid_pct *
               (industrial_jobs.2040 *  industrial_mwh_per_worker.2040) *
               0.11) %>%
      select(ctu_name, mwh_2040)
    return(smart_grid_electr_use)

  }
