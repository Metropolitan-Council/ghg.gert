#' @title Adjust residential Building Unit Counts
#' @family residential
#' @family buildings
#'
#' @description adjusts single and multifamily unit count forecast by city/township
#' from residential inputs table.
#'
#' @inheritParams run_scenario_building
#'
#' @param .new_homes_to_multifamily_pct numeric,  a value between `0` and `1`.
#'      Percentage of new single-family homes to instead be built as multifamily homes.
#'      Default is `0.50`.
#'
#' @return [tibble::tibble()].
#'       A table with columns `ctu_name`, `year`, `var`, and `value`.
#'       Table contains adjusted `single_family_units` and `multifamily_units` record for column `var`
#'       relative to residential inputs table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#'
#' adj_unit_counts(
#'   res_tb = compile_bau_building_energy()$residential,
#'   .selected_ctu = "all",
#'   .new_homes_to_multifamily_pct = 0.50
#' )
#' }
#' @importFrom dplyr filter group_by mutate select ungroup anti_join bind_rows
#' @importFrom tidyr pivot_wider
adj_unit_counts <- function(res_tb,
                            .selected_ctu,
                            .new_homes_to_multifamily_pct) {
  cli::cli_progress_message("*** adjusting residential building unit counts \n")
  if (.new_homes_to_multifamily_pct <= 0) {
    warning("No single family homes instead built as multifamily homes.")
    return(res_tb)
  }
  n_new_homes <-
    res_tb %>%
    dplyr::filter(var %in% c(
      "multifamily_units",
      "single_family_units"
    )) %>%
    dplyr::group_by(ctu_name, var) %>%
    tidyr::pivot_wider(names_from = c(var, year), values_from = value, names_sep = ".") %>%
    dplyr::mutate(
      new_sf_homes = single_family_units.2040 - single_family_units.2018,
      new_mf_homes = multifamily_units.2040 - multifamily_units.2018
    )
  # some CTUs are going to decrease the number of single family units
  # over the next few decades. Remedy this by replacing all negative
  # unit counts with 0.

  sf_now_mf <- n_new_homes %>%
    dplyr::mutate(
      new_homes = ifelse(new_sf_homes < 0, 0, new_sf_homes),
      now_mf = new_sf_homes * .new_homes_to_multifamily_pct
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(ctu_name, now_mf) %>%
    unique()


  new_units <- res_tb %>%
    dplyr::filter(
      var %in% c(
        "multifamily_units",
        "single_family_units"
      ),
      year == 2040
    ) %>%
    dplyr::left_join(sf_now_mf, by = "ctu_name") %>%
    # if multifamily, add the now-multifamily units
    # if single family, subtract the now-multifamily units
    dplyr::mutate(value = ifelse(var == "multifamily_units", value + now_mf,
      value - now_mf
    )) %>%
    dplyr::select(names(res_tb))

  # anti_join to replace original values
  # return a new version of res_tb
  new_res_tb <- res_tb %>%
    dplyr::anti_join(new_units, by = c("ctu_name", "year", "var")) %>%
    dplyr::bind_rows(new_units)

  return(new_res_tb)
}
