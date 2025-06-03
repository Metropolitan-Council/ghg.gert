#' @title Adjust residential Building Unit Counts
#' @family residential
#' @family buildings
#'
#' @description This function adjusts the forecasted single-family and multi-family unit counts
#'    for cities/townships based on the residential inputs table. The purpose is to allow for a specified
#'    percentage of new single-family homes to be built as multi-family homes instead.
#'
#' @inheritParams run_scenario_building
#' @inheritParams filter_ctu
#' @inheritParams calc_ghg_residential
#' @param .new_homes_to_multifamily_pct numeric,  a value between `0` and `1`.
#'      Percentage of new single-family homes to instead be built as multifamily homes.
#'      Default is `0.0`.
#'
#' @return [tibble::tibble()].
#'       A table with columns `geog_name`, `geog_id`, `year`, `var`, and `value`.
#'       Table contains adjusted `single_family_units` and `multifamily_units` record for column `var`
#'       relative to residential inputs table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.ccap)
#'
#' adj_unit_counts(
#'   res_tb = compile_bau_building_energy()$residential,
#'   .selected_ctu = "all",
#'   .new_homes_to_multifamily_pct = 0.50
#' )
#' }
#' @importFrom dplyr filter group_by mutate select ungroup anti_join bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom cli cli_warn
adj_unit_counts <- function(res_tb,
                            .selected_ctu,
                            .new_homes_to_multifamily_pct) {
  # cli::cli_progress_message("*** adjusting residential building unit counts \n")
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)

  if (.new_homes_to_multifamily_pct <= 0) {
    cli::cli_warn("No single family homes instead built as multifamily homes.")
    return(res_tb)
  }

  n_new_homes <-
    res_tb %>%
    # select(-value_change_from_base) %>%
    dplyr::filter(sp_categories %in% c(
      "multifamily_units",
      "single_family_units",
      "single_family_attached",
      "single_family_small_lot",
      "single_family_large_lot"
    )) %>%
    filter(inventory_year == max(inventory_year))

  # some CTUs are going to decrease the number of single family units
  # over the next few decades.
  # We will consider the number of SF units reduced as if they were
  # being constructed and add them onto the multifamily unit count

  if (any(n_new_homes$value_change_from_base < 0)) {
    cli::cli_warn(c(
      "Baseline forecast assumes reducing single family units",
      "Now reducing single family units further"
    ))
  } else if (any(n_new_homes$value_change_from_base == 0)) {
    cli::cli_warn(c(
      "Baseline forecast assumes no change in single family units",
      "No change in housing stock made"
    ))
  }


  sf_now_mf <- n_new_homes %>%
    dplyr::filter(sp_categories != "multifamily_units") %>%
    dplyr::mutate(
      new_homes = ifelse(value_change_from_base < 0, abs(value_change_from_base), value_change_from_base),
      # spread the new % new home to multifamily across
      # all single family home types
      now_mf = new_homes * (.new_homes_to_multifamily_pct / nrow(.))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(geog_name, geog_id, sp_categories, now_mf) %>%
    unique()

  total_new_mf <- sum(sf_now_mf$now_mf)

  new_units <- res_tb %>%
    dplyr::filter(sp_categories %in% c(
      "multifamily_units",
      "single_family_units",
      "single_family_attached",
      "single_family_small_lot",
      "single_family_large_lot"
    )) %>%
    dplyr::filter(inventory_year == max(inventory_year)) %>%
    dplyr::left_join(sf_now_mf, by = c("geog_name", "geog_id", "sp_categories")) %>%
    # if multifamily, add the now-multifamily units
    # if single family, subtract the now-multifamily units
    dplyr::mutate(
      value = ifelse(sp_categories == "multifamily_units", value + total_new_mf,
        value - now_mf
      ),
      value_change_from_base = ifelse(sp_categories == "multifamily_units", value_change_from_base + total_new_mf,
        value_change_from_base - now_mf
      )
    ) %>%
    dplyr::select(names(res_tb))

  # anti_join to replace original values
  # return a new version of res_tb
  new_res_tb <- res_tb %>%
    dplyr::anti_join(new_units, by = c("geog_name", "geog_id", "inventory_year", "sp_categories")) %>%
    dplyr::bind_rows(new_units)

  return(new_res_tb)
}
