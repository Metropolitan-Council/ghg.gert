#' @title Adjust Residential Building Unit Counts
#' @family building_energy_module
#'
#' @description Adjust single and multifamily unit count forecast in residential table
#'
#' @param .new_homes_to_multifamily_pct Numeric.
#' Percentage of new single-family homes to instead be built as multifamily homes
#'      Default is ``
#'
#' @inheritParams scen_building_residential
#' @return
#' @export
#'
#'
#' @importFrom dplyr filter group_by mutate select ungroup anti_join bind_rows
#' @importFrom tidyr pivot_wider
adj_unit_counts <- function(res_tb,
                            .new_homes_to_multifamily_pct) {
  if (.new_homes_to_multifamily_pct <= 0) {
    warning("No single family homes instead built as multifamily homes.")
    return(res_tb)
  }
  n_new_homes <-
    res_tb %>%
    dplyr::filter(var %in% c("multifamily_units",
                             "single_family_units")) %>%
    dplyr::group_by(ctu_name, var) %>%
    tidyr::pivot_wider(names_from = year, values_from = value) %>%
    dplyr::mutate(new_homes = `2040` - `2018`)


  # some CTUs are going to decrease the number of single family units
  # over the next few decades. Remedy this by replacing all negative
  # unit counts with 0.
  sf_now_mf <- n_new_homes %>%
    dplyr::filter(var == "single_family_units") %>%
    dplyr::mutate(
      new_homes = ifelse(new_homes < 0, 0, new_homes),
      now_mf = new_homes * .new_homes_to_multifamily_pct
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(ctu_name, now_mf) %>%
    unique()


  new_units <- res_tb %>%
    dplyr::filter(var %in% c("multifamily_units",
                             "single_family_units"),
                  year == 2040) %>%
    dplyr::left_join(sf_now_mf, by = "ctu_name") %>%
    # if multifamily, add the now-multifamily units
    # if single family, subtract the now-multifamily units
    dplyr::mutate(value = ifelse(var == "multifamily_units", value + now_mf,
                                 value - now_mf)) %>%
    dplyr::select(names(res_tb))

  # anti_join to replace original values
  # return a new version of res_tb
  new_res_tb <- res_tb %>%
    dplyr::anti_join(new_units, by = c("ctu_name", "year", "var")) %>%
    dplyr::bind_rows(new_units)


  return(new_res_tb)
}
