#' @title Adjust residential Building Unit Counts
#' @family residential
#' @family buildings
#'
#' @description This function adjusts the forecasted single-family and multi-family unit counts
#'    for cities/townships based on the land use inputs table. Land use will provide an expected change
#'    in residential density and the ratio to that change and 2040 plans will reduce single family
#'    detached home to increase single family attached and multifamily homes.
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
                            density_output,
                            .selected_ctu) {
  # cli::cli_progress_message("*** adjusting residential building unit counts \n")
  res_tb <- filter_ctu(res_tb, .selected_ctu = .selected_ctu)

  density_change <- (density_output$expected_density[2] - density_output$expected_density[1]) /
    density_output$expected_density[1]

  # counties and the region have no planned land use data (density is NaN),
  # so treat them as no density change
  if (!is.finite(density_change)) density_change <- 0


  # We will estimate a number of SF units that will be reduced
  # from 2050 land use changes and assume the same number of single
  # family attached and multifamily units will be constructed


  sfd_reduction <- res_tb %>%
    dplyr::filter(
      emissions_year >= 2028,
      sp_categories == "single_family_attached"
    ) %>%
    ### ctus that already reducing sfd will reduce further
    dplyr::mutate(
      density_sfd_change =
        ifelse(value_change_from_base < 0,
          value_change_from_base * density_change, # for negative values, get more negative with increased density
          -1 * value_change_from_base * density_change # for positive values, decrease with increased density
        )
    ) %>%
    dplyr::select(geog_name, geog_id, density_sfd_change, emissions_year)


  new_res_tb <- res_tb %>%
    left_join(sfd_reduction,
      by = join_by(geog_name, geog_id, emissions_year)
    ) %>%
    dplyr::mutate(
      value = case_when(
        sp_categories %in% c(
          "multifamily_units",
          "single_family_attached"
        ) &
          emissions_year >= 2028 ~ value - (density_sfd_change / 2), # half to sfa, half to multifamily
        sp_categories %in% c("single_family_detached") &
          emissions_year >= 2028 ~ value + density_sfd_change,
        TRUE ~ value
      ),
      value_change_from_base = case_when(
        sp_categories %in% c(
          "multifamily_units",
          "single_family_attached"
        ) &
          emissions_year >= 2028 ~ value_change_from_base - (density_sfd_change / 2), # half to sfa, half to multifamily
        sp_categories %in% c("single_family_detached") &
          emissions_year >= 2028 ~ value_change_from_base + density_sfd_change,
        TRUE ~ value_change_from_base
      )
    ) %>%
    dplyr::select(names(res_tb))


  return(new_res_tb)
}
