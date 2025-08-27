#' @title Calculate planned land use modifications
#' @family land_use
#'
#' @description This function adjust 2040 planned land use numbers based on user
#' inputs for 2050 changes (guidance areas). Outputs are
#'    provided as a tibble with columns `geog_name`, `geog_id`, `var`, `scen`, `year`, and `value`.
#'
#' @return [tibble::tibble()].
#'       Returns a table with columns `geog_name`, `geog_id`, `var`, `scen`, `year`, and `value`.
#'       The table is the output of the land use module, any modification to
#'       the inputs of the land use module must be specified as an argument
#'       to the function `run_scenario_land_use()`
#'
#' @export
#' @importFrom cli cli_progress_message
#' @examples
#' \dontrun{
#'
#'
#' library(ghg.ccap)
#' }
#'
land_use_update <- function(tb_bau = tb_bau,
                            tb_strategy = tb_strategy,
                            .selected_ctu = "all",
                            .scenario = "alt") {
  # browser()
  tb_bau <- filter_ctu(tb_bau, .selected_ctu = .selected_ctu) %>%
    dplyr::select(
      geog_name,
      ctu_landuse_desc,
      unit_mean,
      acres
    )

  ### join 2040 table with user input

  tb_adj <- full_join(tb_bau,
    tb_strategy,
    by = c(
      "geog_name",
      "ctu_landuse_desc"
    ),
    suffix = c(
      "_thrive",
      "_adj"
    )
  ) %>%
    # remove categories that were deleted
    filter(!is.na(unit_mean_adj)) %>%
    mutate(acres_2050 = case_when(
      is.na(acre_change) ~ acres,
      is.na(acres) ~ acre_change,
      TRUE ~ acres + acre_change
    )) %>%
    select(geog_name,
      ctu_landuse_desc,
      unit_mean = unit_mean_adj,
      acres = acres_2050
    )


  return(tb_adj)
}
