#' @title Build a linear ramp schedule
#' @family buildings
#'
#' @description Constructs a year-by-year ramp tibble for strategy phase-in.
#'   Returns 0 before `start_year`, a linear ramp from `start_year` to
#'   `end_year`, and the full `target_pct` for years after `end_year`.
#'   Covers the full 2005:2050 planning horizon.
#'
#' @param start_year integer, first year the strategy begins.
#' @param end_year integer, year the strategy reaches full adoption.
#' @param target_pct numeric, final adoption fraction (0–1).
#' @param col_name character, name for the output percentage column.
#'
#' @return [tibble::tibble()] with columns `inventory_year` and `col_name`.
#' @export
build_ramp_schedule <- function(start_year,
                                end_year,
                                target_pct,
                                col_name = "ramp_pct") {
  ramp_years <- start_year:end_year
  n_ramp <- length(ramp_years)

  ramp <- tibble::tibble(
    emissions_year = ramp_years,
    ramp_val = seq(
      from = target_pct / n_ramp,
      to = target_pct,
      length.out = n_ramp
    )
  )

  out <- tibble::tibble(emissions_year = 2005:2050) %>%
    dplyr::left_join(ramp, by = "emissions_year") %>%
    dplyr::mutate(
      ramp_val = dplyr::case_when(
        emissions_year < start_year ~ 0,
        emissions_year > end_year ~ target_pct,
        TRUE ~ ramp_val
      )
    )

  names(out)[names(out) == "ramp_val"] <- col_name
  return(out)
}
