#' @title Assign county for ag data
#' @family agriculture
#'
#' @description This function assigns CTUs an associated county based on where the highest percentage of land lies
#'
#' @inheritParams run_module_agriculture
#'
#'
#' @return [tibble::tibble()].
#' A table with the same columns as emissions input
#'
#' @export
#'
#' @importFrom dplyr filter group_by mutate select ungroup anti_join bind_rows slice_head arrange
#' @importFrom tidyr pivot_wider
#' @importFrom cli cli_warn
county_assign <- function(.selected_ctu = .selected_ctu) {
  geog_county <- ghg.gert::ctu_county_area %>%
    filter(geog_name == .selected_ctu) %>%
    arrange(desc(pct_of_ctu_area)) %>%
    slice_head() %>%
    pull(county_name)

  return(geog_county)
}
