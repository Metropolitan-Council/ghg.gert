#' @title Land conversion utility
#'
#' @description Applies area changes to land cover types using linear
#'   interpolation over the specified time window. Before `start_yr`, no change
#'   is applied. Between `start_yr` and `end_yr`, the change ramps linearly.
#'   After `end_yr`, the full `area_change` is held constant.
#'
#' @param df Dataframe with columns `inventory_year`, `area`, and `area_change`.
#'   `area_change` is the total delta to apply by `end_yr` (positive = gain,
#'   negative = loss).
#' @param start_yr The year when the transition begins
#' @param end_yr The year when the transition is complete
#'
#' @return Dataframe with `area` updated to reflect the linear transition
#' @export
simulate_land_conversion <- function(df, start_yr, end_yr) {
  total_years <- end_yr - start_yr

  df_export <- df %>%
    dplyr::mutate(
      delta_area = dplyr::case_when(
        inventory_year < start_yr ~ 0,
        area_change == 0 ~ 0,
        # Linear ramp during transition
        inventory_year <= end_yr ~
          area_change * (inventory_year - start_yr) / total_years,
        # Hold final value after end_yr
        inventory_year > end_yr ~ area_change
      ),
      area = area + delta_area
    )

  return(df_export)
}
