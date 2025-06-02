#' @title Apply Natural Systems Module 3: Cropland Restoration
#'
#' @param df_hist Input dataframe of inventory land cover area from 2001 to 2022
#' @param df_null Input dataframe of land cover area estimates left unchanged from 2022 to 2050
#' @param .restoration_start Numeric start year for land conversion (2025 to 2045)
#' @param .restoration_time Numeric time to complete land conversion (5 to 30 years)
#' @param .restoration_area_perc Available area for land conversion (0 to 100)
#' @param tree_pct Proportion of converted land to be designated as 'Tree' (0 to 100)
#' @param grass_pct Proportion of converted land to be designated as 'Grassland' (0 to 100)
#' @param wetland_pct Proportion of converted land to be designated as 'Wetland' (0 to 100)
#'
#'
#' @return [tibble::tibble()].
#'      A tibble containing the land cover change values in square kilometers
#'      after applying module 3, cropland restoration
#' @export
# Module 3: Cropland Restoration -------------------------------------------
crop_restoration <- function(df_hist,
                             df_null,
                             .restoration_start,
                             .restoration_time,
                             .restoration_area_perc) {
  # Input checks
  if (!is.numeric(.restoration_area_perc) || .restoration_area_perc < 0 || .restoration_area_perc > 100) {
    stop(".restoration_area_perc must be a number between 0 and 100.")
  }

  if (!is.numeric(.restoration_start) || .restoration_start < 2025 || .restoration_start > 2045) {
    stop(".restoration_start must be between 2025 and 2045.")
  }
  if (!is.numeric(.restoration_time) || .restoration_time < 5 || .restoration_time > 30) {
    stop(".restoration_time must be between 5 and 30 years.")
  }

  total_restore_factor <- .restoration_area_perc / 100

  area_available <- df_hist %>%
    filter(inventory_year == max(inventory_year)) %>%
    pull(Cropland)

  total_restore_area <- total_restore_factor * area_available

  future_years <- sort(unique(df_null$inventory_year))

  if (total_restore_area == 0) {
    return(tibble(
      inventory_year = future_years,
      delta_Cropland = rep(0, length(future_years)),
      delta_Tree = rep(0, length(future_years)),
      delta_Grassland = rep(0, length(future_years)),
      delta_Wetland = rep(0, length(future_years)),
      delta_total = rep(0, length(future_years))
    ))
  }



  Cropland <- sapply(future_years, function(year) {
    if (year < .restoration_start) {
      area_available
    } else {
      total_reduction <- logisticGrowth(
        t = year,
        K = total_restore_area,
        r = 4 / .restoration_time,
        t0 = .restoration_start + .restoration_time / 2,
        start_year = .restoration_start,
        end_year = .restoration_start + .restoration_time
      )
      max(area_available - total_reduction, area_available - total_restore_area)
    }
  })

  result <- tibble(
    inventory_year = future_years,
    delta_Cropland = Cropland - df_null$Cropland,
    delta_Tree = -(Cropland - df_null$Cropland),
    delta_total = delta_Cropland + delta_Tree
  )

  result_out <- result %>%
    pivot_longer(
      cols = starts_with("delta_"),
      names_to = "land_cover",
      values_to = "delta"
    ) %>%
    mutate(
      land_cover = sub("delta_", "", land_cover) # Remove "delta_" prefix
    ) %>%
    right_join(
      df_null %>%
        pivot_longer(
          cols = Bare:TOTAL, # Assuming these are all land cover columns
          names_to = "land_cover",
          values_to = "value"
        ),
      by = c("inventory_year", "land_cover")
    ) %>%
    mutate(
      new_value = if_else(!is.na(delta), value + delta, value)
    ) %>%
    select(-value, -delta) %>%
    rename(value = new_value) %>%
    # turn back to wide form
    pivot_wider(
      names_from = land_cover,
      values_from = value
    )


  return(result_out)
}
