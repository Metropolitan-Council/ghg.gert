#' @title Apply Natural Systems Module 3: Cropland Restoration
#'
#' @param df_hist Input dataframe of inventory land cover area from 2001 to 2022
#' @param df_null Input dataframe of land cover area estimates left unchanged from 2022 to 2050
#' @param .restoration_start Numeric start year for land conversion (2025 to 2045)
#' @param .restoration_time Numeric time to complete land conversion (5 to 30 years)
#' @param .wetland_area_perc Available wetland area for land conversion (0 to 100)
#'
#'
#' @return [tibble::tibble()].
#'      A tibble containing the land cover change values in square kilometers
#'      after applying module 3, cropland restoration
#' @export
# Module 3: Wetland Restoration -------------------------------------------
wetland_restoration <- function(df_hist,
                             df_null,
                             .restoration_start,
                             .restoration_time,
                             .wetland_area_perc)  {
  # # Input checks
  # if (!is.numeric(.restoration_area_perc) || .restoration_area_perc < 0 || .restoration_area_perc > 100) {
  #   stop(".restoration_area_perc must be a number between 0 and 100.")
  # }
  #
  # if (!is.numeric(.restoration_start) || .restoration_start < 2025 || .restoration_start > 2045) {
  #   stop(".restoration_start must be between 2025 and 2045.")
  # }
  # if (!is.numeric(.restoration_time) || .restoration_time < 5 || .restoration_time > 30) {
  #   stop(".restoration_time must be between 5 and 30 years.")
  # }

  # browser()


  future_years <- sort(unique(df_null$inventory_year))


  # Calculate total area available for restoration
  current_total <- filter(df_hist, inventory_year == max(inventory_year)) %>% pull(TOTAL)
  current_wetland <- filter(df_hist, inventory_year == max(inventory_year)) %>% pull(Wetland)

  new_wetland <- current_wetland * .wetland_area_perc / 100

  # at this point we know that wetlands need to increase by the specified amount, but
  # we need to decrease other land covers to make room for it. The problem is how to do that
  # in a way that makes sense.


  Wetland <- sapply(future_years, function(year) {
    if (year < .restoration_start) {
      current_wetland
    } else {
      total_gain <- logisticGrowth(
        t = year,
        K = new_wetland,
        r = 4 / .restoration_time,
        t0 = .restoration_start + .restoration_time / 2,
        start_year = .restoration_start,
        end_year = .restoration_start + .restoration_time
      )
      min(current_wetland + total_gain, current_wetland + new_wetland)
    }
  })


  result <- tibble(
    inventory_year = future_years,
    delta_Wetland = Wetland - df_null$Wetland
    # delta_Grassland = Grassland - df_null$Grassland,
    # delta_Bare = Bare - df_null$Bare,
    # delta_Tree = -(Cropland - df_null$Cropland) - (Bare - df_null$Bare) - (Grassland - df_null$Grassland),
    # delta_total = delta_Cropland + delta_Grassland + delta_Bare + delta_Tree
  )




  # current_cropland <- filter(df_hist, inventory_year == max(inventory_year)) %>% pull(Cropland)
  # current_bare <- filter(df_hist, inventory_year == max(inventory_year)) %>% pull(Bare)
  # current_grassland <- filter(df_hist, inventory_year == max(inventory_year)) %>% pull(Grassland)
  # current_forest <- filter(df_hist, inventory_year == max(inventory_year)) %>% pull(Tree)


  # new_cropland <- current_cropland * .cropland_area_perc / 100
  # new_bare <- current_bare * .bare_area_perc / 100
  # new_grassland <- current_grassland * .grassland_area_perc / 100

  # # Fix this
  # if (total_restore_area == 0) {
  #   return(tibble(
  #     inventory_year = future_years,
  #     delta_Cropland = rep(0, length(future_years)),
  #     delta_Tree = rep(0, length(future_years)),
  #     delta_Grassland = rep(0, length(future_years)),
  #     delta_Wetland = rep(0, length(future_years)),
  #     delta_total = rep(0, length(future_years))
  #   ))
  # }


  # Cropland <- sapply(future_years, function(year) {
  #   if (year < .restoration_start) {
  #     current_cropland
  #   } else {
  #     total_reduction <- logisticGrowth(
  #       t = year,
  #       K = new_cropland,
  #       r = 4 / .restoration_time,
  #       t0 = .restoration_start + .restoration_time / 2,
  #       start_year = .restoration_start,
  #       end_year = .restoration_start + .restoration_time
  #     )
  #     max(current_cropland - total_reduction, current_cropland - new_cropland)
  #   }
  # })
  #
  #
  #
  # Grassland <- sapply(future_years, function(year) {
  #   if (year < .restoration_start) {
  #     current_grassland
  #   } else {
  #     total_reduction <- logisticGrowth(
  #       t = year,
  #       K = new_grassland,
  #       r = 4 / .restoration_time,
  #       t0 = .restoration_start + .restoration_time / 2,
  #       start_year = .restoration_start,
  #       end_year = .restoration_start + .restoration_time
  #     )
  #     max(current_grassland - total_reduction, current_grassland - new_grassland)
  #   }
  # })
  #
  #
  # Bare <- sapply(future_years, function(year) {
  #   if (year < .restoration_start) {
  #     current_bare
  #   } else {
  #     total_reduction <- logisticGrowth(
  #       t = year,
  #       K = new_bare,
  #       r = 4 / .restoration_time,
  #       t0 = .restoration_start + .restoration_time / 2,
  #       start_year = .restoration_start,
  #       end_year = .restoration_start + .restoration_time
  #     )
  #     max(current_bare - total_reduction, current_bare - new_bare)
  #   }
  # })
  #
  #
  #
  # result <- tibble(
  #   inventory_year = future_years,
  #   delta_Cropland = Cropland - df_null$Cropland,
  #   delta_Grassland = Grassland - df_null$Grassland,
  #   delta_Bare = Bare - df_null$Bare,
  #   delta_Tree = -(Cropland - df_null$Cropland) - (Bare - df_null$Bare) - (Grassland - df_null$Grassland),
  #   # delta_total = delta_Cropland + delta_Grassland + delta_Bare + delta_Tree
  # )


  land_cover_colnames <- c(
    "Bare",
    "Cropland",
    "Developed_Low",
    "Developed_Med",
    "Developed_High",
    "Grassland",
    "Tree",
    "Urban_Grassland",
    "Urban_Tree",
    "Water",
    "Wetland","TOTAL"
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
          cols = land_cover_colnames, # Assuming these are all land cover columns
          names_to = "land_cover",
          values_to = "value"
        ),
      by = c("inventory_year", "land_cover")
    ) %>%
    mutate(
      new_value = if_else(!is.na(delta), value + delta, value)
    )  %>%
    select(-value, -delta) %>%
    rename(value = new_value) %>%
    # turn back to wide form
    pivot_wider(
      names_from = land_cover,
      values_from = value
    ) %>%
    mutate(
      newTotal = rowSums(across(all_of(land_cover_colnames[-length(land_cover_colnames)])))
    )



  return(result_out)
}
