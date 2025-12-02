#' @title Apply Natural Systems Module 2: Urban Tree Planting
#'
#' @param df_hist Input dataframe of inventory land cover area from 2001 to 2022
#' @param df_null Input dataframe of land cover area estimates left unchanged from 2022 to 2050
#' @param .urban_tree_start Numeric start year for land conversion (2025 to 2045)
#' @param .urban_tree_time Numeric time to complete land conversion (5 to 30 years)
#' @param .urban_tree_area_perc Available area for land conversion (0 to 100)
#'
#'
#' @return [tibble::tibble()].
#'      A tibble containing the land cover change values in square kilometers
#'      after applying module 2, urban tree planting
#' @export
# Module 2: Urban Tree Planting -------------------------------------------
urban_tree_planting <- function(df_hist,
                                df_null,
                                .urban_tree_start,
                                .urban_tree_time,
                                .urban_tree_area_perc) {
  # Input checks
  if (!is.numeric(.urban_tree_area_perc) || .urban_tree_area_perc < 0 || .urban_tree_area_perc > 100) {
    stop(".urban_tree_area_perc must be a number between 0 and 100.")
  }
  if (!is.numeric(.urban_tree_start) || .urban_tree_start < 2025 || .urban_tree_start > 2045) {
    stop(".urban_tree_start must be between 2025 and 2045.")
  }
  if (!is.numeric(.urban_tree_time) || .urban_tree_time < 5 || .urban_tree_time > 30) {
    stop(".urban_tree_time must be between 5 and 30 years.")
  }

  # Total Developed area in 2022
  # Want to determine how much developed area is available for tree planting based on
  # the degree of imperviousness (low, medium and high) where low is 20-49% impervious,
  # medium is 50-79% impervious and high is 80-100% impervious.

  # Plantable fractions per developed type
  plantable_fraction <- c(
    Developed_Low = 0.30, # 30% plantable area, 70% impervious
    Developed_Med = 0.15, # 15% plantable area, 85% impervious
    Developed_High = 0.05 #  5% plantable area, 95% impervious
  )

  # Get last year of inventory
  inventory_end <- df_hist %>% filter(inventory_year == max(inventory_year))

  # Calculate plantable area from each class
  developed_vals <- c(
    Developed_Low  = inventory_end$Developed_Low * plantable_fraction["Developed_Low"],
    Developed_Med  = inventory_end$Developed_Med * plantable_fraction["Developed_Med"],
    Developed_High = inventory_end$Developed_High * plantable_fraction["Developed_High"]
  )
  total_plantable <- sum(developed_vals)

  area_to_convert <- (.urban_tree_area_perc / 100) * total_plantable

  proportions <- developed_vals / total_plantable

  future_years <- sort(unique(df_null$inventory_year))

  # Early return if area_to_convert is zero
  if (area_to_convert == 0) {
    return(tibble(
      inventory_year = future_years,
      delta_Urban_Tree = rep(0, length(future_years)),
      delta_Developed_Low = rep(0, length(future_years)),
      delta_Developed_Med = rep(0, length(future_years)),
      delta_Developed_high = rep(0, length(future_years)),
      delta_total = rep(0, length(future_years))
    ))
  }


  # Logistic deltas for each developed class
  deltas <- lapply(names(proportions), function(class_name) {
    sapply(future_years, function(year) {
      if (year < .urban_tree_start) {
        0
      } else {
        reduction <- logisticGrowth(
          t = year,
          K = area_to_convert * proportions[[class_name]],
          r = 4 / .urban_tree_time,
          t0 = .urban_tree_start + .urban_tree_time / 2,
          start_year = .urban_tree_start,
          end_year = .urban_tree_start + .urban_tree_time
        )
        -reduction
      }
    })
  })

  # Adjusting the names of deltas to remove class name repetition
  names(deltas) <- c("delta_Developed_Low", "delta_Developed_Med", "delta_Developed_High")

  # Convert list to matrix to allow rowSums
  deltas_matrix <- do.call(cbind, deltas)

  # Urban_Tree gain is sum of absolute reductions
  delta_Urban_Tree <- rowSums(-deltas_matrix)

  # Assemble output tibble
  result <- tibble(
    inventory_year = future_years,
    delta_Urban_Tree = delta_Urban_Tree
  )

  # Add deltas for each developed class to result
  for (name in names(deltas)) {
    result[[name]] <- deltas[[name]]
  }

  # create mergeable data frame with df_null for new output
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
