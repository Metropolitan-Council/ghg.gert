#' @title Apply Natural Systems Module 2: Urban Tree Planting
#'
#' @param df_hist Input dataframe of inventory land cover area from 2001 to 2022
#' @param df_null Input dataframe of land cover area estimates left unchanged from 2022 to 2050
#' @param start_yr Numeric start year for land conversion (2025 to 2045)
#' @param comp_time Numeric time to complete land conversion (5 to 30 years)
#' @param area_pct Available area for land conversion (0 to 100)
#'
#'
#' @return [tibble::tibble()].
#'      A tibble containing the land cover change values in square kilometers
#'      after applying module 2, urban tree planting
#' @export
# Module 2: Urban Tree Planting -------------------------------------------
mod2_urbanTreePlanting <- function(df_hist,
                                   df_null,
                                   start_yr,
                                   comp_time,
                                   area_pct
) {
  # Input checks
  if (!is.numeric(area_pct) || area_pct < 0 || area_pct > 100) {
    stop("area_pct must be a number between 0 and 100.")
  }
  if (!is.numeric(start_yr) || start_yr < 2025 || start_yr > 2045) {
    stop("start_yr must be between 2025 and 2045.")
  }
  if (!is.numeric(comp_time) || comp_time < 5 || comp_time > 30) {
    stop("comp_time must be between 5 and 30 years.")
  }

  # Total Developed area in 2022
  # Want to determine how much developed area is available for tree planting based on
  # the degree of imperviousness (low, medium and high) where low is 20-49% impervious,
  # medium is 50-79% impervious and high is 80-100% impervious.

  # Plantable fractions per developed type
  plantable_fraction <- c(
    Developed_Low = 0.30,  # 30% plantable area, 70% impervious
    Developed_Med = 0.15,  # 15% plantable area, 85% impervious
    Developed_High = 0.05  #  5% plantable area, 95% impervious
  )

  # Get last year of inventory
  inventory_end <- df_hist %>% filter(inventory_year == max(inventory_year))

  # Calculate plantable area from each class
  developed_vals <- c(
    Developed_Low  = inventory_end$Developed_Low  * plantable_fraction["Developed_Low"],
    Developed_Med  = inventory_end$Developed_Med  * plantable_fraction["Developed_Med"],
    Developed_High = inventory_end$Developed_High * plantable_fraction["Developed_High"]
  )
  total_plantable <- sum(developed_vals)

  area_to_convert <- (area_pct / 100) * total_plantable

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
      if (year < start_yr) {
        0
      } else {
        reduction <- logisticGrowth(
          t = year,
          K = area_to_convert * proportions[[class_name]],
          r = 4 / comp_time,
          t0 = start_yr + comp_time / 2,
          start_year = start_yr,
          end_year = start_yr + comp_time
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

  # Add total delta (should be zero-sum overall)
  result <- result %>%
    mutate(delta_total = delta_Urban_Tree + delta_Developed_Low + delta_Developed_Med + delta_Developed_High)


  return(result)
}
