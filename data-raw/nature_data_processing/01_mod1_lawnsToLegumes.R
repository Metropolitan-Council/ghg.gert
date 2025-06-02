# Module 1: Lawns to Legumes ----------------------------------------------
mod1_lawnsToLegumes <- function(df_hist, # dataframe of inventory land cover area from 2001 to 2022
                                df_null, # dataframe of land cover area estimates left unchanged from 2022 to 2050
                                start_yr, # start year for land conversion (2025 to 2045)
                                comp_time, # time to complete land conversion (5 to 30 years)
                                area_pct # available area for land conversion (0 to 100)
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

  area_change_factor <- area_pct / 100
  area_available <- df_hist %>%
    filter(inventory_year == max(inventory_year)) %>%
    pull(Urban_Grassland)

  area_to_convert <- area_change_factor * area_available

  future_years <- sort(unique(df_null$inventory_year))

  # Early return if area_to_convert is zero
  if (area_to_convert == 0) {
    return(tibble(
      inventory_year = future_years,
      delta_Urban_Grassland = rep(0, length(future_years)),
      delta_Grassland = rep(0, length(future_years)),
      delta_total = rep(0, length(future_years))
    ))
  }

  Urban_Grassland <- sapply(future_years, function(year) {
    if (year < start_yr) {
      area_available
    } else {
      total_reduction <- logisticGrowth(
        t = year,
        K = area_to_convert,
        r = 4 / comp_time,
        t0 = start_yr + comp_time / 2,
        start_year = start_yr,
        end_year = start_yr + comp_time
      )
      max(area_available - total_reduction, area_available - area_to_convert)
    }
  })

  tibble(
    inventory_year = future_years,
    delta_Urban_Grassland = Urban_Grassland - df_null$Urban_Grassland,
    delta_Grassland = -(Urban_Grassland - df_null$Urban_Grassland),
    delta_total = delta_Urban_Grassland + delta_Grassland
  )
}
