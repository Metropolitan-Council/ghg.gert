#' @title Apply Natural Systems Module 3: Cropland Restoration
#'
#' @param df_hist Input dataframe of inventory land cover area from 2001 to 2022
#' @param df_null Input dataframe of land cover area estimates left unchanged from 2022 to 2050
#' @param start_yr Numeric start year for land conversion (2025 to 2045)
#' @param comp_time Numeric time to complete land conversion (5 to 30 years)
#' @param area_pct Available area for land conversion (0 to 100)
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
mod3_cropRestoration <- function(df_hist,
                                 df_null,
                                 start_yr,
                                 comp_time,
                                 area_pct,
                                 tree_pct,
                                 grass_pct,
                                 wetland_pct) {
  # Input checks
  if (!is.numeric(area_pct) || area_pct < 0 || area_pct > 100) {
    stop("area_pct must be a number between 0 and 100.")
  }

  # Input checks
  if (sum(c(tree_pct, grass_pct, wetland_pct)) < 100 || sum(c(tree_pct, grass_pct, wetland_pct)) > 100) {
    stop("sum of c(tree_pct, grass_pct, wetland_pct) must be 100")
  }

  if (!is.numeric(start_yr) || start_yr < 2025 || start_yr > 2045) {
    stop("start_yr must be between 2025 and 2045.")
  }
  if (!is.numeric(comp_time) || comp_time < 5 || comp_time > 30) {
    stop("comp_time must be between 5 and 30 years.")
  }

  total_restore_factor <- area_pct / 100
  tree_share <- tree_pct / 100
  grass_share <- grass_pct / 100
  wetland_share <- wetland_pct / 100

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
    if (year < start_yr) {
      area_available
    } else {
      total_reduction <- logisticGrowth(
        t = year,
        K = total_restore_area,
        r = 4 / comp_time,
        t0 = start_yr + comp_time / 2,
        start_year = start_yr,
        end_year = start_yr + comp_time
      )
      max(area_available - total_reduction, area_available - total_restore_area)
    }
  })

  result <- tibble(
    inventory_year = future_years,
    delta_Cropland = Cropland - df_null$Cropland,
    total_growth = -(Cropland - df_null$Cropland),
    delta_Tree = total_growth * tree_share,
    delta_Grassland = total_growth * grass_share,
    delta_Wetland = total_growth * wetland_share,
    delta_total = delta_Cropland + delta_Tree + delta_Grassland + delta_Wetland
  )

  return(result)
}
