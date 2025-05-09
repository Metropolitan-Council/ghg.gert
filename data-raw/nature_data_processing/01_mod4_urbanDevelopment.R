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

  tibble(
    inventory_year = future_years,
    delta_Cropland = Cropland - df_null$Cropland,
    total_growth = -(Cropland - df_null$Cropland),
    delta_Tree = total_growth * tree_share,
    delta_Grassland = total_growth * grass_share,
    delta_Wetland = total_growth * wetland_share,
    delta_total = delta_Cropland + delta_Tree + delta_Grassland + delta_Wetland
  )

}





# mod3_cropRestoration <- function(df_hist,
#                                  df_null,
#                                  start_yr,
#                                  comp_time,
#                                  area_pct,
#                                  tree_pct,
#                                  grass_pct,
#                                  wetland_pct,
#                                  dbg=F) {
#
#
#   if (dbg) browser()
#
#   # Input checks
#   if (!is.numeric(area_pct) || area_pct < 0 || area_pct > 100) {
#     stop("area_pct must be a number between 0 and 100.")
#   }
#
#   # Input checks
#   if (sum(c(tree_pct, grass_pct, wetland_pct)) < 100 || sum(c(tree_pct, grass_pct, wetland_pct)) > 100) {
#     stop("sum of c(tree_pct, grass_pct, wetland_pct) must be 100")
#   }
#
#   if (!is.numeric(start_yr) || start_yr < 2025 || start_yr > 2045) {
#     stop("start_yr must be between 2025 and 2045.")
#   }
#   if (!is.numeric(comp_time) || comp_time < 5 || comp_time > 30) {
#     stop("comp_time must be between 5 and 30 years.")
#   }
#
#   total_restore_factor <- area_pct / 100
#   tree_share <- tree_pct / 100
#   grass_share <- grass_pct / 100
#   wetland_share <- wetland_pct / 100
#   # browser()
#   area_available <- df_hist %>%
#     filter(inventory_year == max(inventory_year)) %>%
#     pull(Cropland)
#
#   total_restore_area <- total_restore_factor * area_available
#
#   future_years <- sort(unique(df_null$inventory_year))
#
#   if (total_restore_area == 0) {
#     return(tibble(
#       inventory_year = future_years,
#       delta_Cropland = rep(0, length(future_years)),
#       delta_Tree = rep(0, length(future_years)),
#       delta_Grassland = rep(0, length(future_years)),
#       delta_Wetland = rep(0, length(future_years)),
#       delta_total = rep(0, length(future_years))
#     ))
#   }
#
#
#   total_growth <- sapply(future_years, function(year) {
#     if (year < start_yr) {
#       0
#     } else {
#       logisticGrowth(
#         t = year,
#         K = total_restore_area,
#         r = 4 / comp_time,
#         t0 = start_yr + comp_time / 2,
#         start_year = start_yr,
#         end_year = start_yr + comp_time
#       )
#     }
#   })
#
#   # Scale total_growth to perfectly match total_restore_area
#   if (max(total_growth) != 0) {
#     scaling_factor <- total_restore_area / max(total_growth)
#     total_growth <- total_growth * scaling_factor
#   }
#
#   restore_tree <- total_growth * tree_share
#   restore_grass <- total_growth * grass_share
#   restore_wet <- total_growth * wetland_share
#
#   tibble(
#     inventory_year = future_years,
#     delta_Cropland = -total_growth,
#     delta_Tree = restore_tree,
#     delta_Grassland = restore_grass,
#     delta_Wetland = restore_wet,
#     delta_total = delta_Cropland + delta_Tree + delta_Grassland + delta_Wetland
#   )
# }
