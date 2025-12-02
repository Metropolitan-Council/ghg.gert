#' @title Land conversion utility
#'
#' @param df base dataframe as input
#' @param start_yr The year when growth modeling begins, used for normalization
#' @param end_yr The year when growth modeling ends, used for normalization
#'
#' @return vector equal to the length of 't'
#' @export
# Logistic growth function with normalization and constraints
simulate_land_conversion <- function(df, start_yr, end_yr) {



  df_change <- df %>%
    rowwise() %>%
    mutate(
      delta_area = case_when(
        inventory_year < start_yr ~ 0,
        area_change == 0 ~ 0,

        # During transition period
        inventory_year <= end_yr ~ {
          years_elapsed <- inventory_year - (start_yr - 1)
          total_years   <- end_yr - (start_yr - 1)

          K  <- area_change
          r  <- 10 / total_years
          t0 <- (start_yr - 1) + total_years / 2

          logisticGrowth(
            t = inventory_year,
            K = K,
            r = r,
            t0 = t0,
            start_year = start_yr - 1,
            end_year = end_yr
          )
        },

        # After end_yr → hold final value
        inventory_year > end_yr ~ {
          years_elapsed <- end_yr - (start_yr - 1)
          total_years   <- end_yr - (start_yr - 1)

          K  <- area_change
          r  <- 10 / total_years
          t0 <- (start_yr - 1) + total_years / 2

          logisticGrowth(
            t = end_yr,
            K = K,
            r = r,
            t0 = t0,
            start_year = start_yr - 1,
            end_year = end_yr
          )
        }
      )
    ) %>%
    ungroup()

  df_export <- df_change %>%
    mutate(area = area + delta_area)

  return(df_export)
}
