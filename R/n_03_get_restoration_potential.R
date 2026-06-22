#' @title Get Restoration Potential for a Jurisdiction
#'
#' @description Calculates restoration limits for a jurisdiction to populate
#'   UI input constraints. The soft limit (total area minus developed and water)
#'   caps total restoration across all types. Wetlands are additionally
#'   constrained by the GIS `potential_wetland_area` layer (DNR).
#'
#' @param df_null Input dataframe of land cover area projections
#'
#' @return A list containing:
#'   - wetland_potential_sqkm: max wetland restoration potential (GIS-constrained)
#'   - soft_limit_sqkm: jurisdiction area minus developed and water
#'
#' @export
#' @import dplyr
get_restoration_potential <- function(df_null) {
  df_current <- df_null %>%
    filter(inventory_year == max(inventory_year))

  get_area <- function(type) {
    df_current %>%
      filter(land_cover_type == type) %>%
      pull(area) %>%
      sum(na.rm = TRUE)
  }

  total_area <- sum(df_current$area, na.rm = TRUE)

  developed_area <- df_current %>%
    filter(grepl("^Developed", land_cover_type)) %>%
    pull(area) %>%
    sum(na.rm = TRUE)

  water_area <- get_area("Water")

  # ---------------------------------------------------------------------------
  # Wetland potential (constrained by DNR potential_wetland_area layer)
  # ---------------------------------------------------------------------------
  if ("potential_wetland_area" %in% colnames(df_current)) {
    wetland_potential <- df_current %>%
      filter(land_cover_type %in% c("Tree", "Grassland", "Bare", "Cropland")) %>%
      mutate(
        actual_potential = pmin(area, potential_wetland_area, na.rm = TRUE),
        actual_potential = ifelse(is.na(actual_potential), 0, actual_potential)
      ) %>%
      pull(actual_potential) %>%
      sum(na.rm = TRUE)
  } else {
    wetland_potential <- 0
    warning("No 'potential_wetland_area' column found. Wetland potential set to 0.")
  }

  # ---------------------------------------------------------------------------
  # Soft limit: jurisdiction area minus developed and water
  # ---------------------------------------------------------------------------
  soft_limit <- total_area - developed_area - water_area

  list(
    wetland_potential_sqkm = wetland_potential,
    soft_limit_sqkm = soft_limit
  )
}
