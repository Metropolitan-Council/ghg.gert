#' @title Apply Natural Systems Module: Plant Community Trees
#'
#' @description Converts a tree count input into a land cover area reallocation,
#'   moving area from Developed_Low/Med/High to Urban_Tree over a specified
#'   time window using a linear ramp.
#'
#'   Tree count is converted to area using a per-community density factor
#'   (sqm canopy per tree) from the DNR-calibrated community_tree_baseline
#'   lookup table. The area is distributed proportionally across developed
#'   classes based on their plantable fractions (30%/15%/5%).
#'
#' @param df_null Input dataframe of land cover area estimates (projections)
#' @param start_yr Numeric start year for tree planting
#' @param end_yr Numeric end year for tree planting
#' @param tree_count Numeric number of trees to plant
#' @param geog_id Character geography ID for density lookup. If NULL,
#'   extracted from df_null.
#'
#' @return Dataframe with same structure as df_null, with area reallocated
#'   from developed classes to Urban_Tree over the planting window.
#'   Attribute "tree_planting_info" attached with conversion metadata.
#'
#' @export
#' @importFrom dplyr filter mutate case_when left_join select bind_rows
plant_community_trees <- function(df_null,
                                  start_yr,
                                  end_yr,
                                  tree_count,
                                  geog_id = NULL) {

  # Density lookup

  if (is.null(geog_id)) {
    geog_id <- unique(df_null$geog_id)[1]
  }

  baseline <- ghg.ccap::community_tree_baseline %>%
    dplyr::filter(.data$geog_id == .env$geog_id)

  if (nrow(baseline) == 1) {
    sqm_per_tree <- baseline$sqm_per_tree
    max_trees    <- baseline$max_plantable_trees
  } else {
    # Fallback: regional median density, no cap
    sqm_per_tree <- median(ghg.ccap::community_tree_baseline$sqm_per_tree, na.rm = TRUE)
    max_trees    <- Inf
  }

  # Cap tree count at maximum plantable

  tree_count <- min(tree_count, max_trees)

  # Convert to area (sq km)
  area_to_convert <- tree_count * sqm_per_tree / 1e6


  # Compute area change per land cover type

  plantable_fraction <- c(
    Developed_Low  = 0.30,
    Developed_Med  = 0.15,
    Developed_High = 0.05
  )

  df_max_yr <- df_null %>%
    dplyr::filter(inventory_year == max(inventory_year))

  # Total plantable area for this geography
  total_plantable <- df_max_yr %>%
    dplyr::filter(land_cover_type %in% names(plantable_fraction)) %>%
    dplyr::mutate(plantable = area * plantable_fraction[land_cover_type]) %>%
    dplyr::pull(plantable) %>%
    sum()

  # Safety: don't exceed what's actually available
  area_to_convert <- min(area_to_convert, total_plantable)

  # Each developed class gives up its proportional share
  area_change_lookup <- df_max_yr %>%
    dplyr::mutate(
      area_change = dplyr::case_when(
        land_cover_type %in% names(plantable_fraction) ~
          -1 * area * plantable_fraction[land_cover_type] * (area_to_convert / total_plantable),
        land_cover_type == "Urban_Tree" ~ area_to_convert,
        TRUE ~ 0
      )
    ) %>%
    dplyr::select(land_cover_type, area_change)


  # Handle missing Urban_Tree

  has_urban_tree <- "Urban_Tree" %in% df_max_yr$land_cover_type

  if (!has_urban_tree) {
    # Add Urban_Tree row to the change lookup
    urban_tree_change <- data.frame(
      land_cover_type = "Urban_Tree",
      area_change = area_to_convert
    )
    area_change_lookup <- dplyr::bind_rows(area_change_lookup, urban_tree_change)

    # Add Urban_Tree rows (area = 0) to df_null for all years
    urban_tree_rows <- df_null %>%
      dplyr::filter(land_cover_type == "Developed_Low") %>%
      dplyr::group_by(inventory_year) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        land_cover_type = "Urban_Tree",
        area = 0,
        potential_wetland_area = 0
      )

    df_null <- dplyr::bind_rows(df_null, urban_tree_rows)
  }


  # Apply linear ramp

  df_export <- df_null %>%
    dplyr::left_join(area_change_lookup, by = "land_cover_type") %>%
    dplyr::mutate(
      area_change = tidyr::replace_na(area_change, 0),
      fraction = pmax(0, pmin(1, (inventory_year - start_yr) / (end_yr - start_yr))),
      area = area + area_change * fraction
    ) %>%
    dplyr::select(dplyr::all_of(colnames(df_null)))


  # Attach metadata

  attr(df_export, "tree_planting_info") <- list(
    tree_count = tree_count,
    sqm_per_tree = sqm_per_tree,
    area_converted_sqkm = area_to_convert,
    max_plantable_trees = max_trees,
    geog_id = geog_id
  )

  return(df_export)
}
