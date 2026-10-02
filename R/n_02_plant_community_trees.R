#' @title Apply Natural Systems Module: Plant Community Trees
#'
#' @description Converts a tree count input into a land cover area reallocation,
#'   area over a specified time window using a linear ramp.
#'
#'   Tree count is converted to area using a per-community density factor
#'   (sqm canopy per tree) from the DNR-calibrated community_tree_baseline
#'   lookup table. Capped at max_plantable_trees.
#'
#' @param df_null Input dataframe of land cover area estimates (projections)
#' @param start_yr Numeric start year for tree planting
#' @param end_yr Numeric end year for tree planting
#' @param tree_count Numeric number of trees to plant
#' @param geog_id Character geography ID for density lookup. If NULL,
#'   extracted from df_null.
#'
#' @return Dataframe with same structure as df_null, with Urban_Tree area
#'   increased over the planting window.
#'   Attribute "tree_planting_info" attached with conversion metadata.
#'
#' @export
#' @importFrom dplyr filter mutate bind_rows select
plant_community_trees <- function(df_null,
                                  start_yr,
                                  end_yr,
                                  tree_count,
                                  geog_id = NULL) {
  # Density lookup

  if (is.null(geog_id)) {
    geog_id <- unique(df_null$geog_id)[1]
  }

  baseline <- ghg.gert::community_tree_baseline %>%
    dplyr::filter(.data$geog_id == .env$geog_id)

  if (nrow(baseline) == 1) {
    sqm_per_tree <- baseline$sqm_per_tree
    max_trees <- baseline$max_plantable_trees
  } else {
    # Fallback: regional median density, no cap
    sqm_per_tree <- median(ghg.gert::community_tree_baseline$sqm_per_tree, na.rm = TRUE)
    max_trees <- Inf
  }

  # Validate tree count against max
  if (tree_count > max_trees) {
    stop(
      "tree_count (", tree_count, ") exceeds max_plantable_trees (",
      max_trees, ") for geog_id ", geog_id,
      call. = FALSE
    )
  }

  # Convert to area (sq km)
  area_to_add <- tree_count * sqm_per_tree / 1e6


  # Handle missing Urban_Tree

  if (!"Urban_Tree" %in% df_null$land_cover_type) {
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
    dplyr::mutate(
      area = dplyr::if_else(
        land_cover_type == "Urban_Tree",
        area + area_to_add * pmax(0, pmin(1, (inventory_year - start_yr) / (end_yr - start_yr))),
        area
      )
    )


  # Attach metadata

  attr(df_export, "tree_planting_info") <- list(
    tree_count = tree_count,
    sqm_per_tree = sqm_per_tree,
    area_added_sqkm = area_to_add,
    max_plantable_trees = max_trees,
    geog_id = geog_id
  )


  return(df_export)
}
