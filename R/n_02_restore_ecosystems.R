#' @title Unified Ecosystem Restoration
#'
#' @description Interface for restoring wetlands, forests, and grassland/prairie.
#'   Wetlands use GIS-constrained potential (DNR) with an ambition percentage.
#'   Forests and grassland allow direct acreage specification.
#'
#'   Total restoration is capped at a soft limit (jurisdiction area minus
#'   developed and water). Within that, cities determine their own planning
#'   priorities. No source-land tracking is performed — only the target
#'   natural system types (Wetland, Tree, Grassland) gain area.
#'
#' @param df_null Input dataframe of land cover area estimates (projections)
#' @param restore_wetland Logical, whether to restore wetlands (default FALSE)
#' @param wetland_ambition_pct Numeric 0-100, percent of wetland potential to realize (default 50)
#' @param forest_area_sqkm Numeric, committed forest restoration area in sq km (default 0)
#' @param prairie_area_sqkm Numeric, committed grassland restoration area in sq km (default 0)
#' @param start_yr Start year for restoration (default 2025)
#' @param end_yr End year for restoration (default 2050)
#'
#' @return Dataframe with projected land cover areas. Includes attributes:
#'   - "restoration_allocations": area added per target type (for frontend display)
#'   - "restoration_summary": totals
#'   - "validation_warnings": any warnings about exceeding limits
#'
#' @export
#' @import dplyr
restore_ecosystems <- function(df_null,
                               restore_wetland = FALSE,
                               wetland_ambition_pct = 50,
                               forest_area_sqkm = 0,
                               prairie_area_sqkm = 0,
                               start_yr = 2025,
                               end_yr = 2050) {
  # ===========================================================================
  # Input validation
  # ===========================================================================
  stopifnot(
    is.data.frame(df_null),
    is.logical(restore_wetland),
    is.numeric(wetland_ambition_pct) && wetland_ambition_pct >= 0 && wetland_ambition_pct <= 100,
    is.numeric(forest_area_sqkm) && forest_area_sqkm >= 0,
    is.numeric(prairie_area_sqkm) && prairie_area_sqkm >= 0,
    is.numeric(start_yr),
    is.numeric(end_yr) && end_yr >= start_yr
  )

  if (!restore_wetland && forest_area_sqkm == 0 && prairie_area_sqkm == 0) {
    return(df_null)
  }

  # ===========================================================================
  # Restoration limits
  # ===========================================================================
  restoration_limits <- get_restoration_potential(df_null)
  soft_limit_sqkm <- restoration_limits$soft_limit_sqkm

  df_current <- df_null %>%
    filter(inventory_year == max(inventory_year))

  validation_warnings <- character(0)

  # ===========================================================================
  # Wetland target (constrained by DNR potential_wetland_area layer)
  # ===========================================================================
  wetland_target <- 0

  if (restore_wetland && "potential_wetland_area" %in% colnames(df_current)) {
    wetland_potential_total <- df_current %>%
      filter(land_cover_type %in% c("Tree", "Grassland", "Bare", "Cropland")) %>%
      mutate(
        actual_potential = pmin(area, potential_wetland_area, na.rm = TRUE),
        actual_potential = ifelse(is.na(actual_potential), 0, actual_potential)
      ) %>%
      pull(actual_potential) %>%
      sum(na.rm = TRUE)

    wetland_target <- wetland_potential_total * (wetland_ambition_pct / 100)
  }

  # ===========================================================================
  # Soft limit check
  # ===========================================================================
  total_proposed <- wetland_target + forest_area_sqkm + prairie_area_sqkm

  if (total_proposed > soft_limit_sqkm) {
    validation_warnings <- c(
      validation_warnings,
      sprintf(
        "Total proposed restoration (%.2f sq km) exceeds available non-developed/water area (%.2f sq km).",
        total_proposed, soft_limit_sqkm
      )
    )
  }

  # ===========================================================================
  # Ensure target land cover types exist in the data
  # ===========================================================================
  ensure_land_cover_rows <- function(df, type_name) {
    if (type_name %in% df_current$land_cover_type) return(df)

    template <- df_current %>%
      filter(land_cover_type %in% c("Bare", "Cropland", "Grassland", "Tree")) %>%
      slice(1)

    new_rows <- df %>%
      filter(land_cover_type == template$land_cover_type[1]) %>%
      group_by(inventory_year) %>%
      slice(1) %>%
      dplyr::ungroup() %>%
      mutate(land_cover_type = type_name, area = 0)

    if ("potential_wetland_area" %in% colnames(new_rows)) {
      new_rows <- new_rows %>% mutate(potential_wetland_area = 0)
    }

    bind_rows(df, new_rows)
  }

  if (wetland_target > 0)    df_null <- ensure_land_cover_rows(df_null, "Wetland")
  if (forest_area_sqkm > 0)  df_null <- ensure_land_cover_rows(df_null, "Tree")
  if (prairie_area_sqkm > 0) df_null <- ensure_land_cover_rows(df_null, "Grassland")

  # ===========================================================================
  # Build area_change per target type
  # ===========================================================================
  df_current_updated <- df_null %>%
    filter(inventory_year == max(inventory_year))

  df_max <- df_current_updated %>%
    mutate(
      area_change = case_when(
        land_cover_type == "Wetland"   ~ wetland_target,
        land_cover_type == "Tree"      ~ forest_area_sqkm,
        land_cover_type == "Grassland" ~ prairie_area_sqkm,
        TRUE ~ 0
      )
    )

  # ===========================================================================
  # Apply linear interpolation
  # ===========================================================================
  df_export <- simulate_land_conversion(
    df = df_null %>%
      left_join(
        df_max %>% dplyr::select(land_cover_type, area_change),
        by = join_by(land_cover_type)
      ),
    start_yr = start_yr,
    end_yr = end_yr
  ) %>%
    dplyr::select(all_of(colnames(df_null)))

  # ===========================================================================
  # Attach metadata
  # ===========================================================================

  # Structure kept compatible with frontend: sum(unlist(allocations$X)) returns total
  attr(df_export, "restoration_allocations") <- list(
    wetland = list(total = wetland_target),
    forest  = list(total = forest_area_sqkm),
    prairie = list(total = prairie_area_sqkm)
  )

  attr(df_export, "restoration_summary") <- list(
    wetland_added_sqkm   = wetland_target,
    forest_added_sqkm    = forest_area_sqkm,
    grassland_added_sqkm = prairie_area_sqkm,
    total_restored_sqkm  = total_proposed
  )

  attr(df_export, "validation_warnings") <- validation_warnings

  return(df_export)
}
