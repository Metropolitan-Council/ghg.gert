#' @title Unified Ecosystem Restoration
#'
#' @description Simplified interface for restoring wetlands, forests, and prairies.
#'   Handles source allocation and priority automatically. Area conservation is
#'   maintained by drawing from a conversion pool (Bare, Cropland, and optionally
#'   Grassland/Tree) with a fixed priority order that minimizes ecological impact.
#'
#'   Priority order: Wetlands → Forests → Prairies
#'   Source priority: Bare → Cropland → Grassland → Tree (wetland only)
#'
#' @param df_null Input dataframe of land cover area estimates (projections from 2023-2050)
#' @param restore_wetland Logical, whether to restore wetlands (default TRUE)
#' @param restore_forest Logical, whether to restore forests (default TRUE)
#' @param restore_prairie Logical, whether to restore native prairies/grassland (default FALSE)
#' @param ambition_pct Numeric 0-100, how much of available potential to realize (default 50)
#' @param start_yr Start year for restoration (default 2025)
#' @param end_yr End year for restoration (default 2050)
#' @param grassland_available_pct Percent of existing grassland available for conversion (default 50)
#'
#' @return Dataframe with projected land cover areas. Includes attributes:
#'   - "restoration_allocations": detailed breakdown of area by source and target
#'   - "restoration_summary": total area added to each target type
#'
#' @export
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' # Simple usage - restore wetlands and forests at 40% ambition
#' result <- restore_ecosystems(
#'   df_null = my_projections,
#'   restore_wetland = TRUE,
#'   restore_forest = TRUE,
#'   restore_prairie = FALSE,
#'   ambition_pct = 40
#' )
#'
#' # View what was allocated
#' attr(result, "restoration_summary")
#' }
restore_ecosystems <- function(df_null,
                               restore_wetland = TRUE,
                               restore_forest = TRUE,
                               restore_prairie = FALSE,
                               ambition_pct = 50,
                               start_yr = 2025,
                               end_yr = 2050,
                               grassland_available_pct = 50) {

  # Validate inputs
  stopifnot(
    is.data.frame(df_null),
    is.logical(restore_wetland),
    is.logical(restore_forest),
    is.logical(restore_prairie),
    is.numeric(ambition_pct) && ambition_pct >= 0 && ambition_pct <= 100,
    is.numeric(start_yr),
    is.numeric(end_yr) && end_yr >= start_yr,
    is.numeric(grassland_available_pct) && grassland_available_pct >= 0 && grassland_available_pct <= 100
  )

  # If nothing selected, return unchanged
  if (!restore_wetland && !restore_forest && !restore_prairie) {
    return(df_null)
  }

  # Get current state (last year of data)
  df_current <- df_null %>%
    filter(inventory_year == max(inventory_year))

  # Helper to get area for a land cover type
  get_area <- function(type) {
    df_current %>%
      filter(land_cover_type == type) %>%
      pull(area) %>%
      sum(na.rm = TRUE)
  }

  # Get available areas by source type
  bare_area <- get_area("Bare")
  cropland_area <- get_area("Cropland")
  grassland_area <- get_area("Grassland")
  tree_area <- get_area("Tree")

  # Grassland available for conversion (user-specified fraction)
  grassland_available <- grassland_area * (grassland_available_pct / 100)

  # Initialize tracking of remaining available area by source
  remaining <- list(
    Bare = bare_area,
    Cropland = cropland_area,
    Grassland = grassland_available,
    Tree = tree_area  # Trees only used for wetland conversion
  )

  # Initialize allocation tracking
  allocations <- list(
    wetland = list(Bare = 0, Cropland = 0, Grassland = 0, Tree = 0),
    forest = list(Bare = 0, Cropland = 0, Grassland = 0),
    prairie = list(Bare = 0, Cropland = 0)
  )

  # Helper function to allocate area from sources in priority order
  allocate_from_sources <- function(target_area, valid_sources, remaining) {
    allocated <- setNames(rep(0, length(valid_sources)), valid_sources)
    still_needed <- target_area

    for (src in valid_sources) {
      if (still_needed <= 0) break
      take <- min(still_needed, remaining[[src]])
      allocated[[src]] <- take
      remaining[[src]] <- remaining[[src]] - take
      still_needed <- still_needed - take
    }

    list(allocated = as.list(allocated), remaining = remaining)
  }

  # ===========================================================================
  # PRIORITY 1: Wetlands (constrained by potential_wetland_area layer)
  # ===========================================================================
  if (restore_wetland) {
    # Check if potential_wetland_area column exists
    has_potential_col <- "potential_wetland_area" %in% colnames(df_current)

    if (has_potential_col) {
      # Calculate max wetland potential from constraint layer
      wetland_potential_by_source <- df_current %>%
        filter(land_cover_type %in% c("Tree", "Grassland", "Bare", "Cropland")) %>%
        mutate(
          # Can't convert more than exists, and can't exceed potential
          actual_potential = pmin(area, potential_wetland_area, na.rm = TRUE),
          actual_potential = ifelse(is.na(actual_potential), 0, actual_potential)
        ) %>%
        select(land_cover_type, actual_potential)

      total_wetland_potential <- sum(wetland_potential_by_source$actual_potential, na.rm = TRUE)
      wetland_target <- total_wetland_potential * (ambition_pct / 100)

      # For wetlands, allocate proportionally based on potential in each source
      # This respects the spatial constraint layer
      if (wetland_target > 0 && total_wetland_potential > 0) {
        for (src in c("Bare", "Cropland", "Grassland", "Tree")) {
          src_potential <- wetland_potential_by_source %>%
            filter(land_cover_type == src) %>%
            pull(actual_potential) %>%
            sum(na.rm = TRUE)

          # Proportion of wetland target from this source
          src_allocation <- (src_potential / total_wetland_potential) * wetland_target

          # Cap by remaining available
          if (src == "Tree") {
            src_allocation <- min(src_allocation, remaining$Tree)
            remaining$Tree <- remaining$Tree - src_allocation
          } else if (src == "Grassland") {
            src_allocation <- min(src_allocation, remaining$Grassland)
            remaining$Grassland <- remaining$Grassland - src_allocation
          } else {
            src_allocation <- min(src_allocation, remaining[[src]])
            remaining[[src]] <- remaining[[src]] - src_allocation
          }

          allocations$wetland[[src]] <- src_allocation
        }
      }
    } else {
      # No constraint layer - skip wetland restoration with warning
      warning("No 'potential_wetland_area' column found. Wetland restoration requires this constraint layer.")
    }
  }

  # ===========================================================================
  # PRIORITY 2: Forests
  # ===========================================================================
  if (restore_forest) {
    # Forest potential is whatever remains in the pool (excluding Tree as source)
    forest_pool <- remaining$Bare + remaining$Cropland + remaining$Grassland
    forest_target <- forest_pool * (ambition_pct / 100)

    if (forest_target > 0) {
      result <- allocate_from_sources(
        forest_target,
        c("Bare", "Cropland", "Grassland"),
        remaining
      )
      allocations$forest <- result$allocated
      remaining <- result$remaining
    }
  }

  # ===========================================================================
  # PRIORITY 3: Prairie/Grassland
  # ===========================================================================
  if (restore_prairie) {
    # Prairie can only come from Bare and Cropland (not from existing grassland)
    prairie_pool <- remaining$Bare + remaining$Cropland
    prairie_target <- prairie_pool * (ambition_pct / 100)

    if (prairie_target > 0) {
      result <- allocate_from_sources(
        prairie_target,
        c("Bare", "Cropland"),
        remaining
      )
      allocations$prairie <- result$allocated
      remaining <- result$remaining
    }
  }

  # ===========================================================================
  # Calculate totals for area_change
  # ===========================================================================

  # Sum up what each source loses
  source_losses <- list(
    Bare = allocations$wetland$Bare + allocations$forest$Bare + allocations$prairie$Bare,
    Cropland = allocations$wetland$Cropland + allocations$forest$Cropland + allocations$prairie$Cropland,
    Grassland = allocations$wetland$Grassland + allocations$forest$Grassland,
    Tree = allocations$wetland$Tree
  )

  # Sum up what each target gains
  target_gains <- list(
    Wetland = sum(unlist(allocations$wetland)),
    Tree = sum(unlist(allocations$forest)),
    Grassland = sum(unlist(allocations$prairie))
  )

  # ===========================================================================
  # Handle missing land cover types (create if needed)
  # ===========================================================================

  # Check and add Wetland if needed
  if (target_gains$Wetland > 0 && !"Wetland" %in% df_current$land_cover_type) {
    template_source <- df_current %>%
      filter(land_cover_type %in% c("Bare", "Cropland", "Grassland", "Tree")) %>%
      slice(1)

    wetland_rows <- df_null %>%
      filter(land_cover_type == template_source$land_cover_type[1]) %>%
      group_by(inventory_year) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        land_cover_type = "Wetland",
        area = 0
      )

    # Set potential_wetland_area to 0 if column exists
    if ("potential_wetland_area" %in% colnames(wetland_rows)) {
      wetland_rows <- wetland_rows %>% mutate(potential_wetland_area = 0)
    }

    df_null <- bind_rows(df_null, wetland_rows)
  }

  # Check and add Tree if needed
  if (target_gains$Tree > 0 && !"Tree" %in% df_current$land_cover_type) {
    template_source <- df_current %>%
      filter(land_cover_type %in% c("Bare", "Cropland", "Grassland")) %>%
      slice(1)

    tree_rows <- df_null %>%
      filter(land_cover_type == template_source$land_cover_type[1]) %>%
      group_by(inventory_year) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        land_cover_type = "Tree",
        area = 0
      )

    if ("potential_wetland_area" %in% colnames(tree_rows)) {
      tree_rows <- tree_rows %>% mutate(potential_wetland_area = 0)
    }

    df_null <- bind_rows(df_null, tree_rows)
  }

  # Check and add Grassland if needed
  if (target_gains$Grassland > 0 && !"Grassland" %in% df_current$land_cover_type) {
    template_source <- df_current %>%
      filter(land_cover_type %in% c("Bare", "Cropland")) %>%
      slice(1)

    grassland_rows <- df_null %>%
      filter(land_cover_type == template_source$land_cover_type[1]) %>%
      group_by(inventory_year) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        land_cover_type = "Grassland",
        area = 0
      )

    if ("potential_wetland_area" %in% colnames(grassland_rows)) {
      grassland_rows <- grassland_rows %>% mutate(potential_wetland_area = 0)
    }

    df_null <- bind_rows(df_null, grassland_rows)
  }

  # ===========================================================================
  # Build area_change for each land cover type
  # ===========================================================================

  # Re-fetch current state after adding any new types
  df_current_updated <- df_null %>%
    filter(inventory_year == max(inventory_year))

  df_max <- df_current_updated %>%
    mutate(
      area_change = case_when(
        # Sources lose area (negative change)
        land_cover_type == "Bare" ~ -source_losses$Bare,
        land_cover_type == "Cropland" ~ -source_losses$Cropland,
        # Grassland can both lose (to wetland/forest) and gain (prairie restoration)
        land_cover_type == "Grassland" ~ -source_losses$Grassland + target_gains$Grassland,
        # Tree can both lose (to wetland) and gain (forest restoration)
        land_cover_type == "Tree" ~ -source_losses$Tree + target_gains$Tree,
        # Wetland only gains
        land_cover_type == "Wetland" ~ target_gains$Wetland,
        TRUE ~ 0
      )
    )

  # ===========================================================================
  # Apply land conversion using logistic growth curve
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
  # Attach metadata for optional inspection
  # ===========================================================================
  attr(df_export, "restoration_allocations") <- allocations
  attr(df_export, "restoration_summary") <- list(
    wetland_added_sqkm = target_gains$Wetland,
    forest_added_sqkm = target_gains$Tree,
    prairie_added_sqkm = target_gains$Grassland,
    total_restored_sqkm = sum(unlist(target_gains))
  )

  return(df_export)
}
