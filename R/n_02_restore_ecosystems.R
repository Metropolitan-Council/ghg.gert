#' @title Unified Ecosystem Restoration
#'
#' @description Interface for restoring wetlands, forests, and prairies.
#'   Wetlands use GIS-constrained potential with ambition percentage.
#'   Forests and prairies allow direct acreage specification for flexibility,
#'   as city planners often know their committed acreage.
#'
#'   Area conservation is maintained by drawing from a conversion pool
#'   (Bare, Cropland, and optionally Grassland/Tree). Validation warnings
#'   are returned as attributes if proposed restoration exceeds available land.
#'
#' @param df_null Input dataframe of land cover area estimates (projections from 2023-2050)
#' @param restore_wetland Logical, whether to restore wetlands (default FALSE)
#' @param wetland_ambition_pct Numeric 0-100, percent of wetland potential to realize (default 50)
#' @param forest_area_sqkm Numeric, committed forest restoration area in sq km (default 0)
#' @param prairie_area_sqkm Numeric, committed prairie/grassland restoration area in sq km (default 0)
#' @param start_yr Start year for restoration (default 2025)
#' @param end_yr End year for restoration (default 2050)
#'
#' @return Dataframe with projected land cover areas. Includes attributes:
#'   - "restoration_allocations": detailed breakdown of area by source and target
#'   - "restoration_summary": total area added to each target type
#'   - "validation_warnings": any warnings about exceeding available land
#'   - "validation_info": breakdown of limits and proposed totals
#'
#' @export
#' @import dplyr
#'
#' @examples
#' \dontrun
#' # Restore wetlands at 40% ambition, plus 5 sq km forest and 3 sq km prairie
#' result <- restore_ecosystems(
#'   df_null = my_projections,
#'   restore_wetland = TRUE,
#'   wetland_ambition_pct = 40,
#'   forest_area_sqkm = 5,
#'   prairie_area_sqkm = 3
#' )
#'
#' # View validation info
#' attr(result, "validation_info")
#' attr(result, "validation_warnings")
#' }
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

  # If nothing selected, return unchanged
  if (!restore_wetland && forest_area_sqkm == 0 && prairie_area_sqkm == 0) {
    return(df_null)
  }

  # ===========================================================================
  # Get current state and calculate available land for validation
  # ===========================================================================
  df_current <- df_null %>%
    filter(inventory_year == max(inventory_year))

  # Helper to get area for a land cover type
  get_area <- function(type) {
    df_current %>%
      filter(land_cover_type == type) %>%
      pull(area) %>%
      sum(na.rm = TRUE)
  }

  # Get areas by land cover type
  bare_area <- get_area("Bare")
  cropland_area <- get_area("Cropland")
  grassland_area <- get_area("Grassland")
  tree_area <- get_area("Tree")
  water_area <- get_area("Water")


  # Calculate developed area (sum of all developed types)
  developed_area <- df_current %>%
    filter(grepl("^Developed", land_cover_type)) %>%
    pull(area) %>%
    sum(na.rm = TRUE)

  # Total jurisdictional area

  total_area <- df_current %>%
    pull(area) %>%
    sum(na.rm = TRUE)

  # ===========================================================================
  # Validation limits
  # ===========================================================================
  # Soft limit: total area minus developed and water (realistic restoration ceiling)
  soft_limit_sqkm <- total_area - developed_area - water_area

  # Hard limit: total jurisdictional area (physical impossibility)
  hard_limit_sqkm <- total_area

  # ===========================================================================
  # Initialize tracking
  # ===========================================================================
  remaining <- list(
    Bare = bare_area,
    Cropland = cropland_area,
    Grassland = grassland_area,
    Tree = tree_area
  )

  allocations <- list(
    wetland = list(Bare = 0, Cropland = 0, Grassland = 0, Tree = 0),
    forest = list(Bare = 0, Cropland = 0, Grassland = 0),
    prairie = list(Bare = 0, Cropland = 0)
  )

  validation_warnings <- character(0)

  # ===========================================================================
  # PRIORITY 1: Wetlands (constrained by potential_wetland_area layer)
  # Wetlands are always processed first because they have spatial constraints
  # ===========================================================================
  wetland_target <- 0

  if (restore_wetland) {
    has_potential_col <- "potential_wetland_area" %in% colnames(df_current)

    if (has_potential_col) {
      wetland_potential_by_source <- df_current %>%
        filter(land_cover_type %in% c("Tree", "Grassland", "Bare", "Cropland")) %>%
        mutate(
          actual_potential = pmin(area, potential_wetland_area, na.rm = TRUE),
          actual_potential = ifelse(is.na(actual_potential), 0, actual_potential)
        ) %>%
        select(land_cover_type, actual_potential)

      total_wetland_potential <- sum(wetland_potential_by_source$actual_potential, na.rm = TRUE)
      wetland_target <- total_wetland_potential * (wetland_ambition_pct / 100)

      if (wetland_target > 0 && total_wetland_potential > 0) {
        for (src in c("Bare", "Cropland", "Grassland", "Tree")) {
          src_potential <- wetland_potential_by_source %>%
            filter(land_cover_type == src) %>%
            pull(actual_potential) %>%
            sum(na.rm = TRUE)

          src_allocation <- (src_potential / total_wetland_potential) * wetland_target
          src_allocation <- min(src_allocation, remaining[[src]])
          remaining[[src]] <- remaining[[src]] - src_allocation
          allocations$wetland[[src]] <- src_allocation
        }
      }
    } else {
      warning("No 'potential_wetland_area' column found. Wetland restoration requires this constraint layer.")
    }
  }

  # ===========================================================================
  # VALIDATION: Check if proposed forest + prairie + wetland exceeds limits
  # ===========================================================================
  total_proposed <- wetland_target + forest_area_sqkm + prairie_area_sqkm

  if (total_proposed > hard_limit_sqkm) {
    validation_warnings <- c(
      validation_warnings,
      sprintf(
        "HARD LIMIT EXCEEDED: Proposed restoration (%.2f sq km) exceeds total jurisdictional area (%.2f sq km). Values will be capped.",
        total_proposed, hard_limit_sqkm
      )
    )
    # Cap forest and prairie proportionally if they exceed hard limit
    available_after_wetland <- hard_limit_sqkm - wetland_target
    if (forest_area_sqkm + prairie_area_sqkm > available_after_wetland) {
      scale_factor <- available_after_wetland / (forest_area_sqkm + prairie_area_sqkm)
      forest_area_sqkm <- forest_area_sqkm * scale_factor
      prairie_area_sqkm <- prairie_area_sqkm * scale_factor
    }
  } else if (total_proposed > soft_limit_sqkm) {
    validation_warnings <- c(
      validation_warnings,
      sprintf(
        "WARNING: Proposed restoration (%.2f sq km) exceeds realistic limit (%.2f sq km = total area minus developed and water). Consider reducing targets.",
        total_proposed, soft_limit_sqkm
      )
    )
  }

  # ===========================================================================
  # FOREST ALLOCATION (direct acreage)
  # Priority: Bare -> Cropland -> Grassland
  # ===========================================================================
  forest_allocated <- 0

  if (forest_area_sqkm > 0) {
    forest_still_needed <- forest_area_sqkm

    # Allocate from Bare first
    take_bare <- min(forest_still_needed, remaining$Bare)
    allocations$forest$Bare <- take_bare
    remaining$Bare <- remaining$Bare - take_bare
    forest_still_needed <- forest_still_needed - take_bare

    # Then Cropland
    if (forest_still_needed > 0) {
      take_crop <- min(forest_still_needed, remaining$Cropland)
      allocations$forest$Cropland <- take_crop
      remaining$Cropland <- remaining$Cropland - take_crop
      forest_still_needed <- forest_still_needed - take_crop
    }

    # Then Grassland
    if (forest_still_needed > 0) {
      take_grass <- min(forest_still_needed, remaining$Grassland)
      allocations$forest$Grassland <- take_grass
      remaining$Grassland <- remaining$Grassland - take_grass
      forest_still_needed <- forest_still_needed - take_grass
    }

    forest_allocated <- forest_area_sqkm - forest_still_needed

    if (forest_still_needed > 0) {
      validation_warnings <- c(
        validation_warnings,
        sprintf(
          "FOREST: Only %.2f of %.2f sq km could be allocated (insufficient source land).",
          forest_allocated, forest_area_sqkm
        )
      )
    }
  }

  # ===========================================================================
  # PRAIRIE ALLOCATION (direct acreage)
  # Priority: Bare -> Cropland (cannot use Grassland - that would be converting grassland to grassland)
  # ===========================================================================
  prairie_allocated <- 0

  if (prairie_area_sqkm > 0) {
    prairie_still_needed <- prairie_area_sqkm

    # Allocate from Bare first
    take_bare <- min(prairie_still_needed, remaining$Bare)
    allocations$prairie$Bare <- take_bare
    remaining$Bare <- remaining$Bare - take_bare
    prairie_still_needed <- prairie_still_needed - take_bare

    # Then Cropland
    if (prairie_still_needed > 0) {
      take_crop <- min(prairie_still_needed, remaining$Cropland)
      allocations$prairie$Cropland <- take_crop
      remaining$Cropland <- remaining$Cropland - take_crop
      prairie_still_needed <- prairie_still_needed - take_crop
    }

    prairie_allocated <- prairie_area_sqkm - prairie_still_needed

    if (prairie_still_needed > 0) {
      validation_warnings <- c(
        validation_warnings,
        sprintf(
          "PRAIRIE: Only %.2f of %.2f sq km could be allocated (insufficient source land).",
          prairie_allocated, prairie_area_sqkm
        )
      )
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
  # Attach metadata for inspection and UI feedback
  # ===========================================================================
  attr(df_export, "restoration_allocations") <- allocations

  attr(df_export, "restoration_summary") <- list(
    wetland_added_sqkm = target_gains$Wetland,
    forest_added_sqkm = target_gains$Tree,
    prairie_added_sqkm = target_gains$Grassland,
    total_restored_sqkm = sum(unlist(target_gains))
  )

  attr(df_export, "validation_warnings") <- validation_warnings

  attr(df_export, "validation_info") <- list(
    total_area_sqkm = total_area,
    developed_area_sqkm = developed_area,
    water_area_sqkm = water_area,
    soft_limit_sqkm = soft_limit_sqkm,
    hard_limit_sqkm = hard_limit_sqkm,
    total_proposed_sqkm = total_proposed,
    wetland_proposed_sqkm = wetland_target,
    forest_proposed_sqkm = forest_area_sqkm,
    prairie_proposed_sqkm = prairie_area_sqkm,
    forest_allocated_sqkm = forest_allocated,
    prairie_allocated_sqkm = prairie_allocated
  )

  return(df_export)
}
