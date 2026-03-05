#' @title Get Restoration Potential for a Jurisdiction
#'
#' @description Calculates the maximum restorable area for wetlands, forests,
#'   and prairies based on available land. Returns validation limits for UI
#'   input constraints and warning thresholds.
#'
#'   Wetlands are constrained by the GIS potential_wetland_area layer.
#'   Forests and prairies have flexible limits based on available non-developed,
#'   non-water land area.
#'
#' @param df_null Input dataframe of land cover area projections
#'
#' @return A list containing:
#'   - wetland_potential_sqkm: max wetland restoration potential (GIS-constrained)
#'   - soft_limit_sqkm: total area minus developed and water (realistic ceiling)
#'   - hard_limit_sqkm: total jurisdictional area (physical limit)
#'   - by_source: breakdown of available area by source type
#'   - by_land_cover: current area for each land cover type
#'
#' @export
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' potential <- get_restoration_potential(my_projections)
#'
#' # Use for UI validation
#' if (user_forest + user_prairie > potential$soft_limit_sqkm) {
#'   show_warning("Exceeds realistic restoration potential")
#' }
#' }
get_restoration_potential <- function(df_null) {
  # Get the most recent year's data
  df_current <- df_null %>%
    filter(inventory_year == max(inventory_year))

  # Helper to get area for a land cover type
  get_area <- function(type) {
    df_current %>%
      filter(land_cover_type == type) %>%
      pull(area) %>%
      sum(na.rm = TRUE)
  }

  # Calculate areas by land cover type
  cropland_area <- get_area("Cropland")
  bare_area <- get_area("Bare")
  grassland_area <- get_area("Grassland")
  tree_area <- get_area("Tree")
  water_area <- get_area("Water")
  wetland_area <- get_area("Wetland")

  # Calculate developed area (sum of all developed types)
  developed_area <- df_current %>%
    filter(grepl("^Developed", land_cover_type)) %>%
    pull(area) %>%
    sum(na.rm = TRUE)

  # Total jurisdictional area
  total_area <- df_current %>%
    pull(area) %>%
    sum(na.rm = TRUE)

  # ---------------------------------------------------------------------------
  # Validation limits for forest/prairie (flexible inputs)
  # ---------------------------------------------------------------------------
  # Soft limit: total area minus developed and water
  soft_limit_sqkm <- total_area - developed_area - water_area

  # Hard limit: total jurisdictional area
  hard_limit_sqkm <- total_area

  # ---------------------------------------------------------------------------
  # Wetland potential is constrained by the potential_wetland_area layer
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
  # Conversion pool (what's available for forest/prairie conversion)
  # ---------------------------------------------------------------------------
  conversion_pool <- bare_area + cropland_area + grassland_area

  list(
    # GIS-constrained wetland potential
    wetland_potential_sqkm = wetland_potential,

    # Flexible limits for forest/prairie
    soft_limit_sqkm = soft_limit_sqkm,
    hard_limit_sqkm = hard_limit_sqkm,

    # Conversion pool breakdown
    conversion_pool_sqkm = conversion_pool,
    by_source = list(
      bare_sqkm = bare_area,
      cropland_sqkm = cropland_area,
      grassland_sqkm = grassland_area,
      tree_sqkm = tree_area
    ),

    # Full land cover breakdown for reference
    by_land_cover = list(
      bare_sqkm = bare_area,
      cropland_sqkm = cropland_area,
      grassland_sqkm = grassland_area,
      tree_sqkm = tree_area,
      wetland_sqkm = wetland_area,
      water_sqkm = water_area,
      developed_sqkm = developed_area,
      total_sqkm = total_area
    )
  )
}


#' @title Get Restoration Summary for UI Display
#'
#' @description Returns a tidy summary of restoration allocations for display
#'   in the UI. Shows what land conversions would occur with given inputs.
#'
#' @param df_null Input dataframe of land cover projections
#' @param restore_wetland Logical, include wetland restoration
#' @param wetland_ambition_pct Numeric 0-100, ambition level for wetlands
#' @param forest_area_sqkm Numeric, committed forest area in sq km
#' @param prairie_area_sqkm Numeric, committed prairie area in sq km
#'
#' @return A tibble with columns: source, target, area_sqkm
#'
#' @export
#' @import dplyr
#' @import tibble
#'
#' @examples
#' \dontrun{
#' summary <- get_restoration_summary(
#'   df_null = my_projections,
#'   restore_wetland = TRUE,
#'   wetland_ambition_pct = 40,
#'   forest_area_sqkm = 5,
#'   prairie_area_sqkm = 3
#' )
#' # Returns tibble showing source → target conversions
#' }
#'
get_restoration_summary <- function(df_null,
                                    restore_wetland = FALSE,
                                    wetland_ambition_pct = 50,
                                    forest_area_sqkm = 0,
                                    prairie_area_sqkm = 0) {
  # Run the restoration to get allocations (with dummy years)
  result <- restore_ecosystems(
    df_null = df_null,
    restore_wetland = restore_wetland,
    wetland_ambition_pct = wetland_ambition_pct,
    forest_area_sqkm = forest_area_sqkm,
    prairie_area_sqkm = prairie_area_sqkm,
    start_yr = 2025,
    end_yr = 2050
  )

  allocations <- attr(result, "restoration_allocations")
  summary_stats <- attr(result, "restoration_summary")

  # Build a tidy summary table
  summary_rows <- list()

  # Wetland allocations
  if (!is.null(allocations$wetland)) {
    for (src in names(allocations$wetland)) {
      if (allocations$wetland[[src]] > 0) {
        summary_rows <- append(summary_rows, list(
          tibble(
            source = src,
            target = "Wetland",
            area_sqkm = allocations$wetland[[src]]
          )
        ))
      }
    }
  }

  # Forest allocations
  if (!is.null(allocations$forest)) {
    for (src in names(allocations$forest)) {
      if (allocations$forest[[src]] > 0) {
        summary_rows <- append(summary_rows, list(
          tibble(
            source = src,
            target = "Forest",
            area_sqkm = allocations$forest[[src]]
          )
        ))
      }
    }
  }

  # Prairie allocations
  if (!is.null(allocations$prairie)) {
    for (src in names(allocations$prairie)) {
      if (allocations$prairie[[src]] > 0) {
        summary_rows <- append(summary_rows, list(
          tibble(
            source = src,
            target = "Prairie",
            area_sqkm = allocations$prairie[[src]]
          )
        ))
      }
    }
  }

  # Return empty tibble if nothing allocated
  if (length(summary_rows) == 0) {
    return(tibble(
      source = character(),
      target = character(),
      area_sqkm = numeric()
    ))
  }

  bind_rows(summary_rows) %>%
    arrange(target, desc(area_sqkm))
}


#' @title Format Restoration Summary for Display
#'
#' @description Formats the restoration summary as a human-readable string
#'   suitable for display in a UI tooltip or info panel.
#'
#' @param df_null Input dataframe of land cover projections
#' @param restore_wetland Logical
#' @param wetland_ambition_pct Numeric 0-100
#' @param forest_area_sqkm Numeric
#' @param prairie_area_sqkm Numeric
#'
#' @return Character string describing the restoration plan
#'
#' @export
format_restoration_summary <- function(df_null,
                                       restore_wetland = FALSE,
                                       wetland_ambition_pct = 50,
                                       forest_area_sqkm = 0,
                                       prairie_area_sqkm = 0) {
  summary_df <- get_restoration_summary(
    df_null = df_null,
    restore_wetland = restore_wetland,
    wetland_ambition_pct = wetland_ambition_pct,
    forest_area_sqkm = forest_area_sqkm,
    prairie_area_sqkm = prairie_area_sqkm
  )

  if (nrow(summary_df) == 0) {
    return("No restoration selected or no eligible land available.")
  }

  # Group by target and summarize
  by_target <- summary_df %>%
    group_by(target) %>%
    summarize(
      total_area = sum(area_sqkm),
      sources = paste(
        sprintf("%.2f sq km from %s", area_sqkm, source),
        collapse = ", "
      ),
      .groups = "drop"
    )

  # Build output string
  lines <- by_target %>%
    mutate(
      line = sprintf("%s: +%.2f sq km (%s)", target, total_area, sources)
    ) %>%
    pull(line)

  total_restored <- sum(summary_df$area_sqkm)

  paste0(
    paste(lines, collapse = "\n"),
    sprintf("\n\nTotal restored: %.2f sq km", total_restored)
  )
}


#' @title Validate Restoration Inputs
#'
#' @description Validates proposed restoration inputs against jurisdiction limits.
#'   Returns a list with validation status and any warning messages.
#'   Useful for real-time UI validation.
#'
#' @param df_null Input dataframe of land cover projections
#' @param restore_wetland Logical
#' @param wetland_ambition_pct Numeric 0-100
#' @param forest_area_sqkm Numeric
#' @param prairie_area_sqkm Numeric
#'
#' @return List with:
#'   - valid: Logical, TRUE if within hard limits
#'   - warnings: Character vector of warning messages
#'   - proposed_total_sqkm: Total proposed restoration
#'   - soft_limit_sqkm: Realistic ceiling
#'   - hard_limit_sqkm: Physical maximum
#'
#' @export
validate_restoration_inputs <- function(df_null,
                                        restore_wetland = FALSE,
                                        wetland_ambition_pct = 50,
                                        forest_area_sqkm = 0,
                                        prairie_area_sqkm = 0) {
  potential <- get_restoration_potential(df_null)

  # Calculate wetland target based on ambition
  wetland_target <- 0
  if (restore_wetland) {
    wetland_target <- potential$wetland_potential_sqkm * (wetland_ambition_pct / 100)
  }

  total_proposed <- wetland_target + forest_area_sqkm + prairie_area_sqkm

  warnings <- character(0)
  valid <- TRUE

  # Check against hard limit
  if (total_proposed > potential$hard_limit_sqkm) {
    valid <- FALSE
    warnings <- c(
      warnings,
      sprintf(
        "EXCEEDS HARD LIMIT: Total proposed (%.2f sq km) exceeds jurisdictional area (%.2f sq km).",
        total_proposed, potential$hard_limit_sqkm
      )
    )
  }

  # Check against soft limit
  if (total_proposed > potential$soft_limit_sqkm && valid) {
    warnings <- c(
      warnings,
      sprintf(
        "Exceeds realistic limit: Total proposed (%.2f sq km) exceeds available non-developed/water area (%.2f sq km).",
        total_proposed, potential$soft_limit_sqkm
      )
    )
  }

  # Check if forest + prairie exceeds conversion pool
  if (forest_area_sqkm + prairie_area_sqkm > potential$conversion_pool_sqkm) {
    warnings <- c(
      warnings,
      sprintf(
        "Forest + Prairie (%.2f sq km) exceeds available conversion pool (%.2f sq km bare + cropland + grassland).",
        forest_area_sqkm + prairie_area_sqkm, potential$conversion_pool_sqkm
      )
    )
  }

  list(
    valid = valid,
    warnings = warnings,
    proposed_total_sqkm = total_proposed,
    wetland_proposed_sqkm = wetland_target,
    forest_proposed_sqkm = forest_area_sqkm,
    prairie_proposed_sqkm = prairie_area_sqkm,
    soft_limit_sqkm = potential$soft_limit_sqkm,
    hard_limit_sqkm = potential$hard_limit_sqkm,
    conversion_pool_sqkm = potential$conversion_pool_sqkm
  )
}
