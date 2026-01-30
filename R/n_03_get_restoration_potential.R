#' @title Get Restoration Potential for a Jurisdiction
#'
#' @description Calculates the maximum restorable area for wetlands, forests,
#'   and prairies based on available land and constraint layers. Useful for
#'   informing UI elements about what's achievable.
#'
#' @param df_null Input dataframe of land cover area projections
#' @param grassland_available_pct Percent of grassland that can be converted (default 50)
#'
#' @return A list containing:
#'   - wetland: max wetland restoration potential (sq km)
#'   - forest: max forest restoration potential (sq km)
#'   - prairie: max prairie restoration potential (sq km)
#'   - total_pool: total convertible area (sq km)
#'   - by_source: breakdown of available area by source type
#'
#' @export
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' potential <- get_restoration_potential(my_projections)
#' # Returns list with $wetland, $forest, $prairie, $total_pool, $by_source
#' }
get_restoration_potential <- function(df_null,
                                      grassland_available_pct = 50) {

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

  # Calculate available areas by source type
  cropland_area <- get_area("Cropland")
  bare_area <- get_area("Bare")
  grassland_area <- get_area("Grassland")
  tree_area <- get_area("Tree")

  # Grassland available for conversion (user-specified fraction)
  grassland_available <- grassland_area * (grassland_available_pct / 100)

  # Total conversion pool (for forest/prairie - excluding trees)
  total_pool <- bare_area + cropland_area + grassland_available

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
  # Forest potential = total pool (theoretical max if nothing goes to wetland)
  # ---------------------------------------------------------------------------
  forest_potential <- total_pool

  # ---------------------------------------------------------------------------
  # Prairie potential = bare + cropland (grassland can't become grassland)
  # ---------------------------------------------------------------------------
  prairie_potential <- bare_area + cropland_area

  list(
    wetland = wetland_potential,
    forest = forest_potential,
    prairie = prairie_potential,
    total_pool = total_pool,
    by_source = list(
      bare = bare_area,
      cropland = cropland_area,
      grassland_total = grassland_area,
      grassland_available = grassland_available,
      tree = tree_area
    )
  )
}


#' @title Get Restoration Summary for UI Display
#'
#' @description Returns a tidy summary of restoration allocations for display
#'   in the UI. Shows what land conversions would occur at a given ambition level.
#'
#' @param df_null Input dataframe of land cover projections
#' @param restore_wetland Logical, include wetland restoration
#' @param restore_forest Logical, include forest restoration
#' @param restore_prairie Logical, include prairie restoration
#' @param ambition_pct Numeric 0-100, ambition level
#' @param grassland_available_pct Percent of grassland available for conversion
#'
#' @return A tibble with columns: source, target, area_sqkm
#'
#' @export
#' @import dplyr
#' @import tibble
#'
#' @examples
#' \dontrun
#' summary <- get_restoration_summary(
#'   df_null = my_projections,
#'   restore_wetland = TRUE,
#'   restore_forest = TRUE,
#'   ambition_pct = 40
#' )
#' # Returns tibble showing source → target conversions
#' }
get_restoration_summary <- function(df_null,
                                    restore_wetland = TRUE,
                                    restore_forest = TRUE,
                                    restore_prairie = FALSE,
                                    ambition_pct = 50,
                                    grassland_available_pct = 50) {

  # Run the restoration to get allocations (with dummy years)
  result <- restore_ecosystems(
    df_null = df_null,
    restore_wetland = restore_wetland,
    restore_forest = restore_forest,
    restore_prairie = restore_prairie,
    ambition_pct = ambition_pct,
    start_yr = 2025,
    end_yr = 2050,
    grassland_available_pct = grassland_available_pct
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
#' @param restore_forest Logical
#' @param restore_prairie Logical
#' @param ambition_pct Numeric 0-100
#'
#' @return Character string describing the restoration plan
#'
#' @export
format_restoration_summary <- function(df_null,
                                       restore_wetland = TRUE,
                                       restore_forest = TRUE,
                                       restore_prairie = FALSE,
                                       ambition_pct = 50) {

  summary_df <- get_restoration_summary(
    df_null = df_null,
    restore_wetland = restore_wetland,
    restore_forest = restore_forest,
    restore_prairie = restore_prairie,
    ambition_pct = ambition_pct
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
        sprintf("%.1f sq km from %s", area_sqkm, source),
        collapse = ", "
      ),
      .groups = "drop"
    )

  # Build output string
  lines <- by_target %>%
    mutate(
      line = sprintf("%s: +%.1f sq km (%s)", target, total_area, sources)
    ) %>%
    pull(line)

  total_restored <- sum(summary_df$area_sqkm)

  paste0(
    paste(lines, collapse = "\n"),
    sprintf("\n\nTotal restored: %.1f sq km", total_restored)
  )
}
