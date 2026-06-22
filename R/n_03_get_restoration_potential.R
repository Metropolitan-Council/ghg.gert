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
