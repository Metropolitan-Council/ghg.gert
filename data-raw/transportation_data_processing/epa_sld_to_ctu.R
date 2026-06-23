## EPA SLD to CTU Aggregation
## Aggregates block group-level intersection density to CTU (City/Township/Unorganized) level
## Uses area-weighted spatial averaging
## Author: Automated via Copilot CLI
## Date: 2026-06-23

library(sf)
library(dplyr)
library(tidyr)

# Configuration ----
OUTPUT_DIR <- "data-raw/transportation_data_processing/processed_data"
CSV_DIR <- "data-raw/transportation_data_processing/csv_copies"

# Input files
BLOCKGROUP_FILE <- file.path(OUTPUT_DIR, "mn_sld_blockgroups.RDS")
CTU_FILE <- "data-raw/meta/ccap_ctu.RDS"

# Output files
OUTPUT_WIDE_RDS <- file.path(OUTPUT_DIR, "ctu_intersection_density_all_metrics.RDS")
OUTPUT_LONG_RDS <- file.path(OUTPUT_DIR, "ctu_intersection_density_long.RDS")
OUTPUT_WIDE_CSV <- file.path(CSV_DIR, "ctu_intersection_density_all_metrics.csv")
OUTPUT_LONG_CSV <- file.path(CSV_DIR, "ctu_intersection_density_long.csv")

# D3 metrics to aggregate
D3_METRICS <- c(
  "D3A", "D3AAO", "D3AMM", "D3APO",
  "D3B", "D3BAO", "D3BMM3", "D3BMM4",
  "D3BPO3", "D3BPO4"
)

# Aggregate block groups to CTUs ----
aggregate_sld_to_ctu <- function() {
  if (!file.exists(BLOCKGROUP_FILE)) {
    stop("Block group data not found. Run epa_sld_download.R first.")
  }
  mn_sld <- readRDS(BLOCKGROUP_FILE)

  if (!file.exists(CTU_FILE)) {
    stop("CTU boundary data not found. Expected: ", CTU_FILE)
  }
  ccap_ctu <- readRDS(CTU_FILE)

  # Ensure same CRS and fix invalid geometries
  mn_sld <- st_transform(mn_sld, st_crs(ccap_ctu))
  sf_use_s2(FALSE)
  mn_sld <- st_make_valid(mn_sld)
  ccap_ctu <- st_make_valid(ccap_ctu)

  message("Aggregating EPA SLD to ", nrow(ccap_ctu), " CTUs...")

  # Calculate block group areas (for weighting)
  mn_sld <- mn_sld %>%
    mutate(bg_area = as.numeric(st_area(geometry)))

  # Intersect block groups with CTUs
  intersected <- st_intersection(
    mn_sld %>% select(GEOID10, bg_area, all_of(D3_METRICS)),
    ccap_ctu %>% select(geog_name, ctu_class, county_name)
  )

  # Calculate intersection piece areas and area-weighted averages
  intersected <- intersected %>%
    mutate(piece_area = as.numeric(st_area(geometry)))

  ctu_metrics <- intersected %>%
    st_drop_geometry() %>%
    # For each CTU and metric, calculate weighted average
    group_by(geog_name, ctu_class, county_name) %>%
    summarise(
      across(
        all_of(D3_METRICS),
        ~ {
          # Area-weighted average: sum(value * area) / sum(area)
          # Handle NA values
          valid_idx <- !is.na(.x) & !is.na(piece_area)
          if (sum(valid_idx) == 0) {
            return(NA_real_)
          }
          weighted_sum <- sum(.x[valid_idx] * piece_area[valid_idx], na.rm = TRUE)
          total_area <- sum(piece_area[valid_idx], na.rm = TRUE)
          if (total_area == 0) NA_real_ else weighted_sum / total_area
        },
        .names = "{.col}"
      ),
      total_area_sqm = sum(piece_area, na.rm = TRUE),
      n_blockgroups = n_distinct(GEOID10),
      .groups = "drop"
    )

  # Convert area to square miles for reference
  SQM_TO_SQMI <- 3.861e-7
  ctu_metrics <- ctu_metrics %>%
    mutate(total_area_sqmi = total_area_sqm * SQM_TO_SQMI)

  # Create long format (tidy)
  ctu_metrics_long <- ctu_metrics %>%
    select(geog_name, ctu_class, county_name, all_of(D3_METRICS)) %>%
    pivot_longer(
      cols = all_of(D3_METRICS),
      names_to = "metric",
      values_to = "value"
    )

  # Add metric descriptions
  metric_descriptions <- tribble(
    ~metric, ~description,
    "D3A", "Total road network density (intersections/sq mi)",
    "D3AAO", "Auto-oriented intersection density",
    "D3AMM", "Multi-modal link density",
    "D3APO", "Pedestrian-oriented link density",
    "D3B", "Street intersection density (weighted, auto-oriented eliminated)",
    "D3BAO", "Auto-oriented intersections per sq mi",
    "D3BMM3", "Multi-modal 3-way intersections per sq mi",
    "D3BMM4", "Multi-modal 4+ way intersections per sq mi",
    "D3BPO3", "Pedestrian-oriented 3-way intersections per sq mi",
    "D3BPO4", "Pedestrian-oriented 4+ way intersections per sq mi"
  )

  ctu_metrics_long <- ctu_metrics_long %>%
    left_join(metric_descriptions, by = "metric")

  # Save outputs
  if (!dir.exists(CSV_DIR)) {
    dir.create(CSV_DIR, recursive = TRUE)
  }

  saveRDS(ctu_metrics, OUTPUT_WIDE_RDS)
  ctu_metrics %>%
    st_drop_geometry() %>%
    write.csv(OUTPUT_WIDE_CSV, row.names = FALSE)

  saveRDS(ctu_metrics_long, OUTPUT_LONG_RDS)
  write.csv(ctu_metrics_long, OUTPUT_LONG_CSV, row.names = FALSE)

  # Check for missing data
  n_complete <- sum(complete.cases(ctu_metrics %>% select(all_of(D3_METRICS))))
  message("Aggregated ", nrow(ctu_metrics), " CTUs (", n_complete, " complete)")

  missing_by_metric <- ctu_metrics %>%
    st_drop_geometry() %>%
    summarise(across(all_of(D3_METRICS), ~ sum(is.na(.x))))

  if (any(missing_by_metric > 0)) {
    warning("Some CTUs have missing values")
  }

  return(list(
    wide = ctu_metrics,
    long = ctu_metrics_long
  ))
}

# Main execution ----
if (!interactive()) {
  # When sourced as a script, run the aggregation
  ctu_data <- aggregate_sld_to_ctu()
}
