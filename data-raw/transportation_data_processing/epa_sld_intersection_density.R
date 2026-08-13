## EPA SLD to CTU Aggregation
## Aggregates block group-level intersection density to CTU (City/Township/Unorganized) level
## Uses area-weighted spatial averaging

library(sf)
library(dplyr)
library(tidyr)
library(purrr)
sf_use_s2(FALSE)

pkgload::load_all()

# Configuration ----
OUTPUT_DIR <- "data-raw/transportation_data_processing/processed_data"
CSV_DIR <- "data-raw/transportation_data_processing/csv_copies"

# Input files
BLOCKGROUP_FILE <- file.path(OUTPUT_DIR, "mn_sld_blockgroups.RDS")
CTU_FILE <- "data-raw/meta/ccap_ctu.RDS"

# Output files
OUTPUT_LONG_RDS <- file.path(OUTPUT_DIR, "ctu_intersection_density_long.RDS")
OUTPUT_LONG_CSV <- file.path(CSV_DIR, "ctu_intersection_density_long.csv")

# D3 metrics to aggregate
D3_METRICS <- c(
  "D3A", "D3AAO", "D3AMM", "D3APO",
  "D3B", "D3BAO", "D3BMM3", "D3BMM4",
  "D3BPO3", "D3BPO4"
)

# Aggregate block groups to CTUs ----
if (!file.exists(CTU_FILE)) {
  stop("CTU boundary data not found. Expected: ", CTU_FILE)
}

ccap_ctu <- readRDS(CTU_FILE) %>%
  filter(ctu_id_gnis %in% geog_index$geog_id) %>%
  sf::st_make_valid() %>%
  select(ctu_id_gnis) %>%
  left_join(geog_index %>% select(ctu_id_gnis = geog_id, geog_name, geog_short_name, geog_id_type, geog_level), by = "ctu_id_gnis")

if (!file.exists(BLOCKGROUP_FILE)) {
  stop("Block group data not found. Run epa_sld_download.R first.")
}

mn_sld <- readRDS(BLOCKGROUP_FILE) %>%
  st_transform(st_crs(ccap_ctu)) %>%
  st_make_valid() %>%
  mutate(
    bg_area = as.numeric(AC_LAND) %>%
      units::as_units("acre") %>%
      units::set_units("m^2") %>%
      as.numeric(),
    county_fips = substr(GEOID20, 1, 5)
  ) %>%
  filter(county_fips %in% geog_index$geog_id[geog_index$geog_level == "COUNTY"])
# Ensure same CRS and fix invalid geometries

message("Aggregating EPA SLD to ", nrow(ccap_ctu), " CTUs...")

# Intersect block groups with CTUs
intersected <- st_intersection(
  mn_sld %>% select(GEOID20, bg_area, county_fips, all_of(D3_METRICS)),
  ccap_ctu %>% select(ctu_id_gnis) %>% unique()
) %>%
  # Calculate intersection piece areas and area-weighted averages
  mutate(piece_area = st_area(geometry) %>%
    units::set_units("m^2") %>%
    as.numeric())

epa_sld_ctu <- intersected %>%
  st_drop_geometry() %>%
  # For each CTU and metric, calculate weighted average
  group_by(ctu_id_gnis) %>%
  summarise(
    across(
      all_of(D3_METRICS),
      ~ {
        # Handle NA values
        valid_idx <- !is.na(.x) & !is.na(piece_area)
        if (sum(valid_idx) == 0) {
          return(NA_real_)
        }
        weighted_mean <- weighted.mean(.x[valid_idx], w = piece_area[valid_idx], na.rm = TRUE) %>%
          round(digits = 2)
        total_area <- sum(piece_area[valid_idx], na.rm = TRUE)
        if (total_area == 0) NA_real_ else weighted_mean
      },
      .names = "{.col}"
    ),
    total_area_meters = sum(piece_area, na.rm = TRUE),
    n_blockgroups = n_distinct(GEOID20),
    .groups = "drop"
  ) %>%
  rename(geog_id = ctu_id_gnis) %>%
  # Add geog_index columns
  left_join(geog_index %>% filter(!geog_level %in% c("COUNTY", "REGION")) %>% select(geog_name, geog_id_type, geog_id, geog_level), by = c("geog_id"))


epa_sld_region <- intersected %>%
  st_drop_geometry() %>%
  # For each CTU and metric, calculate weighted average
  # group_by(ctu_id_gnis) %>%
  summarise(
    across(
      all_of(D3_METRICS),
      ~ {
        # Handle NA values
        valid_idx <- !is.na(.x) & !is.na(piece_area)
        if (sum(valid_idx) == 0) {
          return(NA_real_)
        }
        weighted_mean <- weighted.mean(.x[valid_idx], w = piece_area[valid_idx], na.rm = TRUE) %>%
          round(digits = 2)
        total_area <- sum(piece_area[valid_idx], na.rm = TRUE)
        if (total_area == 0) NA_real_ else weighted_mean
      },
      .names = "{.col}"
    ),
    total_area_meters = sum(piece_area, na.rm = TRUE),
    n_blockgroups = n_distinct(GEOID20),
    .groups = "drop"
  ) %>%
  mutate(
    geog_id = "00000000",
  ) %>%
  left_join(geog_index %>% filter(geog_level == "REGION") %>% select(geog_name, geog_id_type, geog_id, geog_level), by = c("geog_id"))


# Add geog_index columns


epa_sld_county <- intersected %>%
  st_drop_geometry() %>%
  # For each CTU and metric, calculate weighted average
  group_by(county_fips) %>%
  summarise(
    across(
      all_of(D3_METRICS),
      ~ {
        # Handle NA values
        valid_idx <- !is.na(.x) & !is.na(piece_area)
        if (sum(valid_idx) == 0) {
          return(NA_real_)
        }
        weighted_mean <- weighted.mean(.x[valid_idx], w = piece_area[valid_idx], na.rm = TRUE) %>%
          round(digits = 2)
        total_area <- sum(piece_area[valid_idx], na.rm = TRUE)
        if (total_area == 0) NA_real_ else weighted_mean
      },
      .names = "{.col}"
    ),
    total_area_meters = sum(piece_area, na.rm = TRUE),
    n_blockgroups = n_distinct(GEOID20),
    .groups = "drop"
  ) %>%
  rename(geog_id = county_fips) %>%
  left_join(geog_index %>% filter(geog_level == "COUNTY") %>% select(geog_name, geog_id_type, geog_id, geog_level), by = c("geog_id"))


epa_sld <- bind_rows(epa_sld_ctu, epa_sld_county, epa_sld_region) %>%
  select(geog_name, geog_id, geog_id_type, geog_level, all_of(D3_METRICS), total_area_meters, n_blockgroups) %>%
  unique() # removes duplicate values for cities in multiple counties

# Create long format (tidy)
epa_sld_ctu_long <- epa_sld_ctu %>%
  bind_rows(epa_sld_county) %>%
  bind_rows(epa_sld_region) %>%
  select(
    geog_name,
    geog_id,
    geog_id_type, geog_level, all_of(D3_METRICS)
  ) %>%
  pivot_longer(
    cols = all_of(D3_METRICS),
    names_to = "metric",
    values_to = "value"
  ) %>%
  unique() # removes duplicate values for cities in multiple counties

# Add metric descriptions
# note that the denominator for sq mile is based on ALAND, not total area
metric_descriptions <- jsonlite::read_json("data-raw/transportation_data_processing/processed_data/epa_sld_metadata.json") %>%
  pluck("fields") %>%
  purrr::map_dfr(~ tibble(metric = .x$name, description = .x$alias)) %>%
  filter(metric %in% D3_METRICS)

epa_sld_ctu_long <- epa_sld_ctu_long %>%
  left_join(metric_descriptions, by = "metric")

# Save outputs
if (!dir.exists(CSV_DIR)) {
  dir.create(CSV_DIR, recursive = TRUE, showWarnings = FALSE)
}

saveRDS(epa_sld_ctu_long, OUTPUT_LONG_RDS)
write.csv(epa_sld_ctu_long, OUTPUT_LONG_CSV, row.names = FALSE)

# Check for missing data
n_complete <- sum(complete.cases(epa_sld %>% select(all_of(D3_METRICS))))
message("Aggregated ", nrow(epa_sld), " geographies")

missing_by_metric <- epa_sld %>%
  st_drop_geometry() %>%
  summarise(across(all_of(D3_METRICS), ~ sum(is.na(.x))))

if (nrow(geog_index) != nrow(epa_sld)) {
  warning("Number of geographies in geog_index (", nrow(geog_index), ") does not match number of geographies in aggregated data (", nrow(epa_sld), ").")
}

if (any(missing_by_metric > 0)) {
  warning("Some geographies have missing values")
}

epa_sld_intersection_density <- epa_sld_ctu_long %>%
  filter(metric == "D3A") %>%
  select(geog_id, geog_name, geog_id_type, value) %>%
  unique() %>%
  rename(intersection_density = value)

usethis::use_data(epa_sld_intersection_density, overwrite = TRUE)
