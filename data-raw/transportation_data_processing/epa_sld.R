## EPA Smart Location Database (SLD) Data Download
## Queries EPA ArcGIS REST service for Minnesota block group intersection density data

library(arcgislayers)
library(sf)
library(dplyr)
library(jsonlite)

# Configuration ----
EPA_SLD_URL <- "https://geodata.epa.gov/arcgis/rest/services/OA/SmartLocationDatabase/MapServer/10"
MN_STATEFP <- "27"
OUTPUT_DIR <- "data-raw/transportation_data_processing/processed_data"
CACHE_FILE <- file.path(OUTPUT_DIR, "mn_sld_blockgroups.RDS")
METADATA_FILE <- file.path(OUTPUT_DIR, "epa_sld_metadata.json")

dir.create("data-raw/transportation_data_processing/processed_data", recursive = TRUE)

# Minnesota projection (UTM Zone 15N)
MN_CRS <- 26915

# D3 intersection density fields to retrieve
D3_FIELDS <- c(
  "D3A", # Total road network density
  "D3AAO", # Auto-oriented intersection density
  "D3AMM", # Multi-modal link density
  "D3APO", # Pedestrian-oriented link density
  "D3B", # Street intersection density (weighted, auto-oriented eliminated)
  "D3BAO", # Auto-oriented intersection density per sq mi
  "D3BMM3", # Multi-modal 3-way intersections per sq mi
  "D3BMM4", # Multi-modal 4+ way intersections per sq mi
  "D3BPO3", # Pedestrian-oriented 3-way intersections per sq mi
  "D3BPO4" # Pedestrian-oriented 4+ way intersections per sq mi
)

# Check if cache is recent ----
cache_is_fresh <- function(cache_file, days = CACHE_DAYS) {
  if (!file.exists(cache_file)) {
    return(FALSE)
  }

  cache_age <- difftime(Sys.time(), file.info(cache_file)$mtime, units = "days")
  return(cache_age < days)
}

# Query EPA SLD service ----
query_epa_sld <- function(force_refresh = FALSE) {
  # Check cache first
  if (!force_refresh && cache_is_fresh(CACHE_FILE)) {
    message("Using cached EPA SLD data (< ", CACHE_DAYS, " days old)")
    return(readRDS(CACHE_FILE))
  }

  message("Querying EPA Smart Location Database...")

  # Connect to EPA service
  tryCatch(
    {
      sld_layer <- arc_open(EPA_SLD_URL)
      query_fields <- c(
        "GEOID10", "GEOID20", "STATEFP", "COUNTYFP", "CSA", "CSA_Name",
        "CBSA", "CBSA_Name", "AC_LAND", "AC_TOTAL", D3_FIELDS
      )

      # Query Minnesota block groups
      mn_sld <- arc_select(
        sld_layer,
        where = paste0("STATEFP = '", MN_STATEFP, "'"),
        fields = query_fields
      )

      # Transform to Minnesota CRS
      mn_sld <- st_transform(mn_sld, crs = MN_CRS)

      field_descriptions <- sld_layer$fields[sld_layer$fields$name %in% query_fields, c("name", "alias")]


      # Save metadata
      metadata <- list(
        service_url = EPA_SLD_URL,
        layer_name = sld_layer$name,
        query_date = as.character(Sys.time()),
        statefp = MN_STATEFP,
        n_records = nrow(mn_sld),
        crs = MN_CRS,
        fields = field_descriptions,
        cache_file = CACHE_FILE
      )

      write_json(metadata, METADATA_FILE, pretty = TRUE, auto_unbox = TRUE)
      saveRDS(mn_sld, CACHE_FILE)

      message("Retrieved ", nrow(mn_sld), " block groups")

      return(mn_sld)
    },
    error = function(e) {
      warning("Failed to query EPA SLD service: ", e$message)

      # Try to fall back to cache
      if (file.exists(CACHE_FILE)) {
        warning("Falling back to cached data (may be stale)")
        return(readRDS(CACHE_FILE))
      } else {
        stop("No cached data available. Cannot proceed.")
      }
    }
  )
}

# Main execution ----
mn_sld_data <- query_epa_sld(force_refresh = TRUE)
