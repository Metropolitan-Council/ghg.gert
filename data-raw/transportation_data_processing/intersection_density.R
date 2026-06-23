## intersection density
## Updated 2026-06-23 to use EPA Smart Location Database (SLD)
## Data source: EPA ArcGIS REST service
## Methodology: Area-weighted aggregation from Census Block Groups to CTUs

library(dplyr)
library(tidyr)
library(ghg.ccap)

# Source EPA SLD data processing scripts
# These will use cached data if available (< 30 days old)
source("data-raw/transportation_data_processing/epa_sld_download.R")
source("data-raw/transportation_data_processing/epa_sld_to_ctu.R")

# Load CTU-level intersection density data (all D3 metrics)
ctu_intersections <- readRDS("data-raw/transportation_data_processing/processed_data/ctu_intersection_density_all_metrics.RDS")

# For backward compatibility with existing code that expects specific format
# Create summary metrics matching old structure if needed
inters <- ctu_intersections %>%
  select(
    ctu_name = geog_name,
    D3A, D3AAO, D3AMM, D3APO,
    D3B, D3BAO, D3BMM3, D3BMM4,
    D3BPO3, D3BPO4,
    total_area_sqmi
  ) %>%
  st_drop_geometry()

# Optional: Create categorical version for compatibility with old code
# If old code expected counts by intersection type:
# inters_wide <- inters %>%
#   mutate(
#     `3-way` = (D3BMM3 + D3BPO3) / 2 * total_area_sqmi,  # Convert density to counts
#     `4-way` = (D3BMM4 + D3BPO4) / 2 * total_area_sqmi,
#     total_inter = `3-way` + `4-way`
#   )
