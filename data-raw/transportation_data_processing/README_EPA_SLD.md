# EPA Smart Location Database Integration

This directory contains scripts for automatically retrieving and processing EPA Smart Location Database (SLD) intersection density data.

## Overview

The EPA SLD provides street network and intersection density metrics at the Census Block Group level. These scripts aggregate this data to the CTU (City/Township/Unorganized territory) level for use in the GHG transportation model.

## Workflow

### 1. Data Retrieval (`epa_sld_download.R`)
- Queries EPA ArcGIS REST service: https://geodata.epa.gov/arcgis/rest/services/OA/SmartLocationDatabase/MapServer/10
- Filters to Minnesota block groups (STATEFP = '27')
- Retrieves all D3 intersection density metrics
- Caches data locally (30-day default cache period)
- Saves metadata about the query

**Output**: 
- `processed_data/mn_sld_blockgroups.RDS` - Minnesota block group data (cached, gitignored)
- `processed_data/epa_sld_metadata.json` - Query metadata

### 2. Spatial Aggregation (`epa_sld_to_ctu.R`)
- Loads EPA SLD block groups and CTU boundaries
- Performs spatial intersection
- Calculates area-weighted averages for all D3 metrics
- Saves data in both wide and long formats

**Output**:
- `processed_data/ctu_intersection_density_all_metrics.RDS` - Wide format (all metrics as columns)
- `processed_data/ctu_intersection_density_long.RDS` - Long/tidy format
- `csv_copies/ctu_intersection_density_all_metrics.csv` - CSV backup (wide)
- `csv_copies/ctu_intersection_density_long.csv` - CSV backup (long)

### 3. Integration (`intersection_density.R`)
- Sources the EPA SLD processing scripts
- Loads CTU-level intersection density data
- Makes data available to downstream transportation models

## D3 Metrics

All EPA SLD D3 intersection density metrics are retrieved and aggregated:

| Metric | Description | Unit |
|--------|-------------|------|
| D3A | Total road network density | intersections/sq mi |
| D3AAO | Auto-oriented intersection density | intersections/sq mi |
| D3AMM | Multi-modal link density | intersections/sq mi |
| D3APO | Pedestrian-oriented link density | intersections/sq mi |
| D3B | Street intersection density (weighted, auto-oriented eliminated) | intersections/sq mi |
| D3BAO | Auto-oriented intersections per sq mi | intersections/sq mi |
| D3BMM3 | Multi-modal 3-way intersections | intersections/sq mi |
| D3BMM4 | Multi-modal 4+ way intersections | intersections/sq mi |
| D3BPO3 | Pedestrian-oriented 3-way intersections | intersections/sq mi |
| D3BPO4 | Pedestrian-oriented 4+ way intersections | intersections/sq mi |

## Usage

### Automatic (via 00_run_all.R)
```r
source("data-raw/transportation_data_processing/00_run_all.R")
```

### Manual
```r
# Download EPA SLD data (uses cache if recent)
source("data-raw/transportation_data_processing/epa_sld_download.R")
mn_sld <- query_epa_sld()

# Force refresh (ignore cache)
mn_sld <- query_epa_sld(force_refresh = TRUE)

# Aggregate to CTUs
source("data-raw/transportation_data_processing/epa_sld_to_ctu.R")
ctu_data <- aggregate_sld_to_ctu()

# Access the data
ctu_wide <- readRDS("data-raw/transportation_data_processing/processed_data/ctu_intersection_density_all_metrics.RDS")
```

## Cache Management

- **Default cache period**: 30 days
- **Cache files** (gitignored):
  - `processed_data/mn_sld_blockgroups.RDS`
  - `processed_data/epa_sld_metadata.json`
- **Output files** (committed to repo):
  - `processed_data/ctu_intersection_density_*.RDS`
  - `csv_copies/ctu_intersection_density_*.csv`

To refresh cached data before 30 days:
```r
query_epa_sld(force_refresh = TRUE)
```

## Data Provenance

- **Source**: EPA Smart Location Database
- **Service**: ArcGIS REST API
- **Layer**: MapServer/10 (Census Block Groups)
- **Version**: Tracked in `epa_sld_metadata.json`
- **Update frequency**: EPA updates SLD every 2-3 years (decennial census cycle)
- **Aggregation method**: Area-weighted spatial averaging from block groups to CTUs

## Dependencies

R packages:
- `arcgislayers` - Query ArcGIS REST services
- `sf` - Spatial operations
- `dplyr`, `tidyr` - Data manipulation
- `jsonlite` - Metadata storage

## Migration Notes

**Previous approach** (pre-2026-06-23):
- Used static CSV file: `intersect_cts.csv`
- Manual data updates required

**Current approach**:
- Automated EPA SLD queries via ArcGIS REST API
- Reproducible area-weighted aggregation
- Version tracking and metadata
- 30-day cache for efficiency

## Troubleshooting

### "Failed to query EPA SLD service"
- Check internet connection
- EPA service may be temporarily unavailable
- Script will fall back to cached data if available

### "No cached data available"
- First-time setup requires internet connection
- Run `query_epa_sld()` to download initial data

### Stale data warning
- Cache is older than 30 days
- Script will still use cached data unless `force_refresh = TRUE`
- EPA SLD is only updated every 2-3 years, so staleness is usually not a concern

## Contact

Questions about this workflow? Contact the CCAP team or see the main repository documentation.
