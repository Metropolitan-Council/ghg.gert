# Following exploratory analysis (data-raw/nature_data_processing/compare_dnr_nlcd_community_trees.R)
# this script takes what was learned there and creates two data objects for use in community trees function and app
#
# Creates two package data objects:
#   1. community_tree_density  — median sqm canopy per tree by thrive designation
#   2. community_tree_baseline — tree count per CTU (DNR actual or modeled estimate)

library(tidyverse)
library(readxl)

# Load data

## CommunityChangeTable is a document provided by DNR directly to MC. It is an aggregated form of
## the viewer accessible here: https://www.dnr.state.mn.us/forestry/urban/community-tree-canopy.html
## note the full data is not available for download
dnr_metro <- read_excel("./data-raw/nature_data_processing/CommunityChangeTable.xlsx") %>%
  rename(
    community = `Community *`,
    county = County,
    trees_2023 = `2023 Street Tree Population`
  ) %>%
  mutate(
    trees_2023 = as.numeric(trees_2023),
    community = str_trim(community)
  ) %>%
  filter(
    county %in% c(
      "Anoka", "Carver", "Dakota", "Hennepin",
      "Ramsey", "Scott", "Washington"
    ),
    !is.na(trees_2023)
  ) %>%
  select(community, county, trees_2023)

ctu_meta <- readRDS("./data-raw/meta/ccap_ctu.rds") %>%
  sf::st_drop_geometry() %>%
  select(geog_name, ctu_class, geog_id = ctu_id_gnis, thrive_designation)

plantable_fraction <- c(
  Developed_Low  = 0.30,
  Developed_Med  = 0.15,
  Developed_High = 0.05
)

nlcd_2022 <- bind_rows(ghg.gert::natural_systems_data$inventory) %>%
  filter(
    geog_id %in% ctu_meta$geog_id,
    inventory_year == 2022,
  ) %>%
  select(geog_name, ctu_class, geog_id, land_cover_type, area) %>%
  pivot_wider(
    id_cols = c(geog_name, ctu_class, geog_id),
    names_from = land_cover_type,
    values_from = area,
    values_fill = 0
  ) %>%
  mutate(
    urban_tree_sqkm = if ("Urban_Tree" %in% names(.)) Urban_Tree else 0,
    total_plantable_sqkm = Developed_Low * plantable_fraction["Developed_Low"] +
      Developed_Med * plantable_fraction["Developed_Med"] +
      Developed_High * plantable_fraction["Developed_High"]
  ) %>%
  select(geog_name, ctu_class, geog_id, urban_tree_sqkm, total_plantable_sqkm) %>%
  left_join(ctu_meta %>% distinct(geog_id, thrive_designation), by = "geog_id")


#  Match DNR to NLCD

matched <- dnr_metro %>%
  inner_join(nlcd_2022, by = c("community" = "geog_name")) %>%
  filter(urban_tree_sqkm > 0) %>%
  mutate(sqm_per_tree = (urban_tree_sqkm * 1e6) / trees_2023)

cat("Matched DNR communities:", nrow(matched), "\n\n")

# Check for DNR communities that didn't match (name mismatches to fix)
unmatched_dnr <- dnr_metro %>%
  anti_join(nlcd_2022, by = c("community" = "geog_name"))

if (nrow(unmatched_dnr) > 0) {
  unmatched_dnr
}
# all accounted for

# Community tree density: sqm/tree by thrive designation

community_tree_density <- matched %>%
  group_by(thrive_designation) %>%
  summarise(
    n_communities = n(),
    median_sqm_per_tree = median(sqm_per_tree),
    mean_sqm_per_tree = mean(sqm_per_tree),
    .groups = "drop"
  )

community_tree_density

# Regional fallback for any designation with too few observations
regional_median <- median(matched$sqm_per_tree)
cat("\nRegional median (fallback):", round(regional_median, 1), "sqm/tree\n")


# Community tree baseline: one row per CTU


community_tree_baseline <- nlcd_2022 %>%
  left_join(
    matched %>% select(geog_id, trees_dnr = trees_2023),
    by = "geog_id"
  ) %>%
  left_join(
    community_tree_density %>% select(thrive_designation, median_sqm_per_tree),
    by = "thrive_designation"
  ) %>%
  mutate(
    # Use designation-specific density; fall back to regional if designation missing
    sqm_per_tree = coalesce(median_sqm_per_tree, regional_median),
    # Modeled count: NLCD canopy area / sqm per tree
    # Round to nearest 1000; fall back to nearest 10 if that rounds to 0
    trees_modeled_raw = urban_tree_sqkm * 1e6 / sqm_per_tree,
    trees_modeled = if_else(round(trees_modeled_raw, -3) == 0 & trees_modeled_raw > 0,
      round(trees_modeled_raw, -1),
      round(trees_modeled_raw, -3)
    ),
    # Final baseline: DNR where available, modeled otherwise
    tree_count = coalesce(trees_dnr, trees_modeled),
    source = if_else(!is.na(trees_dnr), "DNR", "modeled"),
    # Max plantable trees using same density
    max_plantable_raw = total_plantable_sqkm * 1e6 / sqm_per_tree,
    max_plantable_trees = if_else(round(max_plantable_raw, -3) == 0 & max_plantable_raw > 0,
      round(max_plantable_raw, -1),
      round(max_plantable_raw, -3)
    )
  ) %>%
  select(
    geog_name, ctu_class, geog_id, thrive_designation,
    tree_count, source, max_plantable_trees,
    sqm_per_tree, urban_tree_sqkm, total_plantable_sqkm
  )


cat("\n── Baseline tree counts ──\n")
cat("  DNR actual:", sum(community_tree_baseline$source == "DNR"), "communities\n")
cat("  Modeled:   ", sum(community_tree_baseline$source == "modeled"), "communities\n")
cat("  Total:     ", nrow(community_tree_baseline), "communities\n")

# Spot check
cat("\n── Sample (DNR vs modeled side by side) ──\n")
community_tree_baseline %>%
  filter(geog_name %in% c(
    "Minneapolis", "Saint Paul", "Bloomington",
    "Edina", "Lakeville", "Shakopee",
    "Gem Lake", "Sunfish Lake", "Mendota",
    "Benton Twp.", "Medicine Lake",
    "Corcoran", "Afton",
    "New Trier", "Landfall"
  )) %>%
  arrange(desc(tree_count))

# roll up to county, combine, and output

county_tree_baseline <- community_tree_baseline %>%
  left_join(ctu_county_area %>% select(geog_id, county_name, pct_of_ctu_area),
    by = "geog_id"
  ) %>%
  mutate(
    tree_count           = tree_count * pct_of_ctu_area,
    max_plantable_trees  = max_plantable_trees * pct_of_ctu_area,
    urban_tree_sqkm      = urban_tree_sqkm * pct_of_ctu_area,
    total_plantable_sqkm = total_plantable_sqkm * pct_of_ctu_area
  ) %>%
  group_by(county_name) %>%
  summarise(
    tree_count = round(sum(tree_count), -3),
    max_plantable_trees = round(sum(max_plantable_trees), -3),
    urban_tree_sqkm = sum(urban_tree_sqkm),
    total_plantable_sqkm = sum(total_plantable_sqkm),
    .groups = "drop"
  ) %>%
  left_join(
    geog_index %>% filter(geog_level == "COUNTY") %>% select(geog_short_name, geog_id),
    by = c("county_name" = "geog_short_name")
  ) %>%
  mutate(
    geog_name          = paste(county_name, "County"),
    ctu_class          = "COUNTY",
    thrive_designation = county_name,
    source             = "modeled",
    sqm_per_tree       = regional_median
  ) %>%
  select(names(community_tree_baseline))

# Combine CTU and county into single dataset
community_tree_baseline <- bind_rows(community_tree_baseline, county_tree_baseline)

cat("\n── Combined baseline ──\n")
cat("  CTUs:    ", sum(community_tree_baseline$ctu_class != "COUNTY"), "\n")
cat("  Counties:", sum(community_tree_baseline$ctu_class == "COUNTY"), "\n")

cat("\n── County rows ──\n")
community_tree_baseline %>%
  filter(ctu_class == "COUNTY") %>%
  print(width = 120)

# Save as package data ──────────────────────────────────────────────────

usethis::use_data(community_tree_density, overwrite = TRUE)
usethis::use_data(community_tree_baseline, overwrite = TRUE)
