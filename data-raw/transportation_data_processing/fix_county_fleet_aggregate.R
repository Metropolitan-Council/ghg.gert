pkgload::load_all()
source("data-raw/meta/compile_townships.R")

# County VMT ----
# fetch from CPRG repository and sum up at county level, similar to what was done in fix_update_pmt.R
coctu_vmt_forecast <- readRDS(url("https://github.com/Metropolitan-Council/ghg-cprg/raw/refs/heads/main/_transportation/data/mndot_vmt_ctu_gap_filled.RDS"))


# County Vehicle counts ----
# match CTUs with their respective counties
# for ctus with more than one county, allocate VMT based on the proportion of each CTU's VMT in each county for each year
# complete for all vehicle fuel types, plus Total, Exist, and Sales variations for each fuel type
# Tot should be the sum of each fuel type for each year

# Calculate total VMT per CTU across all counties
coctu_vmt <- coctu_vmt_forecast %>%
  group_by(gnis, inventory_year) %>%
  summarize(
    total_coctu_vmt = round(sum(final_city_vmt, na.rm = TRUE)),
    .groups = "drop"
  )


# Calculate proportion of each city's VMT in each county for each year
city_vmt_proportions <- coctu_vmt_forecast %>%
  left_join(coctu_vmt, by = c("gnis", "inventory_year")) %>%
  mutate(
    pct_vmt_in_county = round(final_city_vmt / total_coctu_vmt, digits = 2),
    pct_vmt_in_county = if_else(is.na(pct_vmt_in_county), 0, pct_vmt_in_county)
  ) %>%
  select(gnis, geoid, inventory_year, pct_vmt_in_county) %>%
  left_join(geog_index %>% select(geog_id, geog_name, geog_level), by = c("gnis" = "geog_id"))

# Get CTU-level vehicle counts from transportation_data
ctu_vehicle_counts <- transportation_data$passenger %>%
  left_join(geog_index %>% select(geog_id, geog_name, geog_level), by = c("geog_id" = "geog_id", "geog_name")) %>%
  filter(geog_level != "COUNTY", geog_level != "TOWNSHIP_GROUP") %>%
  filter(
    str_detect(var, "Tot", negate = TRUE),
    str_detect(var, "Stock|Sales|Exist")
  ) %>%
  mutate(inventory_year = as.numeric(year))

# Allocate CTU vehicle counts to counties using VMT proportions
county_vehicle_counts <- ctu_vehicle_counts %>%
  left_join(
    city_vmt_proportions,
    by = c("geog_id" = "gnis", "geog_level", "geog_name", "inventory_year"),
    relationship = "many-to-many"
  ) %>%
  # filter out Twin Cities Region
  filter(!is.na(pct_vmt_in_county)) %>%
  unique() %>%
  mutate(allocated_value = value * pct_vmt_in_county) %>%
  group_by(geoid, inventory_year, mode, var, aeo_mode, type) %>%
  summarize(
    value = sum(allocated_value, na.rm = TRUE) %>% round(digits = 2),
    .groups = "drop"
  ) %>%
  left_join(
    geog_index %>% select(geog_id, geog_name),
    by = c("geoid" = "geog_id")
  ) %>%
  rename(
    geog_id = geoid,
    year = inventory_year
  ) %>%
  select(geog_id, geog_name, mode, var, year, value, aeo_mode, type)

# Calculate "Tot" versions as sum of fuel types by county, year, and vehicle type (Stock/Sales/Exist)
# First, extract the vehicle type (Stock, Sales, or Exist) from the var name
county_vehicle_totals <- county_vehicle_counts %>%
  mutate(
    vehicle_type = case_when(
      str_detect(var, "Stock") ~ "Stock",
      str_detect(var, "Sales") ~ "Sales",
      str_detect(var, "Exist") ~ "Exist",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(vehicle_type)) %>%
  group_by(geog_id, geog_name, mode, year, vehicle_type, type, aeo_mode) %>%
  summarize(
    value = sum(value, na.rm = TRUE) %>% round(digits = 0),
    .groups = "drop"
  ) %>%
  mutate(
    var = paste0("Tot", vehicle_type)
  ) %>%
  select(geog_id, geog_name, mode, var, year, value, aeo_mode, type)

# Combine fuel-specific counts with totals
county_vehicle_counts_full <- bind_rows(
  county_vehicle_counts,
  county_vehicle_totals
)


# County VMT and PMT ----
# Aggregate CTU-level VMT to county level
# county_vmt <- coctu_vmt_forecast %>%
#   filter(inventory_year %in% transportation_data$passenger$year) %>%
#   group_by(geoid, inventory_year) %>%
#   summarize(
#     final_county_vmt = sum(final_city_vmt, na.rm = TRUE) %>% round(digits = 0),
#     .groups = "drop"
#   ) %>%
#   left_join(
#     geog_index %>% select(geog_id, geog_name),
#     by = c("geoid" = "geog_id")
#   ) %>%
#   rename(
#     geog_id = geoid,
#     year = inventory_year
#   ) %>%
#   mutate(
#     mode = "PLDV",
#     var = "VMT",
#     aeo_mode = "LDV",
#     type = "P",
#     value = final_county_vmt
#   ) %>%
#   select(geog_id, geog_name, mode, var, year, value, aeo_mode, type)


# # Allocate CTU-level PMT to counties using VMT proportions
# ctu_pmt <- transportation_data$passenger %>%
#   filter(
#     var == "PMT",
#     mode %in% c("PLDV", "WALK", "BIKE", "BU", "BRT", "RU", "RI", "BS", "AT")
#   ) %>%
#   mutate(inventory_year = as.numeric(year))

# county_pmt <- ctu_pmt %>%
#   left_join(
#     city_vmt_proportions,
#     by = c("geog_id" = "gnis", "inventory_year"),
#     relationship = "many-to-many"
#   ) %>%
#   filter(!is.na(pct_vmt_in_county)) %>%
#   mutate(allocated_value = value * pct_vmt_in_county) %>%
#   group_by(geoid, inventory_year, mode, var, aeo_mode, type) %>%
#   summarize(
#     value = sum(allocated_value, na.rm = TRUE) %>% round(digits = 2),
#     .groups = "drop"
#   ) %>%
#   left_join(
#     geog_index %>% select(geog_id, geog_name),
#     by = c("geoid" = "geog_id"),
#     relationship = "many-to-many"
#   ) %>%
#   rename(
#     geog_id = geoid,
#     year = inventory_year
#   ) %>%
#   # mutate(year = as.character(year)) %>%
#   select(geog_id, geog_name, mode, var, year, value, aeo_mode, type)


# Combine all county-level data ----
county_passenger_data <- bind_rows(
  # county_vmt,
  # county_pmt,
  county_vehicle_counts_full
) %>%
  arrange(geog_id, mode, var, year) %>%
  mutate(year = as.character(year))

# Freight data ----
# Allocate CTU-level freight TMT and Stock to counties using same VMT proportions

# Get CTU-level freight data (TMT and Stock vars)
ctu_freight_data <- transportation_data$freight %>%
  filter(var %in% c("TMT") | str_detect(var, "Stock")) %>%
  left_join(geog_index %>% select(geog_id, geog_name, geog_level), by = c("geog_id" = "geog_id", "geog_name")) %>%
  filter(geog_level != "COUNTY", geog_level != "TOWNSHIP_GROUP") %>%
  mutate(inventory_year = as.numeric(year))

# Allocate CTU freight data to counties using VMT proportions
county_freight_data <- ctu_freight_data %>%
  left_join(
    city_vmt_proportions,
    by = c("geog_id" = "gnis", "inventory_year"),
    relationship = "many-to-many"
  ) %>%
  filter(!is.na(pct_vmt_in_county)) %>%
  unique() %>%
  mutate(allocated_value = value * pct_vmt_in_county) %>%
  group_by(geoid, inventory_year, mode, var, aeo_mode, type) %>%
  summarize(
    value = sum(allocated_value, na.rm = TRUE) %>% round(digits = 2),
    .groups = "drop"
  ) %>%
  left_join(
    geog_index %>% select(geog_id, geog_name),
    by = c("geoid" = "geog_id"),
    relationship = "many-to-many"
  ) %>%
  rename(
    geog_id = geoid,
    year = inventory_year
  ) %>%
  mutate(year = as.character(year)) %>%
  select(geog_id, geog_name, mode, var, year, value, aeo_mode, type)


# Update transportation_data ----
# Add county data to existing transportation_data

# Passenger data
transportation_data$passenger <- bind_rows(
  transportation_data$passenger %>%
    filter(!geog_id %in% unique(county_passenger_data$geog_id)),
  county_passenger_data
) %>%
  arrange(geog_id, mode, var, year) %>%
  unique()

# Freight data
transportation_data$freight <- bind_rows(
  transportation_data$freight %>%
    filter(!geog_id %in% unique(county_freight_data$geog_id)),
  county_freight_data
) %>%
  arrange(geog_id, mode, var, year) %>%
  unique()

# Township groups: AVO and parking ----
# Matched on GNIS id. (transportation_data group rows are added in
# fix_region_aggregate.R, the last script to write it.)
twp_lookup <- twp_group_lookup() %>% select(geog_id, group_name, group_id)

# Save updated datasets
usethis::use_data(transportation_data, overwrite = TRUE)
