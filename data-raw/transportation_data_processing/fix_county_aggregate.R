pkgload::load_all()

# County VMT ----
# fetch from CPRG repository and sum up at county level, similar to what was done in fix_update_pmt.R
coctu_vmt_forecast <- readRDS(url("https://github.com/Metropolitan-Council/ghg-cprg/raw/refs/heads/main/_transportation/data/mndot_vmt_ctu_gap_filled.RDS"))


# County AVO -----
source("data-raw/transportation_data_processing/_tbi_load.R")

# PLDV avo by county
avo_county <- trip %>%
  filter(
    hh_id %in% hh_region$hh_id,
    mode_type %in% c(
      "Household Vehicle",
      "Other Vehicle",
      "For-Hire Vehicle"
    ),
    # origin and destination in MPO area
    trip_o_in_mpo == TRUE,
    trip_d_in_mpo == TRUE,
    # ensure observed trip duration,
    # reasonable distance
    # origin or destination in our counties
    duration_minutes > 0,
    as.character(trip_o_county) %in% cprg_tbi_hh_counties,
    as.character(trip_d_county) %in% cprg_tbi_hh_counties,
    distance_miles < 720,
    distance_miles > 0
  ) %>%
  left_join(hh_region, join_by(survey_year, hh_id)) %>%
  filter(
    linked_trip_weight > 0,
    !is.na(cd_2050)
  ) %>%
  srvyr::as_survey_design(id = linked_trip_id, weights = linked_trip_weight) %>%
  group_by(hh_county) %>%
  summarize(
    num_travelers_numeric = round(srvyr::survey_mean(num_hh_travelers_int, na.rm = T), digits = 2),
    n_trips = srvyr::survey_total(),
    n_trips_sample = n()
  ) %>%
  ungroup() %>%
  mutate(geog_name = stringr::str_replace(hh_county, " MN", " County")) %>%
  left_join(geog_index, by = c("geog_name" = "geog_name")) %>%
  mutate(
    var = "AVO",
    mode = "PLDV",
    aeo_mode = "LDV",
    type = "P",
    value = num_travelers_numeric
  ) %>%
  select(geog_id, geog_name, var, mode, value, aeo_mode, type)

# For all other modes, use the same AVO for each mode
co_non_pldv <- vehicle_occupancy %>%
  filter(!mode %in% c("PLDV", "BU")) %>%
  select(-geog_id, -geog_name) %>%
  unique() %>%
  cross_join(
    avo_county %>%
      select(geog_id, geog_name) %>%
      unique()
  )

# for bus AVO, use the lowest AVO from the Transit Market Area analysis for all counties
co_bu_avo <- vehicle_occupancy %>%
  filter(
    mode == "BU"
  ) %>%
  select(mode, var, value, aeo_mode, type) %>%
  unique() %>%
  filter(value == min(value)) %>%
  cross_join(
    avo_county %>%
      select(geog_id, geog_name) %>%
      unique()
  )

vehicle_occupancy <- bind_rows(
  vehicle_occupancy,
  avo_county,
  co_non_pldv,
  co_bu_avo
) %>%
  unique()

# County Vehicle counts ----
# match CTUs with their respective counties
# for ctus with more than one county, allocate VMT based on the proportion of each CTU's VMT in each county for each year
# complete for all vehicle fuel types, plus Total, Exist, and Sales variations for each fuel type
# Tot should be the sum of each fuel type for each year

# Load CTU data and prepare VMT proportions
cprg_ctu <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/refs/heads/main/_meta/data/cprg_ctu.RDS") %>%
  sf::st_drop_geometry() %>%
  filter(county_name %in% c(
    "Anoka",
    "Carver",
    "Dakota",
    "Hennepin",
    "Ramsey",
    "Scott",
    "Washington"
  )) %>%
  select(-statefp, -state_abb, -geoid_wis, -cprg_area)

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
  filter(geog_level != "COUNTY") %>%
  filter(
    str_detect(var, "Tot", negate = TRUE),
    str_detect(var, "Stock|Sales|Exist")
  ) %>%
  mutate(inventory_year = as.numeric(year))

# Allocate CTU vehicle counts to counties using VMT proportions
county_vehicle_counts <- ctu_vehicle_counts %>%
  left_join(
    city_vmt_proportions,
    by = c("geog_id" = "gnis", "geog_level", "geog_name", "inventory_year")
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


# Allocate CTU-level PMT to counties using VMT proportions
ctu_pmt <- transportation_data$passenger %>%
  filter(
    var == "PMT",
    mode %in% c("PLDV", "WALK", "BIKE", "BU", "BRT", "RU", "RI", "BS", "AT")
  ) %>%
  mutate(inventory_year = as.numeric(year))

county_pmt <- ctu_pmt %>%
  left_join(
    city_vmt_proportions,
    by = c("geog_id" = "gnis", "inventory_year")
  ) %>%
  filter(!is.na(pct_vmt_in_county)) %>%
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
  # mutate(year = as.character(year)) %>%
  select(geog_id, geog_name, mode, var, year, value, aeo_mode, type)


# Combine all county-level data ----
county_passenger_data <- bind_rows(
  # county_vmt,
  county_pmt,
  county_vehicle_counts_full
) %>%
  arrange(geog_id, mode, var, year) %>%
  mutate(year = as.character(year))

# Freight data ----
# Allocate CTU-level freight TMT and Stock to counties using same VMT proportions

# Get CTU-level freight data (TMT and Stock vars)
ctu_freight_data <- transportation_data$freight %>%
  filter(var %in% c("TMT") | str_detect(var, "Stock")) %>%
  mutate(inventory_year = as.numeric(year))

# Allocate CTU freight data to counties using VMT proportions
county_freight_data <- ctu_freight_data %>%
  left_join(
    city_vmt_proportions,
    by = c("geog_id" = "gnis", "inventory_year")
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
    by = c("geoid" = "geog_id")
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


# County Parking Costs ----
# Set parking costs for all counties to minimum observed parking price
min_parking_cost <- parking_cost %>%
  filter(var == "PARK") %>%
  pull(value) %>%
  min(na.rm = TRUE)

# Create county parking for PLDV
county_parking_pldv <- geog_index %>%
  filter(geog_level == "COUNTY") %>%
  select(geog_id, geog_name) %>%
  mutate(
    mode = "PLDV",
    var = "PARK",
    value = min_parking_cost,
    aeo_mode = "LDV",
    type = "P"
  )

# Create county parking for SUT
county_parking_sut <- geog_index %>%
  filter(geog_level == "COUNTY") %>%
  select(geog_id, geog_name) %>%
  mutate(
    mode = "SUT",
    var = "PARK",
    value = min_parking_cost,
    aeo_mode = "MDT",
    type = "F"
  )

# Create county parking for CUT
county_parking_cut <- geog_index %>%
  filter(geog_level == "COUNTY") %>%
  select(geog_id, geog_name) %>%
  mutate(
    mode = "CUT",
    var = "PARK",
    value = min_parking_cost,
    aeo_mode = "HDT",
    type = "F"
  )

# Add county parking to parking_cost dataset
parking_cost <- bind_rows(
  parking_cost,
  county_parking_pldv,
  county_parking_sut,
  county_parking_cut
) %>%
  unique()

# Save updated datasets
usethis::use_data(transportation_data, overwrite = TRUE)
usethis::use_data(vehicle_occupancy, overwrite = TRUE)
usethis::use_data(parking_cost, overwrite = TRUE)



transportation_data$freight %>%
  filter(
    geog_name == "Hennepin County",
    mode == "SUT", var == "BEVStock"
  )
