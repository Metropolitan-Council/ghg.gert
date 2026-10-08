# Counties -----
# our study area includes the 7-county metro

# fetch MN counties
ccap_county <- tigris::counties(state = "MN") %>%
  filter(NAME %in% c(
    "Anoka",
    "Carver",
    "Dakota",
    "Hennepin",
    "Ramsey",
    "Scott",
    "Washington"
  )) %>%
  mutate(
    STATE_ABB = "MN",
    geog_level = "COUNTY"
  ) %>%
  janitor::clean_names() %>%
  # Combine to get cprg_counties
  # Get state names from FIPS codes
  left_join(
    tigris::fips_codes %>%
      select(state_code, state_name) %>%
      unique(),
    by = c("statefp" = "state_code")
  ) %>%
  select(
    county_id = geoid,
    geog_name = name,
    geog_level,
    county_name_full = namelsad,
    state_name, statefp,
    state_abb,
    geometry
  )


# ccap_county_meta <- tribble(
#   ~Column, ~Class, ~Description,
#   "county_id", class(cprg_county$geoid), "Five digit county GEOID",
#   "county_name", class(cprg_county$county_name), "County name",
#   "county_name_full", class(cprg_county$county_name_full), "Full county name",
#   "state_name", class(cprg_county$state_name), "Full state name",
#   "statefp", class(cprg_county$statefp), "State FIPS code",
#   "state_abb", class(cprg_county$state_abb), "Abbreviated state name",
#   "geometry", class(cprg_county$geometry)[1], "Simple feature geometry"
# )

# Cities ------

source("data-raw/demographic/thrive_designation.R")
thrive_des <- mutate(thrive,
  ctu_class = if_else(grepl("Twp.", ctu),
    "TOWNSHIP",
    "CITY"
  ),
  geog_name = str_replace_all(ctu_name, " Township", ""),
  geog_name = str_replace_all(geog_name, "St\\.", "Saint")
) %>%
  ungroup() %>%
  distinct(geog_name, ctu_class, com_des)

# fetch cities from MN Geospatial Commons
ccap_ctu <- councilR::import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/bdry_mn_city_township_unorg/gpkg_bdry_mn_city_township_unorg.zip") %>%
  filter(COUNTY_NAME %in% c(ccap_county$geog_name)) %>%
  mutate(
    STATEFP = "27",
    STATE = "Minnesota",
    STATE_ABB = "MN",
    geog_level = "ctu",
    ctu_id_gnis = stringr::str_pad(GNIS_FEATURE_ID, width = 8, pad = "0", side = "left")
  ) %>%
  select(
    geog_name = FEATURE_NAME,
    CTU_CLASS,
    COUNTY_NAME,
    STATEFP,
    STATE,
    STATE_ABB,
    ctu_id_gnis,
    geometry = SHAPE
  ) %>%
  dplyr::arrange(geog_name) %>%
  janitor::clean_names() %>%
  left_join(thrive_des,
    by = c("geog_name", "ctu_class")
  ) %>%
  mutate(thrive_designation = if_else(
    is.na(com_des),
    "Unorganized territory",
    com_des
  )) %>%
  select(-com_des)


# compile RDS
saveRDS(ccap_county, "data-raw/meta/ccap_county.RDS")
saveRDS(ccap_ctu, "data-raw/meta/ccap_ctu.RDS")


### create city_county sheet

ctu_county_area <- ccap_ctu %>%
  st_transform(5070) %>%
  mutate(piece_area = as.numeric(st_area(geometry))) %>%
  group_by(ctu_id_gnis) %>%
  mutate(
    total_ctu_area = sum(piece_area),
    pct_of_ctu = piece_area / total_ctu_area
  ) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  mutate(geog_name = if_else(ctu_class == "TOWNSHIP",
    paste(geog_name, "Twp."),
    geog_name
  )) %>%
  select(geog_name, county_name, geog_id = ctu_id_gnis, pct_of_ctu_area = pct_of_ctu)

# waldo::compare(ctu_county_area, ghg.ccap::ctu_county_area)
usethis::use_data(ctu_county_area, overwrite = T)
