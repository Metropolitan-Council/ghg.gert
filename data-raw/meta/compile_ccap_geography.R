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
    geog_level = "county"
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
  janitor::clean_names()



# compile RDS
saveRDS(ccap_county, "data-raw/meta/ccap_county.RDS")
saveRDS(ccap_ctu, "data-raw/meta/ccap_ctu.RDS")
