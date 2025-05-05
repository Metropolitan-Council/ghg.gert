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
  mutate(STATE_ABB = "MN") %>%
  janitor::clean_names() %>%
# Combine to get cprg_counties
# Get state names from FIPS codes
  left_join(
    tigris::fips_codes %>%
      select(state_code, state_name) %>%
      unique(),
    by = c("statefp" = "state_code")
  ) %>%
  mutate(county_id_fips = substr(geoid, 3, 5)) %>%
  select(
    county_id = geoid,
    county_id_fips,
    county_name = name,
    county_name_full = namelsad,
    state_name, statefp,
    state_abb = STATE_ABB,
    geometry
  )



ccap_county_meta <- tribble(
  ~Column, ~Class, ~Description,
  "county_id", class(cprg_county$geoid), "Five digit county GEOID",
  "county_name", class(cprg_county$county_name), "County name",
  "county_name_full", class(cprg_county$county_name_full), "Full county name",
  "state_name", class(cprg_county$state_name), "Full state name",
  "statefp", class(cprg_county$statefp), "State FIPS code",
  "state_abb", class(cprg_county$state_abb), "Abbreviated state name",
  "geometry", class(cprg_county$geometry)[1], "Simple feature geometry"
)

# Cities ------

source("data-raw/demographic/thrive_designation.R")
thrive_des <- mutate(thrive,
                     ctu_class = if_else(grepl("Twp.",ctu),
                                         "TOWNSHIP",
                                         "CITY"),
                     ctu_name = str_replace_all(ctu_name, " Township", ""),
                     ctu_name = str_replace_all(ctu_name, "St\\.", "Saint") ) %>%
  ungroup() %>%
  distinct(ctu_name, ctu_class, com_des)

# fetch cities from MN Geospatial Commons
ccap_ctu <- councilR::import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/bdry_mn_city_township_unorg/gpkg_bdry_mn_city_township_unorg.zip") %>%
  filter(COUNTY_NAME %in% c(ccap_county$county_name)) %>%
  mutate(
    STATEFP = "27",
    STATE = "Minnesota",
    STATE_ABB = "MN"
  ) %>%
  select(
    CTU_NAME = FEATURE_NAME,
    CTU_CLASS,
    COUNTY_NAME,
    STATEFP,
    STATE,
    STATE_ABB,
    ctu_id_gnis = GNIS_FEATURE_ID,
    geometry = SHAPE
  ) %>%
  arrange(CTU_NAME) %>%
  janitor::clean_names() %>%
  left_join(thrive_des,
            by = c("ctu_name","ctu_class")) %>%
  mutate(thrive_designation = if_else(
    is.na(com_des),
    "Unorganized territory",
    com_des
  )) %>%
  select(-com_des) %>%
  mutate(ctu_id_gnis = stringr::str_pad(ctu_id_gnis, width = 8, pad = "0", side = "left"))

# compile RDS
saveRDS(ccap_county, "data-raw/meta/ccap_county.RDS")
saveRDS(ccap_ctu, "data-raw/meta/ccap_ctu.RDS")
