# create index of geographies ----
#
# Outputs
#   geog_index    (package data)  one row per geography, attributes only
#   geog_geometry (data-raw/meta/geog_geometry.RDS)  sf, one row per
#                 geog_index row: CTUs, counties, region, township groups
#
devtools::load_all(".")
source("data-raw/meta/compile_townships.R")

metro_counties <- c(
  "Anoka", "Carver", "Dakota", "Hennepin",
  "Ramsey", "Scott", "Washington"
)

# CTU geographies ----
# kept as sf so the same source feeds both geog_index and geog_geometry
cprg_ctu_raw <- readr::read_rds(
  "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_meta/data/cprg_ctu.RDS"
) %>%
  filter(county_name %in% metro_counties) %>%
  mutate(
    geog_name = case_when(
      ctu_class == "TOWNSHIP" ~ paste(ctu_name, "Twp."),
      TRUE ~ ctu_name
    )
  )
sf::st_geometry(cprg_ctu_raw) <- "geometry"

cprg_ctu <- cprg_ctu_raw %>%
  sf::st_drop_geometry() %>%
  select(
    geog_name,
    geog_short_name = ctu_name,
    geog_level = ctu_class,
    geog_id = gnis,
    imagine_designation
  ) %>%
  mutate(geog_id_type = "ctu_gnis") %>%
  distinct()

# County geographies ----
cprg_county_raw <- readr::read_rds(
  "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_meta/data/cprg_county.RDS"
) %>%
  filter(county_name %in% metro_counties)
sf::st_geometry(cprg_county_raw) <- "geometry"

cprg_county <- cprg_county_raw %>%
  sf::st_drop_geometry() %>%
  select(
    geog_name = county_name_full,
    geog_short_name = county_name,
    geog_id = geoid
  ) %>%
  mutate(
    geog_id_type = "county_fips",
    geog_level = "COUNTY",
    imagine_designation = "County"
  )

# Regional geography ----
cprg_region <- tibble(
  geog_name = "Twin Cities Region",
  geog_short_name = "Twin Cities Region",
  geog_id = "00000000",
  geog_id_type = "region",
  geog_level = "REGION",
  imagine_designation = "Region"
)

geog_index <- bind_rows(as_tibble(cprg_county), cprg_ctu, cprg_region) %>%
  filter(!imagine_designation == "Non-Council Community")

# Township group geographies ----
# Definitions live in data-raw/meta/compile_townships.R. The lookup call validates that
# the member townships in this source match twp_group_meta() (names, counts,
# one county each) before the group rows are added.
twp_lookup <- twp_group_lookup(
  cprg_ctu_raw %>%
    sf::st_drop_geometry() %>%
    filter(geog_name %in% geog_index$geog_name) %>%
    select(geog_name, geog_id = gnis, county_name)
)

geog_index <- twp_group_replace(
  geog_index,
  twp_group_meta() %>%
    transmute(
      geog_name,
      geog_short_name = geog_name,
      geog_id,
      geog_id_type = TWP_GROUP_ID_TYPE,
      geog_level = TWP_GROUP_LEVEL,
      imagine_designation
    )
)

# Geometry ----
# One (multi)polygon per geog_id. CTUs that appear as several pieces in the
# source (e.g. split by county) are unioned back to a single feature.
# Counties, region and township groups are unioned from their parts, so
# all levels share the CTU source's CRS and edges.
geo_crs <- sf::st_crs(cprg_ctu_raw)

ctu_geom <- cprg_ctu_raw %>%
  select(geog_id = gnis, geometry) %>%
  sf::st_make_valid() %>%
  group_by(geog_id) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop")

county_geom <- cprg_county_raw %>%
  select(geog_id = geoid, geometry) %>%
  sf::st_transform(geo_crs) %>%
  sf::st_make_valid() %>%
  group_by(geog_id) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop")

region_geom <- county_geom %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  mutate(geog_id = cprg_region$geog_id)

twp_group_geom <- ctu_geom %>%
  inner_join(
    twp_lookup %>% distinct(geog_id, group_id),
    by = "geog_id"
  ) %>%
  group_by(geog_id = group_id) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop")

# rbind (sf method) rather than bind_rows, which doesn't reliably keep sf
geog_geometry <- list(ctu_geom, county_geom, region_geom, twp_group_geom) %>%
  lapply(function(x) select(x, geog_id, geometry)) %>%
  do.call(rbind, .) %>%
  inner_join(geog_index, by = "geog_id") %>%
  select(names(geog_index), geometry) %>%
  sf::st_as_sf() %>%
  sf::st_cast("MULTIPOLYGON")

# checks: exactly one geometry per geog_index row, none empty
stopifnot(
  nrow(geog_geometry) == nrow(geog_index),
  setequal(geog_geometry$geog_id, geog_index$geog_id),
  !anyDuplicated(geog_geometry$geog_id),
  !any(sf::st_is_empty(geog_geometry))
)
cli::cli_alert_success(
  "geog_geometry: {nrow(geog_geometry)} rows ({paste(names(table(geog_geometry$geog_level)), table(geog_geometry$geog_level), collapse = ', ')})"
)

# Save ----
usethis::use_data(geog_index, overwrite = TRUE)
saveRDS(geog_geometry, "data-raw/meta/geog_geometry.RDS")
