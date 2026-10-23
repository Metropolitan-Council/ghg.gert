# Geography geometry and CTU-county area shares ----
#
# Outputs
#   geog_geometry    sf, one (multi)polygon per geog_index row: CTUs,
#                    counties, region, township groups
#   ctu_county_area  share of each CTU's area in each county, plus the two
#                    township groups (used by county_assign())
#
# requires geog_index.R

pkgload::load_all()
source("data-raw/meta/compile_townships.R")

# retrieve geographies ----
county_geog <- councilR::fetch_county_geo()

# fetch_ctu_geo() looks for mn_fips_codes which is missing from councilR
mn_fips_codes <- tigris::fips_codes %>% dplyr::filter(state == "MN",
                                                      county_code %in% county_geog$COUNTYFP)
ctu_geog <- councilR::fetch_ctu_geo() %>%
  mutate(CTU_NAME = stringr::str_replace_all(CTU_NAME,
                                    "St\\.",
                                    "Saint"),
         CTU_NAME = stringr::str_replace_all(CTU_NAME,
                                             "Village of ",
                                             ""),
         CTU_NAME = stringr::str_replace_all(CTU_NAME,
                                             stringr::fixed(" (unorg.)"),
                                             ""))

# project both to UTM 15N so sf uses planar GEOS rather than spherical s2,
# which rejects some Census polygons (degenerate edges)
county_geog <- sf::st_transform(county_geog, 26915)
ctu_geog <- sf::st_transform(ctu_geog, 26915)

# CTU pieces ----
# Census county subdivisions are already split at county lines, so each
# feature is one CTU-county piece (no overlay needed, so no slivers).
# fetch_ctu_geo() drops county and GNIS: county is recovered by largest
# overlap, geog_id by name match to geog_index.
ctu_pieces <- ctu_geog %>%
  sf::st_make_valid() %>%
  sf::st_join(county_geog %>% select(county_name = NAME), largest = TRUE) %>%
  left_join(
    geog_index %>%
      filter(geog_id_type == "ctu_gnis") %>%
      select(geog_name, geog_id),
    by = c("CTU_NAME" = "geog_name")
  ) %>%
  rename(geog_name = CTU_NAME)

# unmatched names (expect unorganized territory and Non-Council communities;
# anything else is a naming mismatch with geog_index)
ctu_pieces %>%
  sf::st_drop_geometry() %>%
  filter(is.na(geog_id)) %>%
  distinct(geog_name, county_name) %>%
  print()

ctu_pieces <- filter(ctu_pieces, !is.na(geog_id))

### create city_county sheet ----

ctu_county_area <- ctu_pieces %>%
  sf::st_transform(5070) %>%
  mutate(piece_area = as.numeric(sf::st_area(geometry))) %>%
  sf::st_drop_geometry() %>%
  group_by(geog_name, county_name, geog_id) %>%
  summarise(piece_area = sum(piece_area), .groups = "drop") %>%
  group_by(geog_id) %>%
  mutate(pct_of_ctu_area = piece_area / sum(piece_area)) %>%
  ungroup() %>%
  select(geog_name, county_name, geog_id, pct_of_ctu_area)

# Township groups ----
# Definitions live in data-raw/meta/compile_townships.R (sourced above).
# Each group sits wholly within one county; county_assign() uses this table
# to resolve "Carver Townships" -> Carver, etc. The lookup call validates
# membership against the freshly built table.
twp_lookup <- twp_group_lookup(ctu_county_area)

ctu_county_area <- twp_group_replace(
  ctu_county_area,
  twp_group_meta() %>%
    transmute(
      geog_name,
      county_name,
      geog_id,
      pct_of_ctu_area = 1
    )
)

# Geometry ----
# One (multi)polygon per geog_id. CTU pieces are unioned back to a single
# feature; region and township groups are unioned from their parts.
ctu_geom <- ctu_pieces %>%
  select(geog_id, geometry) %>%
  group_by(geog_id) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop")

county_geom <- county_geog %>%
  select(geog_id = GEOID, geometry) %>%
  sf::st_transform(sf::st_crs(ctu_geom)) %>%
  sf::st_make_valid()

region_geom <- county_geom %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  mutate(geog_id = geog_index$geog_id[geog_index$geog_level == "REGION"])

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

# waldo::compare(ctu_county_area, ghg.gert::ctu_county_area)
usethis::use_data(geog_geometry, overwrite = T)
usethis::use_data(ctu_county_area, overwrite = T)
