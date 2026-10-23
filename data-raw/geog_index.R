# create index of geographies ----

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

# Save ----
usethis::use_data(geog_index, overwrite = TRUE)

