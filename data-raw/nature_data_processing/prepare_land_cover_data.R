# Natural systems land cover data ----
#
# Restored from data-raw/nature_data_processing/00_prepare_land_cover_data.R
# (deleted in 3e6ae361) and completed to reproduce the saved object.
#
# Produces package data object `natural_systems_data` (list):
#   $land_cover_carbon        sequestration / stock factors by land cover type
#   $inventory$ctu            NLCD land cover by CTU, 2000 to last NLCD year
#   $inventory$county         same, summed to county
#   $inventory$region         same, summed across the 7 metro counties
#   $projections$ctu/county/region
#                             last inventory year carried forward to 2050
#
# Geographies follow geog_index: the 7 metro counties, their CTUs, the
# region, and township groups (added to the $ctu tables, summed from members).
# The natural systems module (R/n_01_run_scenario_natural_systems.R) binds
# each list's tables together and filters on geog_name.

pkgload::load_all()
source("data-raw/meta/compile_townships.R")

inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_nature/data/"
proj_end <- 2050

lc_cols <- c(
  "geog_name", "ctu_class", "geog_id", "inventory_year",
  "land_cover_type", "area", "potential_wetland_area"
)

# sum area by geography, year and land cover type
sum_land_cover <- function(df, ...) {
  df %>%
    group_by(..., inventory_year, land_cover_type) %>%
    summarize(
      area = sum(area),
      potential_wetland_area = sum(potential_wetland_area),
      .groups = "drop"
    )
}

natural_systems_data <- list()

# land cover carbon factors ----
natural_systems_data$land_cover_carbon <- readr::read_rds(paste0(inpath, "land_cover_carbon.rds"))

# geog_index geographies ----
gi_county <- geog_index %>%
  filter(geog_level == "COUNTY") %>%
  select(county_name = geog_short_name, geog_id, geog_name)
gi_ctu <- geog_index %>%
  filter(geog_id_type == "ctu_gnis") %>%
  select(geog_id, geog_name)
gi_region <- geog_index %>% filter(geog_level == "REGION")

# CTU land cover ----
# Source rows are CTU-county pieces. Keep pieces in the 7 metro counties
# (drops Chisago, Sherburne, St. Croix, Pierce); CTUs split by a county line
# are summed to one row per CTU, and pieces are summed by county below.
lc_pieces <- readr::read_rds(paste0(inpath, "nlcd_ctu_landcover_allyrs.rds")) %>%
  ungroup()

if (!"county_name" %in% names(lc_pieces)) {
  cli::cli_abort(c(
    "{.file nlcd_ctu_landcover_allyrs.rds} has no {.field county_name} column.",
    "i" = "Columns: {.field {names(lc_pieces)}}",
    "i" = "Swap in the county column name here."
  ))
}

lc_pieces <- lc_pieces %>%
  mutate(
    county_name = sub(" County$", "", county_name),
    geog_id = stringr::str_pad(ctu_id, width = 8, pad = "0", side = "left")
  ) %>%
  filter(county_name %in% gi_county$county_name)

# check CTU coverage against geog_index
missing_ctus <- setdiff(gi_ctu$geog_id, lc_pieces$geog_id)
if (length(missing_ctus) > 0) {
  cli::cli_warn("geog_index CTUs with no land cover: {.val {gi_ctu$geog_name[gi_ctu$geog_id %in% missing_ctus]}}")
}

# CTU tier: geog_index CTUs only, named as in geog_index
# (CTUs outside geog_index, e.g. Non-Council communities, still count toward
# their county totals below)
lc_ctu <- lc_pieces %>%
  inner_join(gi_ctu, by = "geog_id") %>%
  sum_land_cover(geog_name, ctu_class, geog_id)

# township groups: summed from member townships, matched on GNIS id
lc_ctu <- twp_group_replace(
  lc_ctu,
  twp_group_sum(lc_ctu,
                value_cols = c("area", "potential_wetland_area"),
                group_cols = c("inventory_year", "land_cover_type"),
                key = "geog_id",
                .fill = list(ctu_class = TWP_GROUP_LEVEL)
  )
)
twp_group_check_sum(lc_ctu, "area", "natural systems CTU", key = "geog_id")

# County land cover ----
# Summed from all CTU pieces in each county; names and FIPS from geog_index.
lc_county <- lc_pieces %>%
  sum_land_cover(county_name) %>%
  inner_join(gi_county, by = "county_name") %>%
  mutate(ctu_class = "COUNTY")

stopifnot(setequal(lc_county$geog_id, gi_county$geog_id))

# Region: sum of the 7 metro counties ----
lc_region <- lc_county %>%
  sum_land_cover() %>%
  mutate(geog_name = gi_region$geog_name, ctu_class = "REGION", geog_id = gi_region$geog_id)

# Assemble inventory and projections ----
inventory <- list(ctu = lc_ctu, county = lc_county, region = lc_region) %>%
  purrr::map(~ select(.x, all_of(lc_cols)) %>% arrange(geog_name, inventory_year, land_cover_type))

# projections hold the last inventory year constant through 2050
last_year <- max(lc_ctu$inventory_year)

natural_systems_data$inventory <- inventory
natural_systems_data$projections <- inventory %>%
  purrr::map(~ .x %>%
               filter(inventory_year == last_year) %>%
               select(-inventory_year) %>%
               tidyr::crossing(inventory_year = (last_year + 1):proj_end) %>%
               select(all_of(lc_cols)) %>%
               arrange(geog_name, inventory_year, land_cover_type))

# checks ----
# county pieces should add up to the region, every year
stopifnot(isTRUE(all.equal(
  sum(lc_county$area), sum(lc_region$area), tolerance = 1e-6
)))
twp_group_check_present(natural_systems_data$inventory$ctu, "natural_systems_data$inventory$ctu")
twp_group_check_present(natural_systems_data$projections$ctu, "natural_systems_data$projections$ctu")

# compare with the saved version: expect new township group rows, the four
# border counties and their CTUs gone, and the region renamed and re-summed
# waldo::compare(natural_systems_data, ghg.gert::natural_systems_data, tolerance = 1e-6)

usethis::use_data(natural_systems_data, overwrite = TRUE)
