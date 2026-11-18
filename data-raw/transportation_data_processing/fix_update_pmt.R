pkgload::load_all()
source("data-raw/meta/compile_townships.R")

# Rebuild passenger PMT; freight TMT and all fleet variables are unchanged.
# Region PMT is rebuilt from CTUs later by fix_region_aggregate.R.

# Daily VMT is split by CTU (gnis), county (geoid), and inventory year.
coctu_vmt_forecast <- readRDS(url("https://github.com/Metropolitan-Council/ghg-cprg/raw/refs/heads/main/_transportation/data/mndot_vmt_ctu_gap_filled.RDS"))

# Combine county pieces into whole-CTU VMT, including cross-county CTUs.
# Only the first source label survives if the county pieces differ.
ctu_vmt_forecast <- coctu_vmt_forecast %>%
  group_by(gnis, inventory_year) %>%
  summarize(
    final_city_vmt = sum(final_city_vmt),
    final_vmt_source = first(final_vmt_source),
    .groups = "keep"
  ) %>%
  ungroup()

# Sum member-township VMT before applying the group AVO.
twp_lookup <- twp_group_lookup()

# Carver and Scott township groups only; cities, counties, and region are excluded.
# These are still VMT totals; each group's PLDV AVO is applied below.
twp_vmt <- ctu_vmt_forecast %>%
  twp_group_tag(by = "gnis", key = "geog_id", lookup = twp_lookup) %>%
  group_by(group_id, inventory_year) %>%
  summarize(
    final_city_vmt = round(sum(final_city_vmt)),
    final_vmt_source = paste(sort(unique(final_vmt_source)), collapse = ", "),
    n_members = n_distinct(gnis),
    .groups = "drop"
  )

# Require both groups in every source year, each with all 10 member townships.
testthat::expect_equal(
  nrow(twp_vmt),
  n_distinct(ctu_vmt_forecast$inventory_year) * nrow(twp_group_meta())
)
testthat::expect_setequal(unique(twp_vmt$group_id), twp_group_meta()$geog_id)
testthat::expect_true(all(
  twp_vmt %>%
    left_join(twp_group_meta() %>% select(group_id = geog_id, expected = n_members), by = "group_id") %>%
    with(n_members == expected)
))

# Sum existing non-PLDV PMT for member townships, not county/region/group rows.
# Currently covers AT, BIKE, BS, BU, RI, RU, WALK; no VMT-to-PMT conversion.
twp_vmt_other <- transportation_data$passenger %>%
  mutate(year = as.character(year)) %>%
  filter(var == "PMT", mode != "PLDV") %>%
  twp_group_tag(by = "geog_id", key = "geog_id", lookup = twp_lookup) %>%
  group_by(group_id, year, mode, var, aeo_mode, type) %>%
  summarize(
    value = round(sum(value, na.rm = TRUE)),
    n_members = n_distinct(geog_id),
    .groups = "drop"
  ) %>%
  mutate(geog_id = group_id) %>%
  left_join(geog_index %>% select(geog_id, geog_name), by = "geog_id")


# TODO: Derive the mode count from non-PLDV PMT, not all passenger modes minus 3.
# With the current data, this expects 90 rows but the seven PMT modes yield 126.
testthat::expect_equal(
  nrow(twp_vmt_other),
  n_distinct(transportation_data$passenger$year) * nrow(twp_group_meta()) * (length(unique(transportation_data$passenger$mode)) - 3)
)

testthat::expect_setequal(unique(twp_vmt_other$group_id), twp_group_meta()$geog_id)
testthat::expect_true(all(
  twp_vmt_other %>%
    left_join(twp_group_meta() %>% select(group_id = geog_id, expected = n_members), by = "group_id") %>%
    with(n_members == expected)
))

# CTUs plus the two township groups, with group IDs in the same key column.
# No county or region VMT rows are added here.
ctu_vmt_forecast_complete <- bind_rows(
  ctu_vmt_forecast,
  twp_vmt %>% transmute(
    gnis = group_id, inventory_year, final_city_vmt, final_vmt_source
  )
)

ctu_vmt_source <- ctu_vmt_forecast_complete %>%
  group_by(gnis, final_vmt_source) %>%
  summarize(vmt_years = paste0(min(as.numeric(inventory_year)), "-", max(as.numeric(inventory_year)), collapse = ", "), .groups = "keep") %>%
  ungroup() %>%
  arrange(gnis, final_vmt_source)



# The subsequent ID join selects CTU/group PLDV occupancy, not county occupancy.
pldv_avo <- vehicle_occupancy %>%
  filter(
    var == "AVO",
    mode == "PLDV"
  ) %>%
  select(
    geog_id, geog_name,
    AVO = value
  ) %>%
  unique()

# PLDV PMT only: whole-CTU/group daily VMT times that geography's AVO.
# Restrict source years to those already represented in passenger data.
pmt_new <- ctu_vmt_forecast_complete %>%
  # only pull out the years that are in the current dataset
  filter(inventory_year %in% c(transportation_data$passenger$year)) %>%
  left_join(pldv_avo, by = c("gnis" = "geog_id")) %>%
  rowwise() %>%
  mutate(
    value =
    # PMT is VMT multiplied by average vehicle occupancy
    # then annualized with 340
    # and multiplied by 0.9 to account for truck traffic
    # which we expect to be about 10% of total traffic
      round(((final_city_vmt * AVO) * 340) * 0.9),
    mode = "PLDV",
    var = "PMT",
    geog_id = gnis,
    year = as.character(inventory_year),
    type = "P",
    aeo_mode = "LDV"
  ) %>%
  ungroup() %>%
  select(mode, var, geog_name, geog_id, year, value, type, aeo_mode)


# County allocation weights ----
# Reuse the original CTU/county pieces, not the appended township-group VMT.

coctu_vmt <- coctu_vmt_forecast %>%
  group_by(gnis, inventory_year) %>%
  summarize(
    total_coctu_vmt = round(sum(final_city_vmt, na.rm = TRUE)),
    .groups = "drop"
  )

# Allocate cross-county CTUs by their VMT shares, not their land-area shares.
# Two-decimal rounding can affect conservation; missing/zero-total shares become 0.
city_vmt_proportions <- coctu_vmt_forecast %>%
  left_join(coctu_vmt, by = c("gnis", "inventory_year")) %>%
  mutate(
    pct_vmt_in_county = round(final_city_vmt / total_coctu_vmt, digits = 2),
    pct_vmt_in_county = if_else(is.na(pct_vmt_in_county), 0, pct_vmt_in_county)
  ) %>%
  select(gnis, geoid, inventory_year, pct_vmt_in_county) %>%
  left_join(geog_index %>% select(geog_id, geog_name, geog_level), by = c("gnis" = "geog_id"))


# Retain existing PMT for these non-PLDV modes at ALL existing geographies,
# then append recalculated PLDV PMT for CTUs and township groups.
# BRT is included if present. Modes outside this list and PLDV are not retained.
# Despite its name, this object is not restricted to CTUs.
ctu_pmt_all <- transportation_data$passenger %>%
  filter(
    var == "PMT",
    mode != "PLDV"
  ) %>%
  bind_rows(pmt_new) %>%
  mutate(inventory_year = as.numeric(year)) %>%
  unique()

# County PMT for PLDV plus the listed non-PLDV modes.
# Only CTU rows matching the VMT lookup contribute; aggregate rows do not.
# PLDV uses allocated CTU PMT (and CTU AVO), NOT county VMT times county AVO.
# Non-PLDV modes also use road-VMT shares to split cross-county CTU activity.
county_pmt <- ctu_pmt_all %>%
  left_join(
    city_vmt_proportions,
    by = c("geog_id" = "gnis", "inventory_year", "geog_name"),
    relationship = "many-to-many"
  ) %>%
  filter(!is.na(pct_vmt_in_county)) %>%
  mutate(allocated_value = value * pct_vmt_in_county) %>%
  group_by(geoid, inventory_year, mode, var, aeo_mode, type) %>%
  summarize(
    value = sum(allocated_value, na.rm = TRUE) %>% round(digits = 0),
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


# TODO: Give twp_vmt_other geog_id/geog_name from group_id before binding.
# Currently its group_id is removed, and the missing-name filter drops every row.
# Existing non-PLDV county/group PMT in ctu_pmt_all is also retained rather than
# replaced, so reruns can keep stale group rows or duplicate county PMT.
pmt_new_all <- bind_rows(
  ctu_pmt_all %>%
    anti_join(
      bind_rows(county_pmt, twp_vmt_other),
      by = c("geog_id", "mode", "var", "year", "aeo_mode", "type")
    ),
  county_pmt,
  twp_vmt_other %>% select(-n_members, -group_id)
) %>%
  filter(!is.na(geog_name))


testthat::expect_equal(
  pmt_new_all %>%
    filter(is.na(geog_id) | is.na(geog_name) | is.na(value) | is.na(type)) %>%
    nrow(),
  0
)


# Replace ALL passenger PMT, not just PLDV. Other vars and freight are preserved.
# Region PLDV is absent until fix_region_aggregate.R; retained non-PLDV region
# values remain unchanged here and are also rebuilt by that later script.
transportation_data$passenger <- transportation_data$passenger %>%
  filter(!(var == "PMT")) %>%
  bind_rows(pmt_new_all %>% select(geog_id, geog_name, mode, var, year, value, aeo_mode, type))


usethis::use_data(transportation_data, overwrite = TRUE)


# Source-year metadata exists for CTUs and township groups with VMT records.
# The full join also includes PMT-only geographies (e.g. counties/region),
# which have no direct VMT source/year entry.
ctu_vmt_year_source <- ctu_vmt_source %>%
  mutate(geog_id = gnis) %>%
  full_join(pmt_new_all %>% select(geog_id, geog_name) %>% unique())


transportation_index$ctu_vmt_year_source <- ctu_vmt_year_source

usethis::use_data(transportation_index, overwrite = TRUE)
