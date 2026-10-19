# Waste activity data (solid waste + wastewater) ----
#
# Produces package data object `waste_data` (list):
#   $inventory             annual activity, years <= ref_year
#   $projections           annual activity, years > ref_year (ref_year per capita rates)
#   $solid_waste_baseline  ref_year source shares vs MPCA 2030 targets
#   $mpca                  MPCA waste reduction and source diversion targets
#   $characterization      MPCA waste composition
#   $epa                   EPA wastewater constants and protein consumption
#
# Geographies: CTUs, counties, region, township groups.
# Output columns (inventory_year, value_activity, ...) are what the waste
# module in R/w_*.R expects; demographic_data's emissions_year is renamed on read.

pkgload::load_all()
source("data-raw/meta/compile_townships.R")

ref_year <- 2022 # last inventory year; projections start the year after
proj_label <- paste0("forecast using ", ref_year, " per capita estimates")
inpath_waste <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_waste/data-raw/"

out_cols <- c(
  "inventory_year", "geog_id", "geog_name", "geog_level", "geog_pop",
  "source", "value_activity", "units_activity", "data_type"
)

# 1. Population ----
population <- demographic_data %>%
  filter(sp_categories == "population") %>%
  select(geog_id, geog_name, geog_level, geog_id_type,
    inventory_year = emissions_year, geog_pop = value
  )

county_pop <- filter(population, geog_level == "COUNTY")
ctu_pop <- filter(population, geog_id_type == "ctu_gnis")

# 2. CTU population share by county ----
# One row per CTU-county; CTUs split by a county line have several, each with
# its share of the CTU's population.
ctu_county_pop <- ctu_county %>%
  transmute(
    geog_id = stringr::str_pad(ctu_id, width = 8, pad = "0"),
    co_code = paste0("27", stringr::str_pad(co_code, width = 3, pad = "0")),
    pct_population
  ) %>%
  bind_rows(tibble(geog_id = "02831011", co_code = "27037", pct_population = 1)) %>%
  filter(pct_population > 0)

missing_share <- setdiff(unique(ctu_pop$geog_id), ctu_county_pop$geog_id)
if (length(missing_share) > 0) {
  cli::cli_warn("CTUs with no county population share (no solid waste): {.val {missing_share}}")
}

# 3. County solid waste (MPCA SCORE, through ref_year) ----
county_sw <- readr::read_rds(paste0(inpath_waste, "solid_waste/mpca_score_allyrs.RDS")) %>%
  select(geog_id = geoid, inventory_year, source, value_activity, units_activity) %>%
  filter(inventory_year <= ref_year) %>%
  # inner join keeps the 7 metro counties (drops Chisago, Sherburne)
  inner_join(county_pop, by = c("geog_id", "inventory_year"))

# per capita rate by county, source and year; years after ref_year carry the
# ref_year rate forward
per_cap <- county_sw %>%
  transmute(co_code = geog_id, inventory_year, source, value_per_cap = value_activity / geog_pop)

per_cap <- bind_rows(
  per_cap,
  per_cap %>%
    filter(inventory_year == ref_year) %>%
    select(-inventory_year) %>%
    tidyr::crossing(inventory_year = (ref_year + 1):max(population$inventory_year))
)

county_activity <- bind_rows(
  county_sw %>%
    mutate(data_type = "county-wide MPCA estimates"),
  county_pop %>%
    filter(inventory_year > ref_year) %>%
    inner_join(per_cap, by = c("geog_id" = "co_code", "inventory_year")) %>%
    mutate(
      value_activity = geog_pop * value_per_cap,
      units_activity = "metric tons MSW",
      data_type = proj_label
    )
) %>%
  select(all_of(out_cols))

# 4. CTU solid waste: downscale county per capita rates ----
# Each CTU's population is split across its counties and multiplied by that
# county's per capita rate (year-specific through ref_year, ref_year rate
# after), then summed back to the CTU.
ctu_activity <- ctu_pop %>%
  inner_join(ctu_county_pop, by = "geog_id", relationship = "many-to-many") %>%
  inner_join(per_cap, by = c("co_code", "inventory_year"), relationship = "many-to-many") %>%
  group_by(inventory_year, geog_id, geog_name, geog_level, geog_pop, source) %>%
  summarize(value_activity = sum(geog_pop * pct_population * value_per_cap), .groups = "drop") %>%
  mutate(
    units_activity = "metric tons MSW",
    data_type = if_else(inventory_year <= ref_year,
      "downscaled from county-wide MPCA estimates", proj_label
    )
  ) %>%
  select(all_of(out_cols))

# 5. Township groups ----
# Summed from member townships (matched on GNIS id); geog_pop repeats on
# every source row, so it is summed once per township per year.
ctu_activity <- twp_group_replace(
  ctu_activity,
  twp_group_sum(ctu_activity,
    value_cols = "value_activity",
    group_cols = c("inventory_year", "source", "units_activity", "data_type"),
    once_cols = "geog_pop", once_by = "inventory_year",
    key = "geog_id",
    .fill = list(geog_level = TWP_GROUP_LEVEL)
  )
)
twp_group_check_sum(ctu_activity, "value_activity", "CTU solid waste", key = "geog_id")

# 6. Region: sum of counties ----
region <- geog_index %>% filter(geog_level == "REGION")

region_activity <- county_activity %>%
  group_by(inventory_year, source, units_activity, data_type) %>%
  summarize(value_activity = sum(value_activity), .groups = "drop") %>%
  left_join(
    county_pop %>%
      group_by(inventory_year) %>%
      summarize(geog_pop = sum(geog_pop), .groups = "drop"),
    by = "inventory_year"
  ) %>%
  mutate(geog_id = region$geog_id, geog_name = region$geog_name, geog_level = region$geog_level) %>%
  select(all_of(out_cols))

solid_waste <- bind_rows(ctu_activity, county_activity, region_activity)

# 7. Assemble waste_data ----
# wastewater uses population as its activity scalar: one template row per
# geography and year
add_wastewater <- function(df) {
  df %>%
    distinct(inventory_year, geog_id, geog_name, geog_level, geog_pop) %>%
    mutate(
      source = "Wastewater",
      value_activity = NA_real_,
      units_activity = "use population as scalar",
      data_type = "use population as scalar"
    ) %>%
    bind_rows(df, .) %>%
    arrange(geog_id, inventory_year, source)
}

waste_data <- list()
waste_data$inventory <- solid_waste %>%
  filter(inventory_year <= ref_year) %>%
  add_wastewater()
waste_data$projections <- solid_waste %>%
  filter(inventory_year > ref_year) %>%
  add_wastewater()

# ref_year source shares compared with MPCA 2030 targets
# (MPCA Metropolitan Solid Waste Management Policy Plan 2022-2042, Table 2)
mpca_2030 <- tibble(
  source = c("Recycling", "Organics", "Waste to energy", "Landfill", "Onsite", "MSW_Compost"),
  target2030 = c(0.474, 0.276, 0.2, 0.05, 0, 0)
)

waste_data$solid_waste_baseline <- waste_data$inventory %>%
  filter(inventory_year == ref_year, source != "Wastewater") %>%
  select(-data_type, -inventory_year) %>%
  group_by(geog_id) %>%
  mutate(
    total_activity = sum(value_activity),
    pct_of_total = value_activity / total_activity
  ) %>%
  ungroup() %>%
  left_join(mpca_2030, by = "source") %>%
  mutate(
    changeFromTarget = pct_of_total - target2030,
    action = case_when(
      changeFromTarget < 0 ~ paste0("Increase ", source),
      changeFromTarget > 0 ~ paste0("Decrease ", source),
      TRUE ~ paste0("Keep ", source)
    )
  )

# MPCA targets
waste_data$mpca$waste_reduction <- tibble(
  year = c(2025, 2030, 2036, 2042),
  target_reduction_pct = c(0.029, 0.064, 0.107, 0.15),
  note = "MPCA waste reduction system objectives in percentages"
)

waste_data$mpca$source_diversions <- tibble(
  source = mpca_2030$source,
  `2025` = c(0.369, 0.215, 0.24, 0.176, 0, 0),
  `2030` = mpca_2030$target2030,
  `2036` = mpca_2030$target2030,
  `2042` = mpca_2030$target2030,
  note = "MPCA solid waste management policy plan"
) %>%
  pivot_longer(
    cols = c(`2025`, `2030`, `2036`, `2042`),
    names_to = "year",
    values_to = "pct_of_total"
  ) %>%
  arrange(year, source)

# waste characterization and EPA wastewater inputs
waste_data$characterization <- readr::read_rds(paste0(inpath_waste, "solid_waste/mpca_waste_composition.RDS"))
waste_data$epa$wastewater_constants <- readr::read_rds(paste0(inpath_waste, "wastewater/epa/epa_wastewater_constants.rds"))
waste_data$epa$protein_consumption <- readr::read_rds(paste0(inpath_waste, "wastewater/epa/epa_protein_consumption.rds"))

# checks ----
# every geog_index geography has inventory rows
missing_geogs <- setdiff(geog_index$geog_id, waste_data$inventory$geog_id)
if (length(missing_geogs) > 0) {
  cli::cli_warn("geog_index geographies with no waste rows: {.val {missing_geogs}}")
}

# one row per geography / year / source
stopifnot(
  !anyDuplicated(waste_data$inventory[c("geog_id", "inventory_year", "source")]),
  !anyDuplicated(waste_data$projections[c("geog_id", "inventory_year", "source")])
)

# CTUs should sum to the county totals (township groups excluded)
ref_check <- solid_waste %>%
  filter(inventory_year == ref_year) %>%
  mutate(level = case_when(
    geog_level == "COUNTY" ~ "county",
    geog_id %in% ctu_pop$geog_id ~ "ctu",
    TRUE ~ "other"
  )) %>%
  group_by(level) %>%
  summarize(total = sum(value_activity, na.rm = TRUE), .groups = "drop")
print(ref_check)

usethis::use_data(waste_data, overwrite = TRUE)
