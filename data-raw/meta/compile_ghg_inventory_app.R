# ============================================================================
# import_ghg_inventory.R
#
# Build the `ghg_inventory` package data object (regional / municipal / full
# CTU) from the ghg-cprg CTU and county emissions outputs.
#
# Makes app specific recategorizations of sectors and subsectors, tailored
# to climate mitigation minimum requirements
#
# BANDAID (flag for transportation DS review): CTU transportation splices
# module BAU over years >= 2015 and keeps raw ghg-cprg for 2005-2014, with no
# alignment across the seam. The pre-2015 delta-shift smoothing lives only in
# the scenario compile step, not here, so a 2014->2015 discontinuity can remain
# in the inventory. Left as-is pending a permanent fix.
# ============================================================================

library(dplyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
devtools::load_all(".")

# helpers --------------------------------------------------------------------

# ghg-cprg main-branch raw URL
ghg_cprg_url <- function(path) {
  paste0(
    "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/",
    path
  )
}

# Recode the rebuilt ghg-cprg building-energy labels onto the app's existing
# category names. Electricity arrives as "... building energy"; natural gas /
# liquid fuel arrive as "... building fuel". Residential building fuel is left
# as-is so its natural gas + propane/fuel oil sources sum together.
recode_building_category <- function(category) {
  dplyr::case_when(
    category == "Residential building energy" ~ "Residential electricity",
    category == "Business building energy" ~ "Non-residential electricity",
    TRUE ~ category
  )
}

# BAU CTU transportation -----------------------------------------------------
# Regenerate the per-CTU BAU transportation summaries directly from the
# transportation module. Replaces the retired `transp_bau_ctu` package object.
# Each element carries `$sum_mode_year`, which the transportation-replace block
# below consumes exactly as it consumed the old package object.
#
# Direct module call keeps this transportation-only: no buildings, land use, or
# building emission factors are touched. `run_module_transportation` defaults to
# `.scenario = "BAU"` and pulls its inputs (transportation_data, factor_values,
# elast, fuel_economy, enviro_factors, ...) from ghg.ccap defaults.
#
# NOTE: heavy per-CTU run. If build time becomes a problem, split this back out
# into a `compile_transp_bau_ctu.R` data-raw step that saves `transp_bau_ctu`.
# Confirm `ctu_list` still ships with ghg.ccap (else derive from geog_index:
#   dplyr::filter(geog_index, geog_level != "COUNTY")$geog_name).

ctu_list <- filter(geog_index, geog_level %in% c("CITY", "TOWNSHIP"))$geog_name

summarize_transportation <- function(transportation_module_output) {
  # browser()
  geography_name <- transportation_module_output$passenger_all$geog_name %>%
    unique()

  sum_mode_year <- transportation_module_output$passenger_all %>%
    dplyr::bind_rows(transportation_module_output$freight_all) %>%
    dplyr::filter(!mode %in% c(
      "MM", "RI", "RU", "FR",
      "WAT", "AIR"
    )) %>%
    dplyr::left_join(
      ghg.ccap::transportation_index$modes %>%
        dplyr::select(mode_abbrev, mode_description_1, sector, category),
      by = c("mode" = "mode_abbrev")
    ) %>%
    dplyr::mutate(emissions_year = as.numeric(year)) %>%
    dplyr::group_by(emissions_year, type, geog_name, geog_id, category, sector) %>%
    dplyr::summarize(
      dir_ghg = sum(dir_ghg, na.rm = T),
      vmt = sum(vmt, na.rm = T)
    ) %>%
    ungroup()

  sum_sector_year <- sum_mode_year %>%
    group_by(geog_name, geog_id, emissions_year, sector) %>%
    dplyr::summarize(
      dir_ghg = sum(dir_ghg, na.rm = T),
      vmt = sum(vmt, na.rm = T)
    )

  sum_year <- sum_mode_year %>%
    group_by(geog_name, geog_id, emissions_year) %>%
    dplyr::summarize(
      dir_ghg = sum(dir_ghg, na.rm = T),
      vmt = sum(vmt, na.rm = T),
      .groups = "keep"
    )

  sum_vehicles <- tabulate_alternative_vehicles(transportation_module_output)


  return(
    list(
      "sum_sector_year" = sum_sector_year,
      "sum_mode_year" = sum_mode_year,
      "sum_year" = sum_year,
      "sum_vehicles" = sum_vehicles,
      "module_output" = transportation_module_output
    )
  )
}

tabulate_alternative_vehicles <- function(pass_tb = transportation_data$passenger) {
  # browser()


  pass_tb$pass_tb %>%
    dplyr::filter(
      stringr::str_detect(var, "Stock"),
      mode == "PLDV"
    ) %>%
    tidyr::pivot_wider(
      names_from = var,
      values_from = value
    ) %>%
    # filter(year == max(year)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      conventional = sum(SIStock, CIStock),
      HEV = sum(HEVStock),
      BEV = sum(BEVStock),
      HEV_pct = HEV / TotStock,
      BEV_pct = BEV / TotStock,
      alternative = sum(HEVStock, BEVStock),
      pct_alternative = alternative / TotStock
    ) %>%
    # select(geog_id, geog_name, TotStock, alternative, pct_alternative) %>%
    unique() %>%
    return()
}


transp_bau_ctu <- purrr::map(
  ctu_list,
  function(x) {
    # cli::cli_alert_info(x)
    suppressMessages(
      summarize_transportation(
        run_module_transportation(.selected_ctu = x)
      )
    )
  }
)


# load ghg-cprg outputs ------------------------------------------------------

ghg_ctu <- read_rds(ghg_cprg_url("_meta/data/ctu_emissions.RDS")) %>%
  filter(emissions_year < 2023)

# diagnostic: any CTU rows still missing emissions (resolve upstream in cprg)
ghg_ctu %>%
  filter(is.na(value_emissions)) %>%
  print(n = 100)
### ssp and commercial natural gas
### shorewood wastewater


ghg_county <- read_rds(ghg_cprg_url("_meta/data/cprg_county_emissions.RDS")) %>%
  filter(
    !county_name %in% c("St. Croix", "Pierce", "Sherburne", "Chisago"),
    emissions_year < 2023
  )

# transportation replacement (CTU) -------------------------------------------
# Splice the BAU transport module output over the raw ghg-cprg transportation
# for years >= 2015 (interpolated to a continuous annual series); 2005-2014
# stay as raw ghg-cprg. Unchanged from prior behavior -- see BANDAID note above.

ctu_transportation_replace <- purrr::map_dfr(
  transp_bau_ctu,
  function(x) x$sum_mode_year
) %>%
  ungroup() %>%
  filter(emissions_year %in% unique(ghg_ctu$emissions_year) | emissions_year == 2025) %>%
  mutate(
    source = "All Vehicles",
    sector = "Transportation",
    sector_alt = "Transportation",
    value_emissions = dir_ghg,
    ctu_id_gnis = geog_id
  ) %>%
  select(-geog_name) %>%
  full_join(ghg_ctu %>%
    select(geog_level, geog_name, ctu_class, ctu_id_gnis, ctu_id_fips) %>%
    unique(), by = "ctu_id_gnis") %>%
  filter()

year_ctu_mode_alt <- ctu_transportation_replace %>%
  select(geog_level, geog_name, ctu_class, ctu_id_gnis, ctu_id_fips, type, category, source, sector, sector_alt, emissions_year) %>%
  unique() %>%
  filter(!is.na(type)) %>%
  group_by(geog_level, geog_name, ctu_class, ctu_id_gnis, ctu_id_fips, type, category, source, sector, sector_alt) %>%
  tidyr::complete(emissions_year = 2015:2025) %>%
  mutate(geog_id = ctu_id_gnis)

transport_replace <- ctu_transportation_replace %>%
  ungroup() %>%
  mutate(geog_id = ctu_id_gnis) %>%
  right_join(
    year_ctu_mode_alt,
    by = c(
      "emissions_year", "type", "geog_id", "category", "sector", "source",
      "sector_alt", "ctu_id_gnis", "geog_level", "geog_name", "ctu_class",
      "ctu_id_fips"
    )
  ) %>%
  arrange(geog_id, -emissions_year) %>%
  group_by(category, sector, source, sector_alt) %>%
  mutate(value_emissions = zoo::na.approx(value_emissions)) %>%
  ungroup() %>%
  left_join(
    ghg_ctu %>%
      select(emissions_year, ctu_population, ctu_id_gnis) %>%
      unique(),
    by = c("ctu_id_gnis", "emissions_year")
  ) %>%
  mutate(emissions_per_capita = value_emissions / ctu_population) %>%
  filter(emissions_year <= max(ghg_ctu$emissions_year)) %>%
  select(names(ghg_ctu))

ghg_ctu_replace <- ghg_ctu %>%
  ungroup() %>%
  filter(!(sector == "Transportation" & emissions_year %in% unique(transport_replace$emissions_year))) %>%
  bind_rows(transport_replace)

# CTU inventory (summarized, no source) --------------------------------------
# Categorization preserved from prior version:
#   * legacy Commercial point-source combustion -> non-residential building fuel
#   * Business -> Non-residential
# New building-energy electricity labels are recoded to the existing
# "* electricity" names, and residential building fuel (natural gas +
# propane/fuel oil) now flows through and is summed.

ctu_ghg_inventory <- ghg_ctu_replace %>%
  dplyr::mutate(
    geog_name = if_else(ctu_class == "TOWNSHIP",
      paste(geog_name, "Twp."),
      geog_name
    )
  ) %>%
  dplyr::rename(
    population = ctu_population,
    fips_id = ctu_id_fips
  ) %>%
  dplyr::select(-c(ctu_id_gnis)) %>%
  dplyr::mutate(
    sector = if_else(sector == "Commercial", "Business", sector),
    category = if_else(category == "Stationary combustion", "Business building fuel", category),
    category = recode_building_category(category)
  ) %>%
  group_by(emissions_year, geog_level, geog_name, ctu_class, fips_id, population, sector, category) %>%
  summarize(value_emissions = sum(value_emissions, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    sector = stringr::str_replace_all(sector, "Business", "Non-residential"),
    category = stringr::str_replace_all(category, "Business", "Non-residential")
  )

# County inventory -----------------------------------------------------------
# Categorization preserved from prior version; only the ghg-cprg label that
# changed is updated ("Building Fuel" -> "Building fuel"). With that fix the
# residential "Building fuel" category (natural gas + liquid fuel sources)
# maps to "Residential building fuel" and both sources sum.

county_ghg_inventory <- ghg_county %>%
  dplyr::mutate(
    ctu_class = "COUNTY",
    category = as.character(category),
    geog_name = paste(county_name, "County")
  ) %>%
  dplyr::rename(
    population = county_total_population,
    fips_id = geoid
  ) %>%
  dplyr::select(-c(data_source, county_name, factor_source, population_data_source)) %>%
  mutate(
    sector = dplyr::case_when(
      sector == "Business" ~ "Non-residential",
      sector == "Commercial" ~ "Non-residential",
      sector == "Industrial" &
        category %in% c(
          "Electricity",
          "Building fuel"
        ) ~ "Non-residential",
      TRUE ~ sector
    ),
    category = dplyr::case_when(
      category %in% c(
        "Building fuel",
        "Commercial fuel combustion",
        "Commercial natural gas"
      ) &
        sector %in% c(
          "Non-residential",
          "Industrial"
        ) ~ "Non-residential building fuel",
      category == "Building fuel" &
        sector == "Residential" ~ "Residential building fuel",
      category == "Electricity" &
        sector %in% c(
          "Non-residential",
          "Industrial"
        ) ~ "Non-residential electricity",
      category == "Electricity" &
        sector == "Residential" ~ "Residential electricity",
      category %in% c(
        "Industrial fuel combustion",
        "Industrial natural gas"
      ) ~ "Industrial fuel combustion",
      TRUE ~ category
    )
  ) %>%
  # reorder sectors
  mutate(
    sector = factor(sector,
      levels = c(
        "Transportation",
        "Residential",
        "Non-residential",
        "Industrial",
        "Waste",
        "Agriculture",
        "Natural Systems"
      )
    ),
    category = factor(category,
      levels = c(
        "Passenger vehicles", "Buses", "Trucks", "Active", "Off-road", "Aviation",
        "Residential electricity", "Residential building fuel",
        "Non-residential electricity", "Non-residential building fuel",
        "Industrial processes", "Industrial fuel combustion", "Refinery processes",
        "Wastewater", "Solid waste",
        "Cropland", "Livestock",
        "Sequestration", "Freshwater"
      ),
      ordered = TRUE
    )
  ) %>%
  ### hot fix, needs permanent fix in cprg repo
  mutate(value_emissions = if_else(is.na(value_emissions),
    0,
    value_emissions
  )) %>%
  group_by(emissions_year, geog_level, geog_name, ctu_class, fips_id, population, sector, category) %>%
  summarize(value_emissions = sum(value_emissions), .groups = "drop")

# regional inventory ---------------------------------------------------------

regional_pop <- county_ghg_inventory %>%
  distinct(emissions_year, fips_id, population) %>%
  group_by(emissions_year) %>%
  summarize(population = sum(population, na.rm = TRUE), .groups = "drop")

regional_ghg_inventory <- county_ghg_inventory %>%
  group_by(emissions_year, sector, category) %>%
  summarize(value_emissions = sum(value_emissions), .groups = "drop") %>%
  left_join(regional_pop, by = "emissions_year")

# full CTU inventory (keeps source) ------------------------------------------
# Same categorization as the summarized CTU inventory, retaining `source`.

full_ctu_ghg_inventory <- ghg_ctu_replace %>%
  dplyr::mutate(
    unit_emissions = "Metric tons CO2e",
    geog_name = if_else(ctu_class == "TOWNSHIP",
      paste(geog_name, "Twp."),
      geog_name
    )
  ) %>%
  dplyr::rename(
    population = ctu_population,
    fips_id = ctu_id_fips
  ) %>%
  dplyr::select(-c(ctu_id_gnis)) %>%
  dplyr::mutate(
    sector = if_else(sector == "Commercial", "Non-residential", sector),
    category = if_else(category == "Stationary combustion", "Non-residential building fuel", category),
    category = recode_building_category(category)
  ) %>%
  group_by(emissions_year, geog_level, geog_name, ctu_class, fips_id, population, sector, category, source) %>%
  summarize(value_emissions = sum(value_emissions, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    sector = stringr::str_replace_all(sector, "Business", "Non-residential"),
    category = stringr::str_replace_all(category, "Business", "Non-residential")
  )

# assemble + save ------------------------------------------------------------

municipal_ghg_inventory <- bind_rows(
  ctu_ghg_inventory,
  county_ghg_inventory
)

ghg_inventory <- list(
  regional_ghg_inventory = regional_ghg_inventory,
  municipal_ghg_inventory = municipal_ghg_inventory,
  full_ctu_ghg_inventory = full_ctu_ghg_inventory
)

usethis::use_data(ghg_inventory, overwrite = TRUE)
