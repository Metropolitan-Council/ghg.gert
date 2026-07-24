# import_tables-building_energy_data.R
#
# Consolidated ingestion of all building energy datasets for ghg.ccap.
# Produces package data object:
#   - building_energy_data  (list: inventories, forecasts, demographic slices)

devtools::load_all(".")

# helper: base URL for ghg-cprg repo ----
ghg_cprg_url <- function(path) {
  paste0(
    "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/",
    path
  )
}

# temp URL for ongoing ghg-cprg work
ghg_cprg_tmp <- function(path) {
  paste0(
    "https://github.com/Metropolitan-Council/ghg-cprg/raw/251-update-electricity-ctu-workflow/",
    path
  )
}

# Imagine community designations (for non-residential energy profiles)
cprg_ctu_desgn <- read_rds(ghg_cprg_url("_meta/data/cprg_ctu.RDS")) %>%
  sf::st_drop_geometry() %>%
  select(ctu_name, ctu_class, imagine_designation)

# load ghg-cprg inventories (electricity, natural gas, propane/fuel oil)


# electricity inventory ----
# CTU-level
ctu_elec_inventory <- read_rds(
  ghg_cprg_tmp("_energy/data/_ctu_electricity_emissions.RDS")
) %>%
  rename(geog_level = ctu_class) %>%
  left_join(ghg.ccap::geog_index, by = join_by(ctu_name == geog_name, geog_level)) %>%
  select(geog_name = ctu_name, geog_id, geog_level, sector, emissions_year, mwh)

#County-level electricity inventory
county_elec_inventory <- read_rds(
  ghg_cprg_tmp("_energy/data/county_elec_activity_by_sector.RDS")
) %>%
  mutate(
    geog_level = "COUNTY",
    sector = if_else(sector == "Residential", "Residential", "Business"),
    county_name = paste(county_name, "County")
  ) %>%
  left_join(ghg.ccap::geog_index, by = join_by(county_name == geog_name, geog_level)) %>%
  group_by(county_name, geog_id, geog_level, sector, emissions_year) %>%
  summarize(mwh = sum(value_activity), .groups = "drop") %>%
  select(
    geog_name = county_name, geog_id,
    geog_level, sector, emissions_year, mwh
  )

# natural gas inventory ----
# CTU-level
ctu_gas_inventory <- read_rds(
  ghg_cprg_url("_energy/data/_ctu_natgas_emissions.RDS")
) %>%
  rename(geog_level = ctu_class) %>%
  left_join(ghg.ccap::geog_index, by = join_by(ctu_name == geog_name, geog_level)) %>%
  select(geog_name = ctu_name, geog_id, geog_level, sector, emissions_year, mcf)

# County-level natural gas inventory
county_gas_inventory <- read_rds(
  ghg_cprg_url("_energy/data/county_natgas_activity_by_sector.RDS")
) %>%
  filter(sector != "Powerplant") %>%
  mutate(
    geog_level = "COUNTY",
    sector = if_else(sector == "Residential", "Residential", "Business"),
    county_name = paste(county_name, "County")
  ) %>%
  left_join(ghg.ccap::geog_index, by = join_by(county_name == geog_name, geog_level)) %>%
  group_by(county_name, geog_id, geog_level, sector, emissions_year) %>%
  summarize(mcf = sum(value_activity), .groups = "drop") %>%
  select(
    geog_name = county_name, geog_id,
    geog_level, sector, inventory_year = emissions_year, mcf
  )

# propane / fuel oil inventory ----
# CTU-level (residential only, from ACS-derived estimates)
ctu_propane_inventory <- read_rds(
  ghg_cprg_url("_energy/data-raw/ctu_propane_fueloil_use.RDS")
) %>%
  rename(geog_level = ctu_class, emissions_year = acs_year) %>%
  mutate(
    sector = "Residential",
    ctu_name = if_else(geog_level == "TOWNSHIP",
                       paste(ctu_name, "Twp."), ctu_name
    )
  ) %>%
  left_join(geog_index, by = join_by(ctu_name == geog_name, geog_level)) %>%
  select(
    geog_name = ctu_name, geog_id, geog_level, sector, emissions_year,
    propane_mmbtu = propane_mmBtu, fueloil_other_mmbtu = fueloil_other_mmBtu
  )

# County-level
county_propane_inventory <- read_rds(
  ghg_cprg_url("_energy/data/county_propane_fueloil_activity.RDS")
) %>%
  mutate(
    geog_level = "COUNTY",
    county_name = paste(county_name, "County")
  ) %>%
  left_join(geog_index, by = join_by(county_name == geog_name, geog_level)) %>%
  select(
    geog_name = county_name, geog_id, geog_level, sector, emissions_year,
    source, mmbtu = activity
  ) %>%
  pivot_wider(
    names_from = source,
    values_from = mmbtu,
    values_fill = 0
  ) %>%
  janitor::clean_names() %>%
  rename(fueloil_other_mmbtu = fuel_oil_other,
         propane_mmbtu = propane)

# assemble building_energy_data ----
building_energy_data <- list(
  # observed inventories (used by b_04 energy calc scripts)
  electricity_inventory = bind_rows(ctu_elec_inventory, county_elec_inventory),
  natgas_inventory      = bind_rows(ctu_gas_inventory, county_gas_inventory),
  propane_inventory     = bind_rows(ctu_propane_inventory, county_propane_inventory),

  # demographic forecasts
  residential = demographic_data %>%
    filter(sp_categories %in% c(
      "multifamily_units", "single_family_attached",
      "single_family_detached", "manufactured_homes"
    )),
  non_residential = demographic_data %>%
    filter(sp_categories %in% c("commercial_jobs", "industrial_jobs")),

  # total jobs with Imagine designation (default arg in b_01)
  jobs = demographic_data %>%
    filter(sp_categories == "jobs") %>%
    mutate(geog_name_tmp = gsub("\\s*Twp\\.", "", geog_name)) %>%
    left_join(
      cprg_ctu_desgn %>% distinct(ctu_name, ctu_class, imagine_designation),
      by = join_by(geog_name_tmp == ctu_name, geog_level == ctu_class)
    ) %>%
    select(-geog_name_tmp)
)

# save ----
usethis::use_data(building_energy_data, overwrite = TRUE)
