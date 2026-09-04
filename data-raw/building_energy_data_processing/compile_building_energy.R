# import_tables-building_energy_data.R
#
# Consolidated ingestion of all building energy datasets for ghg.ccap.
# Produces package data object:
#   - building_energy_data  (list: inventories, forecasts, demographic slices)

devtools::load_all(".")
library(tidyverse)

# helper: base URL for ghg-cprg repo ----
ghg_cprg_url <- function(path) {
  paste0(
    "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/",
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
  ghg_cprg_url("_energy/data/_ctu_electricity_emissions.RDS")
) %>%
  rename(geog_level = ctu_class) %>%
  left_join(geog_index, by = join_by(ctu_name == geog_short_name, geog_level)) %>%
  select(geog_name, geog_id, geog_level, sector, emissions_year, mwh)

#County-level electricity inventory
county_elec_inventory <- read_rds(
  ghg_cprg_url("_energy/data/county_elec_activity_by_sector.RDS")
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

regional_elec_inventory <- county_elec_inventory %>%
  group_by(sector, emissions_year) %>%
  summarize(mwh = sum(mwh), .groups = "drop") %>%
  mutate(geog_name = "Twin Cities Region") %>%
  left_join(geog_index, by = "geog_name") %>%
  select(
    geog_name, geog_id,
    geog_level, sector, emissions_year, mwh
  )

# natural gas inventory ----
# CTU-level
ctu_gas_inventory <- read_rds(
  ghg_cprg_url("_energy/data/_ctu_natgas_emissions.RDS")
) %>%
  rename(geog_level = ctu_class) %>%
  left_join(geog_index, by = join_by(ctu_name == geog_short_name, geog_level)) %>%
  select(geog_name, geog_id, geog_level, sector, emissions_year, mcf)

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
    geog_level, sector, emissions_year, mcf
  )

regional_gas_inventory <- county_gas_inventory %>%
  group_by(sector, emissions_year) %>%
  summarize(mcf = sum(mcf), .groups = "drop") %>%
  mutate(geog_name = "Twin Cities Region") %>%
  left_join(geog_index, by = "geog_name") %>%
  select(
    geog_name, geog_id,
    geog_level, sector, emissions_year, mcf
  )

# propane / fuel oil inventory ----
metro_counties <- c("Anoka", "Carver", "Dakota", "Hennepin",
                    "Ramsey", "Scott", "Washington")

# CTU-level (residential only, from ACS-derived estimates)
ctu_propane_inventory <- read_rds(
  ghg_cprg_url("_energy/data-raw/ctu_propane_fueloil_use.RDS")
) %>%
  filter(county_name %in% metro_counties) %>%
  rename(geog_level = ctu_class, emissions_year = acs_year) %>%
  left_join(geog_index, by = join_by(ctu_name == geog_short_name, geog_level)) %>%
  mutate(sector = "Residential") %>%
  select(
    geog_name, geog_id, geog_level, sector, emissions_year,
    propane_mmbtu = propane_mmBtu, fueloil_other_mmbtu = fueloil_other_mmBtu
  )

# County-level
county_propane_inventory <- read_rds(
  ghg_cprg_url("_energy/data/county_propane_fueloil_activity.RDS")
) %>%
  filter(county_name %in% metro_counties) %>%
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
  rename(fueloil_other_mmbtu = fuel_oil_other)

regional_propane_inventory <- county_propane_inventory %>%
  group_by(sector, emissions_year) %>%
  summarize(propane = sum(propane),
            fueloil_other_mmbtu = sum(fueloil_other_mmbtu),
            .groups = "drop") %>%
  mutate(geog_name = "Twin Cities Region") %>%
  left_join(geog_index, by = "geog_name") %>%
  select(
    geog_name, geog_id,
    geog_level, sector, emissions_year, propane,
    fueloil_other_mmbtu
  )

# assemble building_energy_data ----
building_energy_data <- list(
  # observed inventories (used by b_04 energy calc scripts)
  electricity_inventory = bind_rows(ctu_elec_inventory, county_elec_inventory, regional_elec_inventory) %>%
    left_join(geog_index %>%
                select(geog_id,
                       imagine_designation),
                       by = "geog_id"),
  natgas_inventory      = bind_rows(ctu_gas_inventory, county_gas_inventory, regional_gas_inventory)%>%
    left_join(geog_index %>%
                select(geog_id,
                       imagine_designation),
              by = "geog_id"),
  propane_inventory     = bind_rows(ctu_propane_inventory, county_propane_inventory, regional_propane_inventory)%>%
    left_join(geog_index %>%
                select(geog_id,
                       imagine_designation),
              by = "geog_id"),

  # demographic forecasts
  residential = demographic_data %>%
    filter(sp_categories %in% c(
      "multifamily_units", "single_family_attached",
      "single_family_detached", "manufactured_homes"
    )) %>%
    left_join(geog_index %>%
                select(geog_id,
                       imagine_designation),
              by = "geog_id") %>%
    rename(emissions_year = inventory_year),

  non_residential = demographic_data %>%
    filter(sp_categories %in% c("commercial_jobs", "industrial_jobs")) %>%
    left_join(geog_index %>%
                select(geog_id,
                       imagine_designation),
              by = "geog_id") %>%
    rename(emissions_year = inventory_year),

  # total jobs with Imagine designation (default arg in b_01)
  jobs = demographic_data %>%
    filter(sp_categories == "jobs") %>%
    left_join(geog_index %>%
                select(geog_id,
                       imagine_designation),
              by = "geog_id") %>%
    rename(emissions_year = inventory_year)
)

# save ----
usethis::use_data(building_energy_data, overwrite = TRUE)
