# non-residential baseline
library(tidyr)
library(dplyr)
library(readr)
library(magrittr)
library(purrr)
library(ghg.ccap)
library(councilR)

# get Imagine community designations
cprg_ctu_desgn <- read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_meta/data/cprg_ctu.RDS") %>%
  sf::st_drop_geometry() %>%
  select(ctu_name, ctu_class, imagine_designation)

# import tables
building_energy_data <- c()

##### SP 1.0 used data stored in SQL server.
##### SP 2.0 will incorporate data from other sources

load("data/demographic_data.rda")


# demographic baseline
## -------------------------------------------------------------------------------------------
# building_energy_data$ztrax_sqft_summary_county <-
#   import_from_emissions("metro_energy.ztrax_sqft_summary_county")

# building_energy_data$led_industry_county <-
#   import_from_emissions("metro_demographic.vw_led_industry_county")

building_energy_data$ctu_population <-
  demographic_data %>% filter(sp_categories == "population")

building_energy_data$ctu_mfh <-
  demographic_data %>% filter(sp_categories == "multifamily_units")

building_energy_data$ctu_sf_attached <-
  demographic_data %>% filter(sp_categories == "single_family_attached")

building_energy_data$ctu_sf_detached <-
  demographic_data %>% filter(sp_categories == "single_family_detached")

building_energy_data$ctu_manufactured <-
  demographic_data %>% filter(sp_categories == "manufactured_homes")

# building_energy_data$ctu_qcew_ctu <-
#   import_from_emissions("metro_demographic.vw_qcew_ctu")
#
# building_energy_data$forecast_lu_ctu <-
#   import_from_emissions("metro_demographic.vw_forecast_lu_ctu")

# building_energy_data$ztrax_sqft_summary_ctu <-
#   import_from_emissions("metro_energy.vw_ztrax_sqft_summary_ctu")
#
# building_energy_data$ctu_county <-
#   import_from_emissions("metro_demographic.vw_ctu_county")

# demographic forecast
## -------------------------------------------------------------------------------------------
# building_energy_data$ztrax_building_sqft <-
#   import_from_emissions("metro_energy.vw_ztrax_building_sqft")

# building_energy_data$emp_forecast_industry_county <-
#   import_from_emissions("metro_demographic.vw_emp_forecast_industry_county")

# building_energy_data$ctu_forecast <-
#   import_from_emissions("metro_demographic.vw_ctu_forecast")

# building_energy_data$emp_forecast_industry_ctu <-
#   import_from_emissions("metro_demographic.vw_emp_forecast_industry_ctu")

building_energy_data$commercial_jobs <-
  demographic_data %>% filter(sp_categories == "commercial_jobs")

building_energy_data$industrial_jobs <-
  demographic_data %>% filter(sp_categories == "industrial_jobs")

building_energy_data$jobs <-
  demographic_data %>% filter(sp_categories == "jobs") %>%
  mutate(
    geog_name_tmp = gsub("\\s*Twp\\.", "", geog_name)
  ) %>%
  left_join(cprg_ctu_desgn %>% distinct(ctu_name, ctu_class, imagine_designation),
            by = join_by(geog_name_tmp == ctu_name,
                         geog_level == ctu_class)
  ) %>%
  select(-geog_name_tmp)

# residential baseline
## -------------------------------------------------------------------------------------------
building_energy_data$electricity_residential_ctu <-
  readr::read_rds(
    "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_energy/data-raw/forecast_ctu_residential_mwh.rds"
  ) %>%
  mutate(sector = "Residential",
         ctu_name = if_else(ctu_class == "TOWNSHIP",
                             paste(ctu_name, "Twp."),
                            ctu_name)) %>%
  rename(
    mwh = residential_mwh,
    geog_name = ctu_name,
    geog_level = ctu_class
  )

building_energy_data$electricity_business_ctu <-
  readr::read_rds(
    "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_energy/data-raw/forecast_ctu_business_mwh.rds"
  ) %>%
  mutate(sector = "Business",
         ctu_name = if_else(ctu_class == "TOWNSHIP",
                            paste(ctu_name, "Twp."),
                            ctu_name)) %>%
  rename(
    mwh = business_mwh,
    geog_name = ctu_name,
    geog_level = ctu_class
  ) %>%
  left_join(cprg_ctu_desgn,
            by = join_by(geog_name == ctu_name,
                         geog_level == ctu_class),
            relationship = "many-to-many"
  )


building_energy_data$natural_gas_residential_ctu <-
  readr::read_rds(
    "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_energy/data-raw/forecast_ctu_residential_mcf.rds"
  ) %>%
  mutate(sector = "Residential",
         ctu_name = if_else(ctu_class == "TOWNSHIP",
                            paste(ctu_name, "Twp."),
                            ctu_name)) %>%
  rename(
    mcf = residential_mcf,
    geog_name = ctu_name,
    geog_level = ctu_class
  )

building_energy_data$natural_gas_business_ctu <-
  readr::read_rds(
    "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_energy/data-raw/forecast_ctu_business_mcf.rds"
  ) %>%
  mutate(sector = "Business",
         ctu_name = if_else(ctu_class == "TOWNSHIP",
                            paste(ctu_name, "Twp."),
                            ctu_name)) %>%
  rename(
    mcf = business_mcf,
    geog_name = ctu_name,
    geog_level = ctu_class
  ) %>%
  left_join(cprg_ctu_desgn,
            by = join_by(geog_name == ctu_name,
                         geog_level == ctu_class),
            relationship = "many-to-many"
  )

county_elec_data <-
  readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_meta/data/cprg_county_emissions.RDS") %>%
  filter(category == "Electricity") %>%
  left_join(
    grid_emissions %>%
      select(
        emissions_year = inventory_year,
        mt_co2e_per_mwh
      ),
    by = "emissions_year"
  ) %>%
  mutate(
    mwh = value_emissions / mt_co2e_per_mwh,
    geog_level = "COUNTY",
    sector = if_else(sector == "Residential",
      "Residential",
      "Business"
    ),
    county_name = paste(county_name, "County")
  ) %>%
  group_by(county_name, geoid, geog_level, sector, emissions_year) %>%
  summarize(mwh = sum(mwh)) %>%
  select(
    geog_name = county_name,
    geog_id = geoid,
    geog_level,
    sector,
    inventory_year = emissions_year,
    mwh
  )

building_energy_data$electricity_inventory <-
  readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_energy/data/_ctu_electricity_emissions.RDS") %>%
  rename(geog_level = ctu_class) %>%
  left_join(geog_index) %>%
  select(
    geog_name,
    geog_id,
    geog_level,
    sector,
    inventory_year,
    mwh
  ) %>%
  bind_rows(county_elec_data)


county_gas_data <-
  readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_meta/data/cprg_county_emissions.RDS") %>%
  filter(category == "Building Fuel") %>%
  mutate(
    mcf = value_emissions / enviro_factors$MT_CO2E_PER_MCF_NATGAS,
    geog_level = "COUNTY",
    sector = if_else(sector == "Residential",
      "Residential",
      "Business"
    ),
    county_name = paste(county_name, "County")
  ) %>%
  group_by(county_name, geoid, geog_level, sector, emissions_year) %>%
  summarize(mcf = sum(mcf)) %>%
  select(
    geog_name = county_name,
    geog_id = geoid,
    geog_level,
    sector,
    inventory_year = emissions_year,
    mcf
  )

building_energy_data$natgas_inventory <-
  readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_energy/data/_ctu_natgas_emissions.RDS") %>%
  rename(geog_level = ctu_class) %>%
  left_join(geog_index) %>%
  select(
    geog_name,
    geog_id,
    geog_level,
    sector,
    inventory_year,
    mcf
  ) %>%
  bind_rows(county_gas_data)

## -------------------------------------------------------------------------------------------
# building_energy_data$eia_electricity_servicewide <-
#   import_from_emissions("metro_energy.vw_eia_electricity_servicewide")
#
# building_energy_data$mndoc_electricity_county <-
#   import_from_emissions("metro_energy.vw_mndoc_electricity_county")
#
# building_energy_data$intersect_landuse_utility_service_area_county <-
#   import_from_emissions("metro_energy.vw_intersect_landuse_utility_service_area_county")
#
# building_energy_data$eia_energy_consumption_state <-
#   import_from_emissions("state_energy.eia_energy_consumption_state")
#
# building_energy_data$intersect_landuse_utility_service_area_ctu <-
#   import_from_emissions("metro_energy.vw_intersect_landuse_utility_service_area_ctu")
#
# building_energy_data$state_qcew <-
#   import_from_emissions("state_demographic.vw_state_qcew")
#
# building_energy_data$county <- import_from_emissions("state_demographic.county")
#
# building_energy_data$utility_electricity_by_ctu <-
#   import_from_emissions("metro_energy.vw_utility_electricity_by_ctu")
#
# building_energy_data$nrel_energy_consumption_ctu <-
#   import_from_emissions("metro_energy.vw_nrel_energy_consumption_ctu")
#
# building_energy_data$utility_natural_gas_by_ctu <-
#   import_from_emissions("metro_energy.vw_utility_natural_gas_by_ctu")

# -------------------------------------------------------------------------

usethis::use_data(building_energy_data, overwrite = TRUE)
