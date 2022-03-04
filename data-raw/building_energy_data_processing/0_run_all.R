library(tidyr)
library(dplyr)
library(magrittr)
library(ghg.sp)

source("data-raw/building_energy_data_processing/bau_demographics.R")
source("data-raw/building_energy_data_processing/forecast_demographics.R")

source("data-raw/building_energy_data_processing/bau_residential.R")
source("data-raw/building_energy_data_processing/forecast_residential.R")

source("data-raw/building_energy_data_processing/bau_nonres.R")
source("data-raw/building_energy_data_processing/forecast_nonres.R")

source("data-raw/building_energy_data_processing/bau_emissions.R")

p_baseline_fin
p_forecast_fin$residential_kwh_per_floor_area_forecast

elmo_baseline_compare <- read_rds("data-raw/building_energy_data_processing/lake_elmo_p_baseline.RDS")

arsenal::comparedf(elmo_baseline_compare,
                   p_baseline_fin %>%
                     filter(ctu_name == "Lake Elmo")) %>%
  summary()
 p_baseline_fin %>%
   filter(ctu_name == "Lake Elmo")



# scenarios ? -----
kg_floor_area <- p_forecast_fin %>%
  mutate(kg_co2e_floor_area = (residential_kwh_per_floor_area_forecast *
            enviro_factors$KG_CO2E_PER_MHW_FORECAST / 1000) +
           residential_therms_per_floor_area_forecast *
           enviro_factors$KG_CO2E_PER_THERM_FORECAST) %>%
  select(year, ctu_name, kg_co2e_floor_area) %>%
  unique()


v_kg_co2e_per_floor_area <-  (
  p_forecast$residential_kwh_per_floor_area_forecast *
    v_kg_co2e_per_mwh_forecast_bau / 1000
) +
  (
    p_forecast$residential_therms_per_floor_area_forecast *
      v_kg_co2e_per_therm_forecast_bau
  )


p_forecast_fin %>%
  mutate(floor_area_sqft = residential_kwh_per_floor_area_forecast *
           0.5 * (single_family_average_floor_area_sqft_ctu -
                    multifamily_average_floor_area_sqft_ctu)) %>%
  select(ctu_name, year, floor_area_sqft) %>% View


(p_forecast$single_family_units - p_baseline$single_family_units) *
  s_percent_new_homes *
  (
    p_forecast$single_family_average_floor_area_sqft_ctu - p_forecast$multifamily_average_floor_area_sqft_county
  )
