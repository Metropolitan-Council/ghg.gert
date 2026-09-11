library(dplyr)
library(ghg.gert)
library(councilR)

land_use_data <- c()

land_use_data$ctu_county <-
  import_from_emissions("metro_demographic.vw_ctu_county")

land_use_data$ctu_forecast <-
  import_from_emissions("metro_demographic.vw_ctu_forecast")

land_use_data$scenario_parameters <-
  import_from_emissions("metro_land.scenario_parameters")

land_use_data$ctu_land_use_hectares <-
  import_from_emissions("metro_land.vw_ctu_land_use_hectares")

land_use_data$current_conservation_tillage_county <-
  import_from_emissions("metro_land.vw_current_conservation_tillage_county")

land_use_data$land_use_2016_types <-
  import_from_emissions("metro_land.land_use_2016_types") %>%
  rename("land_use_type" = "description_2")

land_use_data$land_cover_types <-
  import_from_emissions("metro_land.land_cover_types")

land_use_data$land_use_by_cover_type <-
  import_from_emissions("metro_land.vw_land_use_by_cover_type")

land_use_data$ctu_land_use_2016_land_cover <-
  import_from_emissions("metro_land.vw_ctu_land_use_2016_land_cover")

usethis::use_data(land_use_data, overwrite = TRUE)
