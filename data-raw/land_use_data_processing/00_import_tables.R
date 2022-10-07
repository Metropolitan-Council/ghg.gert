# import tables
## -------------------------------------------------------------------------------------------

## demographic

t_ctu_forecast <-
  import_from_emissions("metro_demographic.vw_ctu_forecast")

t_ctu_county <-
  import_from_emissions("metro_demographic.vw_ctu_county")

## land use

t_land_use_by_cover_type <-
  import_from_emissions("metro_land.vw_land_use_by_cover_type")

t_general_carbon_values <-
  import_from_emissions("metro_land.vw_general_carbon_values")

t_ctu_land_use_2016_land_cover <-
  import_from_emissions("metro_land.vw_ctu_land_use_2016_land_cover")

t_ctu_land_use_hectares <-
  import_from_emissions("metro_land.vw_ctu_land_use_hectares")

t_land_cover_types <-
  import_from_emissions("metro_land.land_cover_types")

t_land_use_2016_types <-
  import_from_emissions("metro_land.land_use_2016_types") %>%
  rename("land_use_type" = "description_2")

t_scenario_parameters <-
  import_from_emissions("metro_land.scenario_parameters")

t_current_conservation_tillage_county <-
  import_from_emissions("metro_land.vw_current_conservation_tillage_county")


# create land use dataset -------------------------------------------------

land_use_data <- c()

land_use_data$ctu_county <- t_ctu_county

land_use_data$ctu_forecast <- t_ctu_forecast

land_use_data$scenario_parameters <- t_scenario_parameters

land_use_data$ctu_land_use_hectares <- t_ctu_land_use_hectares

land_use_data$current_conservation_tillage_county <-
  t_current_conservation_tillage_county

land_use_data$land_use_types <- t_land_use_2016_types

land_use_data$land_cover_types <- t_land_cover_types

land_use_data$land_use_by_cover_type <- t_land_use_by_cover_type

usethis::use_data(land_use_data, overwrite = T)

