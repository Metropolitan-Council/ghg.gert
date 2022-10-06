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
  import_from_emissions("metro_land.land_use_2016_types")

t_scenario_parameters <-
  import_from_emissions("metro_land.scenario_parameters")

t_current_conservation_tillage_county <-
  import_from_emissions("metro_land.vw_current_conservation_tillage_county")
