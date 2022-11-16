library(tidyverse)

# values to be modified by user -----
# Megagrams of carbon sequestration per hectare per year of forest land cover.
# (1 megagram = 1 tonne = 1 metric ton) of carbon per hectare per year.

carbon_sequestration_factors <- list(
  IMPERVIOUS_SEQUEST_MG_C_PER_HECTARE_PER_YEAR = 0,
  GRASS_SEQUEST_MG_C_PER_HECTARE_PER_YEAR = -0.42,
  TREES_SEQUEST_MG_C_PER_HECTARE_PER_YEAR = -1.27,
  WATER_SEQUEST_MG_C_PER_HECTARE_PER_YEAR = 0,
  BARREN_SEQUEST_MG_C_PER_HECTARE_PER_YEAR = -0.014,
  FOREST_SEQUEST_MG_C_PER_HECTARE_PER_YEAR = -0.625,
  SHRUB_SEQUEST_MG_C_PER_HECTARE_PER_YEAR = -0.287,
  GRASSLAND_SEQUEST_MG_C_PER_HECTARE_PER_YEAR = -0.287,
  AGRICULTURE_SEQUEST_MG_C_PER_HECTARE_PER_YEAR = -0.19,
  WOODY_WETLAND_SEQUEST_MG_C_PER_HECTARE_PER_YEAR = -0.625,
  WETLAND_SEQUEST_MG_C_PER_HECTARE_PER_YEAR = -1.493,
  PARKING_LOT_SEQUEST_MG_C_PER_HECTARE_PER_YEAR = 0
)



usethis::use_data(carbon_sequestration_factors, overwrite = TRUE)
