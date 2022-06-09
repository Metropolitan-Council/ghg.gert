library(tidyverse)

# values to be modified by user -----
# The estimated carbon stock by land cover surfaces in megagrams of carbon per hectare
# (1 megagram = 1 tonne = 1 metric ton) of carbon per hectare.

carbon_stock_factors <- list(
  IMPERVIOUS_STOCK_MG_C_PER_HECTARE = 33,
  GRASS_STOCK_MG_C_PER_HECTARE = 77.04,
  TREES_STOCK_MG_C_PER_HECTARE = 115,
  WATER_STOCK_MG_C_PER_HECTARE = 0,
  BARREN_STOCK_MG_C_PER_HECTARE = 5,
  FOREST_STOCK_MG_C_PER_HECTARE = 117,
  SHRUB_STOCK_MG_C_PER_HECTARE = 49,
  GRASSLAND_STOCK_MG_C_PER_HECTARE = 49,
  AGRICULTURE_STOCK_MG_C_PER_HECTARE = 41,
  WOODY_WETLAND_STOCK_MG_C_PER_HECTARE = 117,
  WETLAND_STOCK_MG_C_PER_HECTARE = 296.75,
  PARKING_LOT_STOCK_MG_C_PER_HECTARE = 33
)


usethis::use_data(carbon_stock_factors, overwrite = TRUE)



