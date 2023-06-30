library(ghg.sp)
library(tidyverse)

landuse_test <- run_scenario_land_use() %>%
  filter(str_detect(var, "stock"),
         is.na(value))

carbon_stock_na_test <- calc_carbon_stock_per_ctu(
  tb=land_use_data,
  .selected_ctu = "all",
  .conservation_tillage_intervention = "current_conservation_tillage",
  .tree_planting_intervention = "none",
  .tree_planting_per_capita = 0.0,
  .tree_planting_per_hectare = 0,
  .parking_lot_reduction_percentage = 0,
  .enviro_factors = ghg.sp::enviro_factors,
  detail = FALSE
  )

#possible weirdness with many-to-many relationship in parking_lot_land_cover?

parking_lot_land_cover <-
  calc_parking_lot_land_cover(
    tb=land_use_data,
    .selected_ctu = "all",
    .tree_planting_intervention = "none",
    .tree_planting_per_capita = 0.0,
    .tree_planting_per_hectare = 0,
    .parking_lot_reduction_percentage = 0,
    .enviro_factors = ghg.sp::enviro_factors,
    detail = FALSE
  )

# ctus with NA for parking_lot_land_cover:
# Brooklyn Center, Fort Snelling (unorg.), Hilltop, Rogers
# ctus with NaN:
# Lake St. Croix Beach, Maple Plain, Rockford
# what's up with Credit River Twp?

tree_parking_land_cover <- calc_tree_planting_land_cover(
  tb=land_use_data,
  .selected_ctu = "all",
  .tree_planting_intervention = "none",
  .tree_planting_per_capita = 0.0,
  .tree_planting_per_hectare = 0,
  .enviro_factors = ghg.sp::enviro_factors,
  detail = FALSE
)

#all ctus listed above have NA or NaN for tree_planting_land_cover

land_cover_by_city <- calc_land_cover_by_land_use(
  tb = land_use_data,
  .selected_ctu = "all"
) %>%
  dplyr::group_by(ctu_name, year, land_cover_type) %>%
  dplyr::summarise(land_cover_hectares = sum(land_cover_land_use_hectares))

#likewise

land_cover_percentages <- calc_land_cover_percentages(
  tb = land_use_data,
  .selected_ctu = "Rockford"
)

#yep

ctu_land_use_hectares <- filter_ctu(land_use_data$ctu_land_use_hectares, "Maple Plain")
ctu_forecast <- filter_ctu(land_use_data$ctu_land_use_hectares, "Maple Plain")
ctu_land_use_2016_land_cover <- filter_ctu(land_use_data$ctu_land_use_hectares, "Andover")

#
