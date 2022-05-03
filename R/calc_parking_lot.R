## ----setup, include=FALSE------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------------------------------------------
parking_lot_land_cover <-
  tree_planting_land_cover %>%
  select(
    ctu_name,
    year,
    agriculture,
    barren,
    forest,
    grass,
    grassland,
    impervious,
    parking_lot,
    shrub,
    trees,
    water,
    wetland,
    woody_wetland,
    total_area
  ) %>%
  dplyr::mutate(
    parking_lot_2 =
      case_when(year == 2016 ~ parking_lot,
                year == 2040 & scenario$scenario_description_2 == "compact_dev_with_drs"  ~
                  (parking_lot * (1 - parking_lot_reduction_percentage)),
                year == 2040 & scenario$scenario_description_2 != "compact_dev_with_drs" ~
                  parking_lot),
    decreased_parking_lot = parking_lot - parking_lot_2,
    scaling_factor = (total_area + decreased_parking_lot) / total_area,
    impervious =  impervious * scaling_factor,
    grass =  grass * scaling_factor,
    trees =  trees * scaling_factor,
    barren =  barren * scaling_factor,
    forest =  forest * scaling_factor,
    shrub = shrub * scaling_factor,
    grassland = grassland * scaling_factor,
    agriculture = agriculture * scaling_factor,
    woody_wetland = woody_wetland * scaling_factor,
    wetland = wetland * scaling_factor)
#here you could pivot longer and then take all the land covers * scaling factor, and then pivot wider again. Or maybe liz has a purrr suggestion?

