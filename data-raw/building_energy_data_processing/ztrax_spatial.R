# load required libraries
library(data.table)
library(tidyverse)
library(readxl)
library(sf)
library(fst)
library(future)
library(furrr)
library(councilR)

bldg_areas <- fst::read.fst("/Volumes/shared/CommDev/Research/ZTRAX/fst_data/20201012/ZAsmt/metro_BuildingAreas.fst") %>% as.data.table()
bldg <- fst::read.fst("/Volumes/shared/CommDev/Research/ZTRAX/fst_data/20201012/ZAsmt/metro_Building.fst") %>% as.data.table()
main <- fst::read.fst("/Volumes/shared/CommDev/Research/ZTRAX/fst_data/20201012/ZAsmt/metro_Main.fst") %>% as.data.table()

geo_info <- main[, .(
  RowID,
  PropertyCity,
  County,
  PropertyAddressLatitude,
  PropertyAddressLongitude,
  FIPS
)]

bldg_area_geo <- data.table::merge.data.table(bldg, geo_info, by = "RowID")

bldg_all <- data.table::merge.data.table(bldg_area_geo, bldg_areas, by = "RowID") %>% as.data.table()


landuse_dict <- read_xlsx("/Volumes/shared/CommDev/Research/ZTRAX/originals/ZTRAXDataDictionary/LandUse.xlsx") %>% as.data.table()

bldgarea_dict <- read_xlsx("/Volumes/shared/CommDev/Research/ZTRAX/originals/ZTRAXDataDictionary/BldgArea.xlsx") %>% as.data.table()


bldg_all <- data.table::merge.data.table(bldg_all,
  landuse_dict,
  by.x = "PropertyLandUseStndCode",
  by.y = "StndCode"
) %>% as.data.table()


bldg_res <- data.table::merge.data.table(
  bldg_all[Classification %in% c(
    "Residential",
    "Residential Income - Multi-Family"
  ), ],
  bldgarea_dict,
  by = "BuildingAreaStndCode"
)


res_small <- bldg_res[, .(
  RowID,
  PropertyCity,
  PropertyAddressLatitude,
  PropertyAddressLongitude,
  County,
  HousingType,
  BuildingAreaStndCode,
  BuildingAreaDescription,
  BuildingAreaSqFt,
  YearBuilt,
  NoOfUnits,
  PropertyCountyLandUseCode,
  PropertyCountyLandUseDescription,
  FIPS.x,
  FIPS.y
)][BuildingAreaStndCode %in% c("BAL", "BAH"), ][!HousingType %in% c(
  "NA",
  "Other housing",
  "Manufactured housing"
), ]


res_points <- res_small[, .(RowID,
  County,
  lat = PropertyAddressLatitude,
  lng = PropertyAddressLongitude
)] %>%
  filter(
    !is.na(lat),
    !is.na(lng)
  ) %>%
  sf::st_as_sf(
    coords = c("lat", "lng"),
    crs = 4326
  )

library(councilR)

ctu_geo <- councilR::fetch_ctu_geo() %>%
  st_transform(4326)

county_geo <- councilR::fetch_county_geo() %>%
  st_transform(4326)

region_geo <- summarize(ctu_geo,
  do.union = TRUE
)

future::plan(future::multisession)



tictoc::tic("County complete")
res_county <- res_points %>%
  rowwise() %>%
  mutate(group_n = sample(1:20, 1)) %>%
  group_by(County, group_n) %>%
  group_split() %>%
  furrr::future_map_dfr(
    function(x) {
      sf::st_intersection(county_geo, x)
    }
  )
tictoc::toc()

saveRDS(res_county, "data-raw/building_energy_data_processing/ztrax_spatial/res_county.RDS")


tictoc::tic("CTU complete")
res_ctu <- res_points %>%
  rowwise() %>%
  mutate(group_n = sample(1:20, 1)) %>%
  group_by(County, group_n) %>%
  group_split() %>%
  furrr::future_map(
    function(x) {
      sf::st_intersection(x, ctu_geo)
    }
  )

tictoc::toc()

saveRDS(res_ctu, "data-raw/building_energy_data_processing/ztrax_spatial/res_ctu.RDS")
