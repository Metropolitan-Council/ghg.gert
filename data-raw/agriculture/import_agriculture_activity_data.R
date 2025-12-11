### load in agricultural activity data

library(dplyr)

inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_agriculture/data/"

livestock_county <- readr::read_rds(paste0(inpath, "usda_census_data.rds"))
livestock_ctu <-readr::read_rds(paste0(inpath, "township_usda_census_data.rds"))

### combine county and ctu into single dataframe

livestock <- dplyr::bind_rows(
  livestock_county %>%
    rename(inventory_year = year) %>%
    filter(inventory_year >= 2005) %>%
    mutate(geog_class = "County",
           geog_name = county_name) %>%
    select(inventory_year, geog_name, county_name, livestock_type,
           head_count, data_type),
  livestock_ctu %>%
    rename(geog_name = ctu_name,
           geog_class = ctu_class) %>%
    filter(inventory_year >= 2005) %>%
    select(inventory_year, geog_name, county_name, livestock_type,
           head_count = township_head_count, data_type)
)

### crop data

crops_county <- readr::read_rds(paste0(inpath, "county_crop_production.rds"))
crops_ctu <-readr::read_rds(paste0(inpath, "ctu_usda_crop_data.rds"))

### combine county and ctu into single dataframe

crops <- dplyr::bind_rows(
  crops_county %>%
    as_tibble() %>%
    filter(inventory_year >= 2005) %>%
    mutate(geog_class = "County",
           geog_name = county_name,
           geoid = as.numeric(geoid)) %>%
    select(geoid, inventory_year, geog_name, county_name, crop_type,
           metric_tons),
  crops_ctu %>%
    rename(geog_name = ctu_name,
           geog_class = ctu_class) %>%
    filter(inventory_year >= 2005) %>%
    select(geoid = ctu_id, inventory_year, geog_name, county_name, crop_type,
           metric_tons = ctu_metric_tons)
)

### fertilizer data

fertilizer_county <- readr::read_rds(paste0(inpath, "county_fertilizer_activity.rds"))
fertilizer_ctu <-readr::read_rds(paste0(inpath, "ctu_fertilizer_activity.rds"))

### combine county and ctu into single dataframe

fertilizer <- dplyr::bind_rows(
  fertilizer_county %>%
    as_tibble() %>%
    filter(inventory_year >= 2005) %>%
    mutate(geog_class = "County",
           geog_name = county_name,
           geoid = as.numeric(geoid)) %>%
    select(geoid, inventory_year, geog_name, county_name, fertilizer_type,
           metric_tons_applied),
  fertilizer_ctu %>%
    rename(geog_name = ctu_name,
           geog_class = ctu_class) %>%
    filter(inventory_year >= 2005) %>%
    select(geoid = ctu_id, inventory_year, geog_name, county_name, fertilizer_type,
          metric_tons_applied)
)



agriculture_activity_data <- list(
  livestock = livestock,
  crops = crops,
  fertilizer = fertilizer
)

usethis::use_data(agriculture_activity_data, overwrite=T)
