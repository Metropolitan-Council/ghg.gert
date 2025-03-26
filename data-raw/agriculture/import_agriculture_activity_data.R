### load in agricultural activity data

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
    mutate(geog_class = "County") %>%
    select(inventory_year, geog_name, county_name, livestock_type,
           head_count = township_head_count, data_type)
)
