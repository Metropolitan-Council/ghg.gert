### load in agricultural activity data and extend to 2050

# Function to extend dataset to 2050
extend_to_2050 <- function(df, value_col, group_cols = "livestock_type", year_col = "year") {
  # Find max year for each group
  max_years <- df %>%
    group_by(across(all_of(group_cols))) %>%
    summarize(max_year = max(.data[[year_col]]), .groups = 'drop')

  # Get the values at max year for each group
  max_values <- df %>%
    inner_join(max_years, by = group_cols) %>%
    filter(.data[[year_col]] == max_year) %>%
    select(all_of(group_cols), max_year, all_of(value_col))

  # Create extended rows from max_year + 1 to 2050
  extended <- max_values %>%
    group_by(across(all_of(group_cols))) %>%
    reframe(
      !!year_col := (max_year + 1):2050,
      !!value_col := .data[[value_col]]
    )

  # Combine original data with extended data
  result <- bind_rows(df, extended) %>%
    arrange(across(all_of(c(group_cols, year_col))))

  return(result)
}

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

livestock_extended <- extend_to_2050(livestock,
                                      year_col = "inventory_year",
                                      value_col = "head_count",
                                      group_cols = c("geog_name",
                                                     "county_name",
                                                     "livestock_type"))

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

crops_extended <- extend_to_2050(crops,
                                     year_col = "inventory_year",
                                     value_col = "metric_tons",
                                     group_cols = c("geoid",
                                                    "geog_name",
                                                    "county_name",
                                                    "crop_type"))

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

fertilizer_extended <- extend_to_2050(fertilizer,
                                 year_col = "inventory_year",
                                 value_col = "metric_tons_applied",
                                 group_cols = c("geoid",
                                                "geog_name",
                                                "county_name",
                                                "fertilizer_type"))


agriculture_activity_data <- list(
  livestock = livestock_extended,
  crops = crops_extended,
  fertilizer = fertilizer_extended
)

usethis::use_data(agriculture_activity_data, overwrite=T)
