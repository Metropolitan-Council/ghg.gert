### load in agricultural activity data and extend to 2050
devtools::load_all(".")

metro_counties <- c("Anoka", "Carver", "Dakota", "Hennepin",
                    "Ramsey", "Scott", "Washington")

# helper: extend last observed value to 2050
extend_to_2050 <- function(df, value_col, group_cols, year_col = "inventory_year") {
  max_years <- df %>%
    group_by(across(all_of(group_cols))) %>%
    summarize(max_year = max(.data[[year_col]]), .groups = "drop")

  max_values <- df %>%
    inner_join(max_years, by = group_cols) %>%
    filter(.data[[year_col]] == max_year) %>%
    select(all_of(group_cols), max_year, all_of(value_col))

  extended <- max_values %>%
    group_by(across(all_of(group_cols))) %>%
    reframe(
      !!year_col := (max_year + 1):2050,
      !!value_col := .data[[value_col]]
    )

  bind_rows(df, extended) %>%
    arrange(across(all_of(c(group_cols, year_col))))
}

# helper: geog_index lookup keyed on lowercase short name + level
gi_lookup <- geog_index %>%
  mutate(join_name = tolower(geog_short_name)) %>%
  select(join_name, geog_level, geog_name, geog_id)

inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_agriculture/data/"

# livestock ----
livestock_county <- readr::read_rds(paste0(inpath, "usda_census_data.rds"))
livestock_ctu <- readr::read_rds(paste0(inpath, "township_usda_census_data.rds"))

livestock <- bind_rows(
  livestock_county %>%
    filter(
      year >= 2005,
      county_name %in% metro_counties
    ) %>%
    mutate(geog_name = paste(county_name, "County")) %>%
    select(
      inventory_year = year, geog_name, county_name,
      livestock_type, head_count, data_type
    ),
  livestock_ctu %>%
    filter(
      inventory_year >= 2005,
      county_name %in% metro_counties
    ) %>%
    mutate(join_name = tolower(ctu_name)) %>%
    left_join(gi_lookup, by = join_by(join_name, ctu_class == geog_level)) %>%
    select(
      inventory_year, geog_name, county_name,
      livestock_type, head_count = township_head_count, data_type
    )
)

livestock_extended <- extend_to_2050(livestock,
                                     value_col = "head_count",
                                     group_cols = c("geog_name", "county_name", "livestock_type")
) %>% ungroup()

# crops ----
crops_county <- readr::read_rds(paste0(inpath, "county_crop_production.rds"))
crops_ctu <- readr::read_rds(paste0(inpath, "ctu_usda_crop_data.rds"))

crops <- bind_rows(
  crops_county %>%
    as_tibble() %>%
    filter(
      inventory_year >= 2005,
      county_name %in% metro_counties
    ) %>%
    mutate(
      geog_name = paste(county_name, "County"),
      geoid = as.numeric(geoid)
    ) %>%
    select(geoid, inventory_year, geog_name, county_name, crop_type, metric_tons),
  crops_ctu %>%
    filter(
      inventory_year >= 2005,
      county_name %in% metro_counties
    ) %>%
    mutate(join_name = tolower(ctu_name)) %>%
    left_join(gi_lookup, by = join_by(join_name, ctu_class == geog_level)) %>%
    select(
      geoid = ctu_id, inventory_year, geog_name, county_name,
      crop_type, metric_tons = ctu_metric_tons
    )
)

crops_extended <- extend_to_2050(crops,
                                 value_col = "metric_tons",
                                 group_cols = c("geoid", "geog_name", "county_name", "crop_type")
) %>% ungroup()

# fertilizer ----
fertilizer_county <- readr::read_rds(paste0(inpath, "county_fertilizer_activity.rds"))
fertilizer_ctu <- readr::read_rds(paste0(inpath, "ctu_fertilizer_activity.rds"))

fertilizer <- bind_rows(
  fertilizer_county %>%
    as_tibble() %>%
    filter(
      inventory_year >= 2005,
      county_name %in% metro_counties
    ) %>%
    mutate(
      geog_name = paste(county_name, "County"),
      geoid = as.numeric(geoid)
    ) %>%
    select(
      geoid, inventory_year, geog_name, county_name,
      fertilizer_type, metric_tons_applied
    ),
  fertilizer_ctu %>%
    filter(
      inventory_year >= 2005,
      county_name %in% metro_counties
    ) %>%
    mutate(join_name = tolower(ctu_name)) %>%
    left_join(gi_lookup, by = join_by(join_name, ctu_class == geog_level)) %>%
    select(
      geoid = ctu_id, inventory_year, geog_name, county_name,
      fertilizer_type, metric_tons_applied
    )
)

fertilizer_extended <- extend_to_2050(fertilizer,
                                      value_col = "metric_tons_applied",
                                      group_cols = c("geoid", "geog_name", "county_name", "fertilizer_type")
) %>% ungroup()

# save ----
agriculture_activity_data <- list(
  livestock = livestock_extended,
  crops = crops_extended,
  fertilizer = fertilizer_extended
)

usethis::use_data(agriculture_activity_data, overwrite = TRUE)
