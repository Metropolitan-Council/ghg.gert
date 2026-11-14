pkgload::load_all()
source("data-raw/meta/compile_townships.R")

# Region = sum of CTUs only. Filtering to CTU ids (rather than excluding a
# list of county/region names) keeps township groups out of the total.
ctu_ids <- geog_index$geog_id[geog_index$geog_id_type == "ctu_gnis"]

transportation_data$passenger <-
  bind_rows(
    transportation_data$passenger %>%
      filter(geog_name != "Twin Cities Region"),
    transportation_data$passenger %>%
      filter(geog_id %in% ctu_ids) %>%
      group_by(mode, var, year, type, aeo_mode) %>%
      unique() %>%
      summarize(value = sum(value, na.rm = T) %>% round(digits = 3), .groups = "keep") %>%
      mutate(
        geog_id = "00000000",
        geog_name = "Twin Cities Region"
      ) %>%
      select(names(transportation_data$passenger))
  )


transportation_data$freight <-
  bind_rows(
    transportation_data$freight %>%
      filter(geog_name != "Twin Cities Region"),
    transportation_data$freight %>%
      filter(geog_id %in% ctu_ids) %>%
      group_by(mode, var, year, type, aeo_mode) %>%
      summarize(value = sum(value, na.rm = T) %>% round(digits = 3), .groups = "keep") %>%
      mutate(
        geog_id = "00000000",
        geog_name = "Twin Cities Region"
      ) %>%
      select(names(transportation_data$freight))
  )

# Township groups ----
# Every passenger/freight var is extensive (PMT, TMT, vehicle counts incl.
# Tot*), so a straight sum of member townships is correct. Matched on GNIS id.
for (nm in c("passenger", "freight")) {
  transportation_data[[nm]] <- twp_group_replace(
    transportation_data[[nm]],
    twp_group_sum(
      transportation_data[[nm]],
      value_cols = "value",
      group_cols = c("mode", "var", "year", "aeo_mode", "type"),
      key = "geog_id"
    )
  )
  twp_group_check_sum(transportation_data[[nm]], "value", nm, key = "geog_id")
}

usethis::use_data(transportation_data, overwrite = TRUE)
