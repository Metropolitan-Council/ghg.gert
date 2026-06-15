#### Compare agricultural communities by satellite and land use

## read in standard council planned land use data
landuse <- readr::read_csv("./data-raw/land_use_data_processing/gen_land_use.csv") %>%
  janitor::clean_names()

ag_area <- agriculture_area %>%
  filter(inventory_year == 2020,
         geog_id %in% ctu_county_area$geog_id)

landuse_ag <- landuse %>%
  filter(year == 2020,
         land_use_description %in% c("Agriculture",
                                     "Farmstead"),
         acres != 0) %>%
  group_by(ctu_id, ctu_name) %>%
  summarize(ag_acres = sum(acres), .groups = "drop") %>%
  mutate(ctu_id = if_else(ctu_id == "00663886", "02830139", ctu_id)) #update credit river to ensure merge

# compare outcomes

combined <- full_join(
  landuse_ag %>% select(ctu_id, ctu_name, ag_acres),
  ag_area %>% select(geog_id, geog_name, area),
  by = c("ctu_id" = "geog_id")
) %>%
  mutate(
    lc_ag_acres = replace_na(area, 0) * 247.105,
    ag_acres = replace_na(ag_acres, 0),
    name = coalesce(ctu_name, geog_name)
  )

# CTUs missing from one or the other
not_in_ag_area <- combined %>% filter(is.na(area)) %>% pull(name)
not_in_landuse <- combined %>% filter(is.na(ctu_name)) %>% pull(name)

cat("In generalized land use but NOT in NLCD:\n")
print(not_in_ag_area)
cat("\nIn NLCD but NOT in generalized land use:\n")
print(not_in_landuse)

# Scatter plot
ggplot2::ggplot(combined, ggplot2::aes(x = ag_acres, y = lc_ag_acres)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  ggplot2::labs(
    x = "Generalized Land Use Ag (acres)",
    y = "Land Cover Ag (acres)",
    title = "Agricultural Acreage: Land Use vs Land Cover"
  ) +
  ggplot2::theme_minimal()

agriculture_flag <- geog_index %>%
  left_join(combined %>%
              filter(ag_acres > 0 & lc_ag_acres > 0) %>%
              mutate(has_ag = TRUE) %>%
              select(geog_id = ctu_id, has_ag),
            by = "geog_id"
  ) %>%
  mutate(has_ag = case_when(
    geog_level == "COUNTY" ~ TRUE,
    is.na(has_ag) ~ FALSE,
    TRUE ~ has_ag
  ))

usethis::use_data(agriculture_flag, overwrite = T)
