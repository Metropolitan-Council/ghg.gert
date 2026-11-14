library(dplyr)
devtools::load_all(".")
source("data-raw/meta/compile_townships.R")

# township groups: natural_systems_data has no group rows, so sum member
# township land cover here (inventory and projections separately) before
# percent_area is computed below. Matched on GNIS id.
add_twp_groups <- function(df) {
  twp_group_replace(
    df,
    twp_group_sum(df,
      value_cols = intersect(c("area", "potential_wetland_area"), names(df)),
      group_cols = c("inventory_year", "land_cover_type"),
      key = "geog_id",
      .fill = list(ctu_class = TWP_GROUP_LEVEL)
    )
  )
}

lc <- bind_rows(
  add_twp_groups(natural_systems_data$inventory$ctu),
  add_twp_groups(natural_systems_data$projections$ctu),
  natural_systems_data$inventory$county,
  natural_systems_data$projections$county
)

agriculture_area <- lc %>%
  group_by(geog_name, geog_id, inventory_year) %>%
  mutate(percent_area = area / sum(area)) %>%
  ungroup() %>%
  filter(
    land_cover_type == "Cropland",
    inventory_year >= 2005
  ) %>%
  select(-potential_wetland_area)

twp_group_check_present(agriculture_area, "agriculture_area")

usethis::use_data(agriculture_area, overwrite = T)
