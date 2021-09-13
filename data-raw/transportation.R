## code to prepare `transportation` dataset goes here
library(tidyverse)

pass_transpo <- read_csv("data-raw/pass_transpo_dat.csv")
freight_transpo <- read_csv("data-raw/freight_transpo_dat.csv")



pass_transpo_long <- pass_transpo %>%
  group_by(mode, var, ctu) %>%
  mutate_at(4:12, as.numeric) %>%
  pivot_longer(cols = c(
    `2015`, `2018`, `2020`,
    `2025`, `2030`, `2035`,
    `2040`, `2045`, `2050`
  ), names_to = "year") %>%
  mutate(
    aeo_mode = case_when(
      mode == "PLDV" ~ "LDV",
      mode == "BU" ~ "BUS",
      mode == "BRT" ~ "BUS",
      mode == "RU" ~ "RAIL",
      mode == "RI" ~ "RAIL",
      mode == "SUT" ~ "MDT",
      mode == "CUT" ~ "HDT",
      mode == "FR" ~ "FRAIL",
      mode %in% c("MM", "AIR", "WAT") ~ "FSHIP"
    ),
    type = "P"
  ) %>%
  group_by(mode, var, ctu, year, aeo_mode, type) %>%
  # selects highest value in case of duplicate entries
  top_n(1, value) %>%
  ungroup()

# pass_transpo_long %>%
#   filter(mode == "BU",
#          ctu == "Blaine",
#          var == "TotStock") %>%
#   group_by(mode, var, ctu, year, aeo_mode, type) %>%
#   top_n(1, value)


freight_transpo_long <- freight_transpo %>%
  group_by(mode, var, ctu) %>%
  mutate_at(4:12, as.numeric) %>%
  pivot_longer(cols = c(
    `2015`, `2018`, `2020`,
    `2025`, `2030`, `2035`,
    `2040`, `2045`, `2050`
  ), names_to = "year") %>%
  mutate(
    aeo_mode = case_when(
      mode == "PLDV" ~ "LDV",
      mode == "SUT" ~ "MDT",
      mode == "CUT" ~ "HDT",
      mode == "FR" ~ "FRAIL",
      mode %in% c("MM", "AIR", "WAT") ~ "FSHIP"
    ),
    type = "F"
  )


transportation_data <- list(
  passenger = pass_transpo_long,
  freight = freight_transpo_long
)

usethis::use_data(transportation_data, overwrite = TRUE)


