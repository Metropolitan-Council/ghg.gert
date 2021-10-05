## code to prepare `transportation` dataset goes here
library(tidyverse)

pass_transpo <- read_csv("data-raw/pass_transpo_dat.csv",
  col_names = c(
    "mode", "var", "ctu",
    "2015", "2018", "2020",
    "2025", "2030", "2035",
    "2040", "2045", "2050"
  )
)
freight_transpo <- read_csv("data-raw/freight_transpo_dat.csv")


# passenger data -----
pass_transpo_long <- pass_transpo %>%
  group_by(mode, var, ctu) %>%
  mutate_at(4:12, as.numeric) %>%
  # mutate_at(4:12, replace_na, 0)
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
      mode == "BS" ~ "BUS",
      mode %in% c("MM", "AIR", "WAT") ~ "FSHIP"
    ),
    type = "P"
  ) %>%
  group_by(mode, var, ctu, year, aeo_mode, type) %>%
  # selects highest value in case of duplicate entries
  top_n(1, value) %>%
  ungroup()

ctu_year_unique <- pass_transpo_long %>%
  select(year, ctu) %>%
  filter(ctu != "All") %>%
  unique()

passenger_transpo_all <- pass_transpo_long %>%
  filter(ctu == "All") %>%
  select(-ctu) %>%
  right_join(ctu_year_unique) %>%
  select(names(pass_transpo_long))


# freight data -----
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
  ) %>%
  ungroup()



freight_transpo_all <- freight_transpo_long %>%
  filter(ctu == "All") %>%
  select(-ctu) %>%
  unique() %>%
  right_join(ctu_year_unique) %>%
  select(names(freight_transpo_long))



transportation_data <- list(
  passenger = rbind(
    pass_transpo_long %>%
      filter(ctu != "All"),
    passenger_transpo_all
  ) %>%
    unique(),
  freight = rbind(
    freight_transpo_long %>%
      filter(ctu != "All"),
    freight_transpo_all
  ) %>%
    unique()
)


testthat::expect_false("All" %in% transportation_data$passenger$ctu)
testthat::expect_false("All" %in% transportation_data$freight$ctu)

testthat::expect_equal(154296, nrow(transportation_data$passenger))
testthat::expect_equal(72144, nrow(transportation_data$freight))


usethis::use_data(transportation_data, overwrite = TRUE)
