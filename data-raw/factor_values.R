## code to prepare `factors` dataset goes here

library(tidyverse)

aeo <- read_csv("data-raw/factors/aeo_factor_dat.csv") # Average Energy Outlook
cost <- read_csv("data-raw/factors/cost_factor_dat.csv")
ghg <- read_csv("data-raw/factors/ghg_factor_dat.csv")


aeo_long <- aeo %>%
  group_by(AEOScen, Mode, Metric) %>%
  pivot_longer(cols = c(
    `2015`, `2018`, `2020`,
    `2025`, `2030`, `2035`,
    `2040`, `2045`, `2050`
  ), names_to = "year") %>%
  ungroup()



cost_long <- cost %>%
  group_by(mode, var, AV) %>%
  pivot_longer(cols = c(
    `2015`, `2018`, `2020`,
    `2025`, `2030`, `2035`,
    `2040`, `2045`, `2050`
  ), names_to = "year") %>%
  ungroup()


ghg_long <- ghg %>%
  group_by(source) %>%
  pivot_longer(cols = c(
    `2015`, `2018`, `2020`,
    `2025`, `2030`, `2035`,
    `2040`, `2045`, `2050`
  ), names_to = "year") %>%
  select(-ctu) %>%
  ungroup()



factor_values <- list(
  aeo = aeo_long,
  cost = cost_long,
  ghg = ghg_long
)



usethis::use_data(factor_values, overwrite = TRUE)
