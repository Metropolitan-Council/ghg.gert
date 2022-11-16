## code to prepare `factors` dataset goes here

library(tidyverse)

aeo <- read_csv("data-raw/factors/aeo_factor_dat.csv") # Average Energy Outlook
cost <- read_csv("data-raw/factors/cost_factor_dat.csv")
ghg <- read_csv("data-raw/factors/ghg_factor_dat.csv")


# AEO recognizes that there is uncertainty in the macroeconomic future
# in addition to the AEO reference scenarios,
# changes forecasts on mileage


aeo_long <- aeo %>%
  group_by(AEOScen, Mode, Metric) %>%
  pivot_longer(cols = c(
    `2015`, `2018`, `2020`,
    `2025`, `2030`, `2035`,
    `2040`, `2045`, `2050`
  ), names_to = "year") %>%
  ungroup() %>%
  select(
    aeo_scen = AEOScen,
    mode = Mode,
    metric = Metric,
    year,
    value
  )



cost_long <- cost %>%
  group_by(mode, var, AV) %>%
  pivot_longer(cols = c(
    `2015`, `2018`, `2020`,
    `2025`, `2030`, `2035`,
    `2040`, `2045`, `2050`
  ), names_to = "year") %>%
  ungroup() %>%
  mutate(AV = as.logical(AV)) %>%
  select(mode,
    var,
    is_av = AV,
    year,
    value
  )


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

# waldo::compare(ghg.sp::factor_values, factor_values)

usethis::use_data(factor_values, overwrite = TRUE)
