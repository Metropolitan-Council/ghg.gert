## code to prepare `factors` dataset goes here

source("data-raw/transportation_data_processing/eia_datasets.R")

aeo <- read_csv("data-raw/transportation_data_processing/factors/aeo_factor_dat.csv") # Average Energy Outlook
cost <- read_csv("data-raw/transportation_data_processing/factors/cost_factor_dat.csv")
ghg <- read_csv("data-raw/transportation_data_processing/factors/ghg_factor_dat.csv")

# AEO -----
# AEO recognizes that there is uncertainty in the macroeconomic future
# in addition to the AEO reference scenarios,
# changes forecasts on mileage


aeo_factors_new <- vmt_change %>%
  ungroup() %>%
  mutate(metadata = paste0(
    "EIA Annual Energy Outlook, ",
    aeo_year,
    " ", name, " Scenario. ",
    seriesName,
    " (", seriesId, ")"
  )) %>%
  select(aeo_scen,
    mode = aeo_mode, year = period, metric = var, value = one_min_ref,
    metadata
  ) %>%
  bind_rows(
    mpg_change %>%
      filter(var %in% c("MPG", "SIMPG")) %>%
      mutate(var = "MPG") %>%
      ungroup() %>%
      mutate(metadata = paste0(
        "EIA Annual Energy Outlook, ",
        aeo_year,
        " ", name, " Scenario. ",
        seriesName,
        " (", seriesId, ")"
      )) %>%
      select(aeo_scen, mode = aeo_mode, year = period, metric = var, value = one_min_ref, metadata)
  )


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
  ) %>%
  arrange(aeo_scen, mode, metric, year)


aeo_long %>%
  filter(metric == "MPG") %>%
  select(mode, aeo_scen, metric) %>%
  unique() %>%
  nrow()


aeo_final <- aeo_long %>%
  filter(year %in% c(2015, 2018, 2020)) %>%
  # reset relative value to 1 for 2015-2020
  mutate(value = 1) %>%
  # bind rows with new dataset
  bind_rows(aeo_factors_new) %>%
  arrange(aeo_scen, mode, metric, year)

aeo_final %>%
  filter(metric == "MPG") %>%
  select(mode, aeo_scen, metric) %>%
  unique() %>%
  nrow()

# waldo::compare(aeo_final, aeo_long)

# cost -----
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


# ghg factors ------
# https://www.epa.gov/sites/default/files/2015-07/documents/emission-factors_2014.pdf
# all values were converted to metric tons

ghg_long <- ghg %>%
  group_by(source) %>%
  pivot_longer(cols = c(
    `2015`, `2018`, `2020`,
    `2025`, `2030`, `2035`,
    `2040`, `2045`, `2050`
  ), names_to = "year") %>%
  select(-ctu) %>%
  unique() %>%
  ungroup()


grid_emissions_kwh <- ghg.gert::grid_emissions %>%
  filter(emissions_year %in% ghg_long$year) %>%
  mutate(mt_co2e_per_kwh = mt_co2e_per_mwh / 1000) %>%
  mutate(
    source = "ER",
    year = as.character(emissions_year),
    value = mt_co2e_per_kwh
  ) %>%
  select(names(ghg_long))


ghg_long <- ghg_long %>%
  filter(source != "ER") %>%
  bind_rows(grid_emissions_kwh)

# finish up -----
factor_values <- list(
  aeo = aeo_final,
  cost = cost_long,
  ghg = ghg_long
)

# waldo::compare(ghg.gert::factor_values, factor_values)

usethis::use_data(factor_values, overwrite = TRUE)
