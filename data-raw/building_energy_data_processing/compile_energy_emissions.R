# compile_grid_emissions.R
# Grid emission factors + stationary combustion (propane, kerosene/fuel oil)
# from EPA GHG Emission Factor Hub
devtools::load_all(".")
library(imputeTS)

# GWP values (AR6)
gwp <- list(co2 = 1, ch4 = 27.9, n2o = 273)

# load EPA GHG Factor Hub ----
epa_hub <- readr::read_rds(
  "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_meta/data/epa_ghg_factor_hub.RDS"
)

# grid emissions (eGRID + MISO projections) ----
egrid <- epa_hub$egridTimeSeries %>%
  mutate(
    emissions_year = as.numeric(Year),
    factor_source = Source,
    mt_per_mwh = as.numeric(value *
                              units::as_units("pound") %>%
                              units::set_units("metric_ton")),
    co2e = case_when(
      grepl("CH4", emission) ~ mt_per_mwh * gwp$ch4,
      grepl("N2O", emission) ~ mt_per_mwh * gwp$n2o,
      grepl("CO2", emission) ~ mt_per_mwh * gwp$co2
    )
  ) %>%
  group_by(emissions_year, factor_source) %>%
  summarize(mt_co2e_per_mwh = sum(co2e), .groups = "drop")

miso <- readr::read_csv(
  "data-raw/grid/generated_emissions_MISO_LRZ_1_2023_2043_co2_for_electricity_1y.csv",
  show_col_types = FALSE
)

miso_ef <- miso %>%
  mutate(
    emissions_year = as.numeric(substr(start_date_utc, 1, 4)),
    mt_co2e_per_mwh = as.numeric(total_co2_intensity_lbs_per_mwh *
                                   units::as_units("pound") %>%
                                   units::set_units("metric_ton")),
    factor_source = "MISO regional projections"
  ) %>%
  select(emissions_year, mt_co2e_per_mwh, factor_source) %>%
  filter(emissions_year != 2024) # 2024 appears anomalously low

grid_emissions <- bind_rows(egrid, miso_ef) %>%
  right_join(
    data.frame(emissions_year = seq(2005, 2050)),
    by = "emissions_year"
  ) %>%
  mutate(
    factor_source = if_else(
      is.na(mt_co2e_per_mwh),
      "MISO Modeled",
      factor_source
    ),
    log_co2e = log(mt_co2e_per_mwh)
  ) %>%
  arrange(emissions_year) %>%
  mutate(
    log_co2e = imputeTS::na_kalman(log_co2e),
    mt_co2e_per_mwh = exp(log_co2e)
  ) %>%
  select(-log_co2e) %>%
  # MN 100% Clean Energy Standard: zero-emission grid by 2040
  mutate(
    mt_co2e_per_mwh = if_else(emissions_year >= 2040, 0, mt_co2e_per_mwh),
    factor_source = if_else(emissions_year >= 2040, "MN 100% Clean Energy Standard", factor_source)
  )

# stationary combustion emission factors ----
# MT CO2e per inventory unit for each fuel
# natural gas: per MCF (= 1000 scf); propane & kerosene: per mmBtu
sc <- epa_hub$stationary_combustion

combustion_ef <- bind_rows(
  # natural gas — use per-scf factors × 1000 to get per-MCF
  sc %>%
    filter(`Fuel type` == "Natural Gas", per_unit == "scf") %>%
    mutate(
      mt_co2e = case_when(
        emission == "kg CO2" ~ value * 1000 / 1e3 * gwp$co2,
        emission == "g CH4"  ~ value * 1000 / 1e6 * gwp$ch4,
        emission == "g N2O"  ~ value * 1000 / 1e6 * gwp$n2o
      )
    ) %>%
    filter(!is.na(mt_co2e)) %>%
    summarize(
      fuel_type = "Natural Gas",
      mt_co2e_per_unit = sum(mt_co2e),
      unit = "mcf"
    ),
  # propane & kerosene — per mmBtu (matches inventory units)
  sc %>%
    filter(`Fuel type` %in% c("Propane", "Kerosene"), per_unit == "mmBtu") %>%
    mutate(
      mt_co2e = case_when(
        emission == "kg CO2" ~ value / 1e3 * gwp$co2,
        emission == "g CH4"  ~ value / 1e6 * gwp$ch4,
        emission == "g N2O"  ~ value / 1e6 * gwp$n2o
      )
    ) %>%
    group_by(fuel_type = `Fuel type`) %>%
    summarize(mt_co2e_per_unit = sum(mt_co2e), .groups = "drop") %>%
    mutate(unit = "mmbtu")
)

# save ----
usethis::use_data(grid_emissions, overwrite = TRUE)
usethis::use_data(combustion_ef, overwrite = TRUE)
