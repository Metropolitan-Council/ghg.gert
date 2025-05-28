# comparing with the values in our existing table,
# the newer versions don't significantly change the values.
epa_ghg_factor_hub <- read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/refs/heads/main/_meta/data/epa_ghg_factor_hub.RDS")

epa_ghg_factor_hub$mobile_combustion %>%
  filter(`Fuel Type` %in% c(
    # "Aviation Gasoline",
    "Biodiesel (100%)",
    "Diesel Fuel",
    "Motor Gasoline"
  )) %>%
  mutate(
    source = c("BCI", "CI", "SI"),
    ton_per_gallon = `kg CO2 per unit` %>%
      units::as_units("kilogram") %>%
      units::set_units("metric_ton") %>%
      as.numeric()
  )

epa_ghg_factor_hub$egrid %>%
  filter(year == max(year)) %>%
  mutate(
    ton_per_mwh =
      value %>%
        units::as_units("lb") %>%
        units::set_units("metric_ton")
  )
