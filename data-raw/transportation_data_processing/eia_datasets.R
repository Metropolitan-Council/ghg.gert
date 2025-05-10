# Pull annual gas and diesel prices from EIA API
pkgload::load_all()
install.packages("eia")
library(eia)

# Minnesota specific gas
gas_prices <- eia_data("petroleum/pri/gnd",
  freq = "annual",
  data = "value",
  facets = list(series = "EMM_EPM0_PTE_SMN_DPG")
)

# Midwest specific diesel
diesel_prices <- eia_data("petroleum/pri/gnd",
  freq = "annual",
  data = "value",
  facets = list(series = "EMD_EPD2D_PTE_R20_DPG")
)

si_fuel_cost <- gas_prices %>%
  filter(period == max(period)) %>%
  dplyr::pull(value)

ci_fuel_cost <- diesel_prices %>%
  filter(period == max(period)) %>%
  dplyr::pull(value)
