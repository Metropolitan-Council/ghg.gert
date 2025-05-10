# Pull data from EIA
pkgload::load_all()
# install.packages("eia")
library(eia)


# Pull annual gas and diesel prices ------
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

# pull annual energy outlook  ----


aeo <- eia_data("aeo/2025", freq = "annual",
                facets = list(
                  history = list("PROJECTION",
                                 "HISTORIC"),
                  scenario = list("ref2025",
                                  "aeo2023ref"),
                              tableId = 50),
                data = "value")


aeo_fuel_economy <- aeo %>%
  filter(seriesName %in% c("Light-Duty Fuel Economy : Conventional Cars : Gasoline",
                           # "Light-Duty Fuel Economy : Conventional Cars : TDI Diesel",

                           # "Light-Duty Fuel Economy : Alternative-Fuel Cars : 100-Mile Electric Vehicle",
                           # "Light-Duty Fuel Economy : Alternative-Fuel Cars : 200-Mile Electric Vehicle",
                           # "Light-Duty Fuel Economy : Alternative-Fuel Cars : 300-Mile Electric Vehicle",

                           "Light-Duty Fuel Economy : Alternative-Fuel Cars : Electric-Gasoline Hybrid",

                           # "Light-Duty Fuel Economy : Alternative-Fuel Cars : Plug-in 50 Gasoline Hybrid",
                           "Light-Duty Fuel Economy : Alternative-Fuel Cars : Plug-in 20 Gasoline Hybrid",
                           "Light-Duty Fuel Economy : Conventional Light Trucks : TDI Diesel"
  ),
  scenario == "ref2025") %>%
  dplyr::arrange(seriesId, period)
