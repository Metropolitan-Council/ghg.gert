pkgload::load_all()
library(tidyverse)

# debug(adj_fleet_shares)
bev95 <- run_module_transportation(
  .selected_ctu = "Afton",
  .calc_transp_ghg_embodied = TRUE,
  .bev_pct_sales = .90,
  .scenario = "bev95"
)

bev40 <- run_module_transportation(
  .selected_ctu = "Afton",
  .calc_transp_ghg_embodied = TRUE,
  .bev_pct_sales = .4,
  .hev_pct_sales = 0.2,
  .scenario = "bev40"
)

bev20 <- run_module_transportation(
  .selected_ctu = "Afton",
  .calc_transp_ghg_embodied = TRUE,
  .bev_pct_sales = .2,
  .hev_pct_sales = 0.1,
  .scenario = "bev20"
)

bev0 <- run_module_transportation(
  .selected_ctu = "Afton",
  .calc_transp_ghg_embodied = TRUE,
  .scenario = "bev0"
)

bev95$passenger_all %>%
  bind_rows(bev95$freight_all) %>%
  bind_rows(bev40$passenger_all) %>%
  bind_rows(bev40$freight_all) %>%
  bind_rows(bev20$passenger_all) %>%
  bind_rows(bev20$freight_all) %>%
  bind_rows(bev0$passenger_all) %>%
  bind_rows(bev0$freight_all) %>%
  filter(
    year %in% c("2040"),
    type == "P"
  ) %>%
  group_by(ctu, scenario, year) %>% # mode, sector
  summarise(
    emissions = sum(dir_ghg, na.rm = T),
    # ghg_embodied = sum(ghg_embodied, na.rm = T),
    .groups = "keep"
  ) %>%
  pivot_wider(names_from = scenario, values_from = emissions) %>%
  data.frame()
#>     ctu year     bev0    bev20    bev40    bev95
#> 1 Afton 2018 33.60364 33.60364 33.60364 33.60364
#> 2 Afton 2040 32.27895 32.53166 32.50399 31.40979


current_pct_sales <- transportation_data$passenger %>%
  filter(
    mode == "PLDV",
    stringr::str_detect(var, "Sales"),
    year == "2040"
  ) %>%
  pivot_wider(
    names_from = "var",
    values_from = value
  ) %>%
  mutate(
    pct_bev = BEVSales / TotSales,
    pct_alt = (BEVSales + HEVSales ) / TotSales
  ) %>%
  select(year, ctu, pct_bev, pct_alt) %>%
  head()


calc_elasticity(
  elas_list = c(rep(0, length(unique(transportation_data$passenger$year)))),
  elas = 0.8,
  num_inits = 3,
  num_yrs = length(unique(transportation_data$passenger$year)) - 3
)
