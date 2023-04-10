library(ghg.sp)
library(tidyverse)

bev95 <- run_scenario_transportation(.selected_ctu = "Afton",
                                     .calc_transp_ghg_embodied = TRUE,
                                     .bev_pct_sales = .95, .scenario = "bev95")
bev40 <- run_scenario_transportation(.selected_ctu = "Afton",
                                     .calc_transp_ghg_embodied = TRUE,
                                     .bev_pct_sales = .4, .scenario = "bev40")
bev20 <- run_scenario_transportation(.selected_ctu = "Afton",
                                     .calc_transp_ghg_embodied = TRUE,
                                     .bev_pct_sales = .2, .scenario = "bev20")
bev0 <- run_scenario_transportation(.selected_ctu = "Afton",
                                    .calc_transp_ghg_embodied = TRUE,
                                    .bev_pct_sales = 0, .scenario = "bev0")

bev95$passenger_all %>%
  bind_rows(bev95$freight_all) %>%
  bind_rows(bev40$passenger_all) %>%
  bind_rows(bev40$freight_all) %>%
  bind_rows(bev20$passenger_all) %>%
  bind_rows(bev20$freight_all) %>%
  bind_rows(bev0$passenger_all) %>%
  bind_rows(bev0$freight_all) %>%
  filter(year %in% c("2018", "2040")) %>%
  group_by(ctu, scenario, year) %>% # mode, sector
  summarise(emissions = sum(dir_ghg, ghg_embodied, na.rm = T), .groups = "keep") %>%
  pivot_wider(names_from = scenario, values_from = emissions) %>%
  data.frame()
#> `summarise()` has grouped output by 'ctu', 'scenario'. You can override using
#> the `.groups` argument.
#>     ctu year     bev0    bev20    bev40    bev95
#> 1 Afton 2018 33.60364 33.60364 33.60364 33.60364
#> 2 Afton 2040 32.27895 32.53166 32.50399 31.40979
