# work with testing datasets
library(tidyverse)

bau_comp <- readRDS("../ghg.sp.tool.model/mod_3/outputs/bau_summary.RDS") %>%
  group_by(type, scenario, mode, class, ctu, output) %>%
  mutate_at(7:13, as.numeric) %>%
  pivot_longer(cols = c(
    `2015`, `2018`, `2020`,
    `2025`, `2030`, `2035`,
    `2040`
  ), names_to = "year") %>%
  group_by(type, scenario, mode, class, ctu, output, year) %>%
  tidyr::pivot_wider(
    names_from = output,
    values_from = value
  ) %>%
  mutate(
    VMT = VMT * 10^5,
    `DIR-GHG` = `DIR-GHG` * 10^5,
    `INDIR-GHG` = `INDIR-GHG` * 10^3
  )

bau_comp %>%
  filter(
    mode == "BU",
    class == "BEV"
  ) %>%
  select(1:5, ghg_embodied = `INDIR-GHG`)


## BAU passenger VMT

left_join(
  bau_comp %>%
    filter(mode == "PLDV") %>%
    unique(),
  bau_summary$passenger$PLDV$emb_ghg %>%
    select(-ghg_embodied_source) %>%
    unique(),
  by = c("type", "mode", "ctu", "year", "class")
) %>%
  mutate(indir_ghg_diff = `INDIR-GHG` - ghg_embodied) %>%
  View()






bau_comp %>%
  filter(
    mode == "AIR"
    # class == "BEV"
    # class == "BCI"
  )

bau_summary$freight$AIR_WAT_MM$vmt %>%
  filter(
    mode == "AIR",
    ctu == "St. Paul"
  ) %>%
  arrange(year) %>%
  mutate(vmt = vmt / 10^5)

bind_rows(
  bau_summary$freight$AIR_WAT_MM$vmt,
  bau_summary$freight$FRAIL$vmt,
  bau_summary$freight$SUT_CUT$vmt
)



bau_summary$passenger$PLDV$dir_ghg %>%
  filter(
    class == "BEV",
    ctu == "St. Paul",
    mode == "PLDV"
  ) %>%
  arrange(year)
# mutate(vmt = vmt / 10^5)

# MIT scenario -----


mit_comp <- readRDS("../ghg.sp.tool.model/mod_3/outputs/mit_land_summary.RDS") %>%
  group_by(type, scenario, mode, class, ctu, output) %>%
  mutate_at(7:13, as.numeric) %>%
  pivot_longer(cols = c(
    `2015`, `2018`, `2020`,
    `2025`, `2030`, `2035`,
    `2040`
  ), names_to = "year") %>%
  group_by(type, scenario, mode, class, ctu, output, year) %>%
  tidyr::pivot_wider(
    names_from = output,
    values_from = value
  )


mit_comp %>%
  filter(
    mode == "BU",
    class == "BCI"
  ) %>%
  View()

bau_summary$passenger$BU_BRT$vmt %>%
  filter(
    ctu == "St. Paul",
    mode == "BU",
    stock == "BCIStock"
  ) %>%
  arrange(year) %>%
  mutate(vmt = vmt / 10^5)


## mit AV


adj_fleet <- adj_fleet_shares(
  .bev_pct_sales = 0,
  .phev_pct_sales = 0,
  .hev_pct_sales = 0,
  .av_pct = 0.05
)

auto_veh <- scen_autonomous_vehicle(
  .pass_tb = adj_fleet$pass,
  .scenario = "MIT",
  .electric_scenario = "ER",
  .aeo_scenario = "REF",
  .drs_fuel_type = "BEV",
  .av_pct = 0.05,
  .av_fuel_type = "BEV"
)


auto_veh$vmt

mit_drs <- readRDS("../ghg.sp.tool.model/mod_3/outputs/mit_drs_summary.RDS") %>%
  group_by(type, scenario, mode, class, ctu, output) %>%
  mutate_at(7:13, as.numeric) %>%
  pivot_longer(cols = c(
    `2015`, `2018`, `2020`,
    `2025`, `2030`, `2035`,
    `2040`
  ), names_to = "year") %>%
  group_by(type, scenario, mode, class, ctu, output, year) %>%
  tidyr::pivot_wider(
    names_from = output,
    values_from = value
  )
