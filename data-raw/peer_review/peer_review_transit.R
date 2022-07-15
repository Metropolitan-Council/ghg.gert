pkgload::load_all()
library(dplyr)
library(wesanderson)
library(councilR)
library(ggplot2)
ggplot2::theme_set(
  councilR::theme_council(
    use_showtext = T,
    use_manual_font_sizes = T
  ) +
    theme(plot.caption.position = "plot")
)


st_paul_pass <- transportation_data$passenger %>%
  filter(
    ctu %in% c(
      "St. Paul",
      "All"
    ),
    !year %in% c(
      "2045",
      "2050"
    )
  )

st_paul_freight <- transportation_data$freight %>%
  filter(
    ctu %in% c(
      "St. Paul",
      "All"
    ),
    !year %in% c(
      "2045",
      "2050"
    )
  )


bau_summary <- run_scenario_transportation(
  pass_tb = st_paul_pass,
  freight_tb = st_paul_freight,
  .scenario = "BAU",
  .electric_scenario = "ER",
  .aeo_scenario = "REF"
) %>%
  suppressMessages()


# browser()
mitigation_trans <- run_scenario_transportation(
  pass_tb = st_paul_pass,
  freight_tb = st_paul_freight,
  .scenario = "strategy_improve_transit",
  .electric_scenario = "ER",
  .aeo_scenario = "REF",
  .transit_avo_pct = 0.10,
  .transit_rider_pct = 0.10,
  .pldv_avo_pct = 0.05
) %>%
  suppressMessages()


mitigation_lu <- run_scenario_transportation(
  pass_tb = st_paul_pass,
  freight_tb = st_paul_freight,
  .scenario = "strategy_land_use",
  .electric_scenario = "ER",
  .aeo_scenario = "REF",
  .pop_dens_pct_change = 0.05,
  .emp_dens_pct_change = 0.05,
  .land_use_diversity_pct_change = 0.05,
  .intersection_design_pct_change = 0.05,
  .job_access_pct_change = 0.05
  # .pldv_avo_pct = 0.05
) %>%
  suppressMessages()

# debug(vmt_transit_ridership)
mitigation_lu_transit <- run_scenario_transportation(
  pass_tb = st_paul_pass,
  freight_tb = st_paul_freight,
  .scenario = "strategy_land_use_and_transit",
  .electric_scenario = "ER",
  .aeo_scenario = "REF",
  .pop_dens_pct_change = 0.05,
  .emp_dens_pct_change = 0.05,
  .land_use_diversity_pct_change = 0.05,
  .intersection_design_pct_change = 0.05,
  .job_access_pct_change = 0.05,
  .transit_avo_pct = 0.10,
  .transit_rider_pct = 0.10,
  .pldv_avo_pct = 0.05
) %>%
  suppressMessages()

# Plots -----

all_scen_passenger_vmt <- purrr::map_dfr(
  list(
    bau_summary,
    mitigation_trans,
    mitigation_lu,
    mitigation_lu_transit
  ),
  function(x) {
    x$passenger_all %>%
      filter(mode %in% c(
        # "AV",
        "PLDV"
        # "DRS"
        # "BU", "BRT",
        # "RU", "RI",
        # "AT"
      )) %>%
      select(year, scenario, vmt) %>%
      unique() %>%
      group_by(year, scenario) %>%
      summarize(vmt = sum(vmt, na.rm = T), .groups = "keep")
  }
)

ggplot(
  all_scen_passenger_vmt,
  aes(
    x = year,
    y = vmt,
    color = scenario,
    group = scenario
  )
) +
  geom_point() +
  geom_line(
    alpha = 0.5,
    size = 1
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Region",
    subtitle = "passenger car vehicle miles traveled",
    color = ""
  )

ggsave("./data-raw/peer_review/figs/scen_run.png",
  width = 8,
  height = 6
)


all_scen_transit_vmt <- purrr::map_dfr(
  list(
    bau_summary,
    mitigation_trans,
    mitigation_lu,
    mitigation_lu_transit
  ),
  function(x) {
    x$passenger_all %>%
      filter(mode %in% c(
        # "AV",
        # "PLDV"
        # "DRS"
        "BU", "BRT",
        "RU", "RI"
        # "AT"
      )) %>%
      select(year, scenario, vmt) %>%
      unique() %>%
      group_by(year, scenario) %>%
      summarize(vmt = sum(vmt, na.rm = T) * 12, .groups = "keep")
  }
)

ggplot(
  all_scen_transit_vmt,
  aes(
    x = year,
    y = vmt,
    color = scenario,
    group = scenario
  )
) +
  geom_point() +
  geom_line(
    alpha = 0.5,
    size = 1
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Region transit vehicle miles traveled",
    color = "",
    caption = "We would expect to see VMT decrease when the transit AVO increases (more people in vehicle, more PMT) and the VMT to increase when transit ridership increases (more vehicle miles traveled)"
  )

ggsave("./data-raw/peer_review/figs/scen_run.png",
  width = 8,
  height = 6
)


all_scen_passenger_dir_ghg <- purrr::map_dfr(
  list(
    bau_summary,
    mitigation_trans,
    mitigation_lu,
    mitigation_lu_transit
  ),
  function(x) {
    x$passenger_all %>%
      filter(mode %in% c(
        "AV",
        "PLDV",
        "DRS"
        # "BU", "BRT",
        # "RU", "RI",
        # "AT"
      )) %>%
      unique() %>%
      group_by(year, scenario) %>%
      summarize(dir_ghg = sum(dir_ghg, na.rm = T), .groups = "keep")
  }
)


ggplot(
  all_scen_passenger_dir_ghg,
  aes(
    x = year,
    y = dir_ghg,
    color = scenario,
    group = scenario
  )
) +
  geom_point() +
  geom_line(
    alpha = 0.5,
    size = 1
  ) +
  labs(title = "PLDV, AV direct emissions")


## bus powertrain proportions -----

transportation_data$passenger %>%
  filter(
    mode == "PLDV",
    str_detect(var, "Exist") | str_detect(var, "Stock") | str_detect(var, "Sales")
    # var != "TotStock"
  ) %>%
  group_by(var, year) %>%
  summarize(value = sum(value)) %>%
  tidyr::pivot_wider(
    names_from = var,
    values_from = value
  ) %>%
  rowwise() %>%
  mutate(
    TotStock_new = CIStock + BEVStock + HEVStock + PHEVStock,
    TotExist_new = CIExist + BEVExist + HEVExist + PHEVExist,
    TotSales_new = CISales + BEVSales + HEVSales + PHEVSales
  ) %>%
  mutate(across(2:4, ~ . / TotStock))


transportation_data$passenger %>%
  filter(
    mode == "PLDV",
    str_detect(var, "Stock"),
    var != "TotStock"
  ) %>%
  group_by(var, year) %>%
  summarize(value = sum(value)) %>%
  ggplot(aes(
    x = year, y = value,
    color = var,
    fill = var
  )) +
  geom_col(position = "fill") +
  scale_fill_manual(
    name = "Powertrain",
    values = wesanderson::wes_palettes$Royal2,
    aesthetics = c("fill", "color"),
    labels = c(
      "BEVStock" = "Electric",
      "CIStock" = "Diesel",
      "BCIStock" = "Diesel",
      "HEVStock" = "Hybrid",
      "PHEVStock" = "Plug-in hybrid",
      "SIStock" = "Gasoline"
    )
  ) +
  scale_y_continuous(labels = scales::percent)
