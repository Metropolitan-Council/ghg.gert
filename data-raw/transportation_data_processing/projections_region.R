pkgload::load_all()


region_parking <- parking_cost %>%
  group_by(mode, var, type, aeo_mode) %>%
  summarize(value = min(value)) %>%
  mutate(
    geog_id = "00000000",
    geog_name = "Twin Cities Region"
  ) %>%
  ungroup()

region_avo <- vehicle_occupancy %>%
  group_by(mode, var, type, aeo_mode) %>%
  summarise(value = mean(value)) %>%
  mutate(
    geog_id = "00000000",
    geog_name = "Twin Cities Region"
  ) %>%
  ungroup()


run_transport <- function(bev) {
  run_module_transportation(
    .scenario = paste0("bev_", bev),
    pass_tb = region_plus_pass,
    freight_tb = region_plus_freight,
    .selected_ctu = "Twin Cities Region",
    .parking_cost = region_parking,
    .vehicle_occupancy = region_avo,
    .bev_pct_stock = bev
  ) %>%
    suppressMessages()
}

summarize_emiss <- function(x) {
  bau_mode_year <- x$passenger_all %>%
    dplyr::bind_rows(x$freight_all) %>%
    dplyr::filter(!mode %in% c(
      "MM", "RI",
      "RU", "FR",
      "WAT", "AIR"
    )) %>%
    dplyr::left_join(
      ghg.ccap::transportation_index$modes %>%
        dplyr::select(mode_abbrev, mode_description_1, sector, category),
      by = c("mode" = "mode_abbrev")
    ) %>%
    dplyr::mutate(emissions_year = as.numeric(year)) %>%
    dplyr::group_by(emissions_year, type, scenario, geog_name, geog_id, category, sector) %>%
    dplyr::summarize(
      dir_ghg = sum(dir_ghg, na.rm = T),
      vmt = sum(vmt, na.rm = T),
      .groups = "keep"
    )



  bau_year <- x$passenger_all %>%
    dplyr::bind_rows(x$freight_all) %>%
    # filter(mode %in% c("PLDV", "SUT", "CUT")) %>%
    dplyr::filter(!mode %in% c(
      "MM", "RI",
      "RU", "FR",
      "WAT", "AIR"
    )) %>%
    dplyr::mutate(emissions_year = as.numeric(year)) %>%
    dplyr::group_by(emissions_year, scenario, geog_name, geog_id) %>%
    dplyr::summarize(
      dir_ghg = sum(dir_ghg, na.rm = T),
      vmt = sum(vmt, na.rm = T),
      .groups = "keep"
    )

  pldv_bev <- x$pass_tb %>%
    filter(
      stringr::str_detect(var, "BEVStock"),
      mode == "PLDV"
    )

  return(list(
    "bau_mode_year" = bau_mode_year,
    "pldv_bev" = pldv_bev,
    "bau_year" = bau_year
  ))
}


baseline_diff <- function(x, baseline) {
  x$bau_mode_year %>%
    # filter(emissions_year == 2050 |  emissions_year == 2030) %>%
    left_join(
      baseline_summary$bau_mode_year,
      join_by(emissions_year, type, geog_name, geog_id, category, sector),
      suffix = c(".scen", ".baseline")
    ) %>%
    mutate(
      dir_ghg_diff = round(dir_ghg.scen - dir_ghg.baseline, digits = 2),
      dir_ghg_pct_diff = dir_ghg_diff / dir_ghg.baseline,
      vmt_diff = round(vmt.scen - vmt.baseline, digits = 2),
      vmt_pct_diff = vmt_diff / vmt.baseline
    ) %>%
    return()
}

baseline_diff_total <- function(x, baseline) {
  x$bau_year %>%
    left_join(
      baseline_summary$bau_year,
      join_by(emissions_year, geog_name, geog_id),
      suffix = c(".scen", ".baseline")
    ) %>%
    mutate(
      dir_ghg_diff = round(dir_ghg.scen - dir_ghg.baseline, digits = 2),
      dir_ghg_pct_diff = dir_ghg_diff / dir_ghg.baseline,
      vmt_diff = round(vmt.scen - vmt.baseline, digits = 2),
      vmt_pct_diff = vmt_diff / vmt.baseline
    ) %>%
    return()
}


baseline <- run_module_transportation(
  .scenario = "BAU",
  pass_tb = region_plus_pass,
  freight_tb = region_plus_freight,
  .selected_ctu = "Twin Cities Region",
  .parking_cost = region_parking,
  .vehicle_occupancy = region_avo
)

baseline_summary <- summarize_emiss(baseline)


bev_percentages <- purrr::map(
  seq(0.01, 1, 0.05),
  run_transport
)


bev_percentages_summary <-
  bev_percentages %>%
  purrr::map(summarize_emiss)

purrr::map_dfr(bev_percentages_summary, baseline_diff,
  baseline = baseline
) %>%
  filter(vmt_diff != 0)


baseline_diff(
  x = bev_percentages[[12]] %>% summarize_emiss(),
  baseline = baseline
)

# summarize_emiss(baseline)
# summarize_emiss(bev_percentages[[12]])


baseline$pass_tb %>%
  filter(
    mode == "PLDV",
    stringr::str_detect(var, "Stock")
  ) %>%
  pivot_wider(
    names_from = var,
    values_from = value
  )


# 56% BEV adoption will reduce PLDV emissions by 53.4%



# assign



run_transport_vmt <- function(vmt_reduction) {
  run_module_transportation(
    .scenario = paste0("vmt_", vmt_reduction),
    pass_tb = region_plus_pass,
    freight_tb = region_plus_freight,
    .selected_ctu = "Twin Cities Region",
    .parking_cost = region_parking,
    .vehicle_occupancy = region_avo,
    .vmt_reduction_pct = vmt_reduction
  ) %>%
    suppressMessages()
}



ppp <- run_module_transportation(
  .scenario = "PPP",
  pass_tb = region_plus_pass,
  freight_tb = region_plus_freight,
  .selected_ctu = "Twin Cities Region",
  .parking_cost = region_parking,
  .vehicle_occupancy = region_avo,
  .vmt_reduction_pct = 0.18,
  # .pop_dens_pct_change = 0.01,
  # .emp_dens_pct_change = 0.01,
  # .pldv_avo_pct = 0.01,
  # .pop_dens_pct_change = pop_dens,
  .bev_pct_stock = 0.56
)


ppp %>%
  summarize_emiss() %>%
  baseline_diff() %>%
  saveRDS("data-raw/transportation_data_processing/ppp_baseline_diff.RDS")
sppp %>%
  summarize_emiss() %>%
  baseline_diff_total()



net_zero <- run_module_transportation(
  .scenario = "Net Zero",
  pass_tb = region_plus_pass,
  freight_tb = region_plus_freight,
  .selected_ctu = "Twin Cities Region",
  .parking_cost = region_parking,
  .vehicle_occupancy = region_avo,
  .vmt_reduction_pct = 0.5,
  .pldv_avo_pct = 0.1,
  .pop_dens_pct_change = 0.06,
  .emp_dens_pct_change = 0.06,
  # .transit_service_pct = 0.01,
  # .pldv_avo_pct = 0.01,
  .bev_pct_stock = 1
)

net_zero %>%
  summarize_emiss() %>%
  baseline_diff_total()
net_zero %>%
  summarize_emiss() %>%
  baseline_diff()

purrr::map_dfr(vmt_percentages_summary, baseline_diff,
  baseline = baseline
) %>% View()
