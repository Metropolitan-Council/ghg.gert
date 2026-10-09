stock_diff <- function(x, baseline) {
  x$pass_tb %>%
    filter(
      mode == "PLDV",
      stringr::str_detect(var, "Stock")
    ) %>%
    pivot_wider(
      names_from = var,
      values_from = value
    ) %>%
    left_join(
      baseline$pass_tb %>%
        filter(
          mode == "PLDV",
          stringr::str_detect(var, "Stock")
        ) %>%
        pivot_wider(
          names_from = var,
          values_from = value
        ),
      by = join_by(mode, geog_id, geog_name, year, aeo_mode, type),
      suffix = c(".scen", ".baseline")
    ) %>%
    mutate(
      BEV_diff = BEVStock.baseline - BEVStock.scen,
      SI_diff = SIStock.baseline - SIStock.scen,
      CI_diff = CIStock.baseline - CIStock.scen,
      HEV_diff = HEVStock.baseline - HEVStock.scen,
      Tot_diff = TotStock.baseline - TotStock.scen
    )
}

pmt_diff <- function(x, baseline) {
  x$pass_tb %>%
    filter(
      mode == "PLDV",
      stringr::str_detect(var, "PMT")
    ) %>%
    pivot_wider(
      names_from = var,
      values_from = value
    ) %>%
    left_join(
      baseline$pass_tb %>%
        filter(
          mode == "PLDV",
          stringr::str_detect(var, "PMT")
        ) %>%
        pivot_wider(
          names_from = var,
          values_from = value
        ),
      by = join_by(mode, geog_id, geog_name, year, aeo_mode, type),
      suffix = c(".scen", ".baseline")
    ) %>%
    mutate(PMT_dfif = PMT.baseline - PMT.scen)
}

vmt_diff <- function(x, baseline) {
  x$passenger_all %>%
    filter(year == 2050) %>%
    filter(mode == "PLDV") %>%
    left_join(
      baseline$passenger_all %>%
        filter(mode == "PLDV"),
      by = join_by(mode, stock, geog_id, geog_name, year, aeo_mode, type, class, geog_level, geog_id_type),
      suffix = c(".scen", ".baseline")
    ) %>%
    mutate(
      dir_ghg_diff = round(dir_ghg.scen - dir_ghg.baseline, digits = 2),
      dir_ghg_pct_diff = dir_ghg_diff / dir_ghg.baseline,
      vmt_diff = round(vmt.scen - vmt.baseline, digits = 2),
      vmt_pct_diff = vmt_diff / vmt.baseline
    )
}

all_bau <- purrr::map(
  geog_index$geog_name,
  (function(x) {
    cli::cli_alert_info(x)
    suppressMessages(
      run_module_transportation(
        .scenario = "bau",
        .selected_ctu = x,
        pass_tb = ghg.gert::transportation_data$passenger,
        freight_tb = ghg.gert::transportation_data$freight,
        .factor_values = ghg.gert::factor_values,
        .enviro_factors = ghg.gert::enviro_factors,
        .elast = ghg.gert::elast,
        .elast_5d = ghg.gert::elast_5d,
        .fuel_economy = ghg.gert::fuel_economy
      )
    )
  })
)


run_transport_bev <- function(bev) {
  purrr::map(
    geog_index$geog_name,
    (function(x) {
      cli::cli_alert_info(x)
      suppressMessages(
        run_module_transportation(
          .scenario = paste0("bev_", bev),
          .selected_ctu = x,
          pass_tb = ghg.gert::transportation_data$passenger,
          freight_tb = ghg.gert::transportation_data$freight,
          .factor_values = ghg.gert::factor_values,
          .enviro_factors = ghg.gert::enviro_factors,
          .elast = ghg.gert::elast,
          .elast_5d = ghg.gert::elast_5d,
          .fuel_economy = ghg.gert::fuel_economy,
          .bev_pct_stock = bev
        )
      )
    })
  )
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
      ghg.gert::transportation_index$modes %>%
        dplyr::select(mode_abbrev, mode_description_1, sector, category),
      by = c("mode" = "mode_abbrev")
    ) %>%
    dplyr::mutate(emissions_year = as.numeric(year)) %>%
    dplyr::group_by(emissions_year, type, scenario, category, sector) %>%
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
    "pldv_bev" = pldv_bev
  ))
}


bev_percentages <- purrr::map(
  seq(0.01, 1, 0.05),
  run_transport_bev
)
