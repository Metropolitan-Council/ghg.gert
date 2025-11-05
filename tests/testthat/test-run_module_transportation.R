test_that("Density changes have anticipated effect, Minneapolis", {
  popdens_decrease <- run_module_transportation(
    .selected_ctu = "Minneapolis",
    .pop_dens_pct_change = -0.2,
    .scenario = "pop_decrease"
  ) %>%
    suppressMessages() %>%
    suppressWarnings()

  popdens_increase <- run_module_transportation(
    .selected_ctu = "Minneapolis",
    .pop_dens_pct_change = 0.2,
    .scenario = "pop_increase"
  ) %>%
    suppressMessages() %>%
    suppressWarnings()

  popdens_bau <- run_module_transportation(
    .selected_ctu = "Minneapolis",
    .pop_dens_pct_change = 0,
    .scenario = "pop_bau"
  ) %>%
    suppressMessages() %>%
    suppressWarnings()

  empdens_decrease <- run_module_transportation(
    .selected_ctu = "Minneapolis",
    .emp_dens_pct_change = -0.2,
    .scenario = "emp_decrease"
  ) %>%
    suppressMessages() %>%
    suppressWarnings()

  empdens_increase <- run_module_transportation(
    .selected_ctu = "Minneapolis",
    .emp_dens_pct_change = 0.2,
    .scenario = "emp_increase"
  ) %>%
    suppressMessages() %>%
    suppressWarnings()

  empdens_bau <- run_module_transportation(
    .selected_ctu = "Minneapolis",
    .emp_dens_pct_change = -0,
    .scenario = "emp_bau",
  ) %>%
    suppressMessages() %>%
    suppressWarnings()

  dens_result <- popdens_decrease$passenger_all %>%
    bind_rows(popdens_decrease$freight_all) %>%
    bind_rows(popdens_bau$passenger_all) %>%
    bind_rows(popdens_bau$freight_all) %>%
    bind_rows(popdens_increase$passenger_all) %>%
    bind_rows(popdens_increase$freight_all) %>%
    bind_rows(empdens_decrease$passenger_all) %>%
    bind_rows(empdens_decrease$freight_all) %>%
    bind_rows(empdens_bau$passenger_all) %>%
    bind_rows(empdens_bau$freight_all) %>%
    bind_rows(empdens_increase$passenger_all) %>%
    bind_rows(empdens_increase$freight_all) %>%
    filter(year == "2040") %>%
    group_by(geog_name, scenario, year) %>% # mode, sector
    summarise(emissions = sum(dir_ghg, na.rm = T), .groups = "keep") %>%
    tidyr::separate(scenario, into = c("density type", "change"), sep = "_") %>%
    pivot_wider(names_from = change, values_from = emissions) %>%
    mutate(flag = ifelse(decrease < bau, "reducing density reduces emissions", NA_character_)) %>%
    data.frame()


  testthat::expect_equal(unique(dens_result$flag), NA_character_)


  test_names <- function(df) {
    df_names <- names(df)

    testthat::expect_equal(
      df_names,
      c(
        "type", "stock", "scenario",
        "geog_name", "geog_id", "year",
        "mode", "aeo_mode", "vmt", "class",
        "dir_ghg",
        "geog_level", "geog_id_type"
      )
    )
  }

  purrr::map(
    list(
      popdens_bau$passenger_all,
      popdens_increase$passenger_all,
      popdens_decrease$passenger_all,
      popdens_bau$freight_all,
      popdens_decrease$freight_all,
      popdens_increase$freight_all,
      empdens_bau$passenger_all,
      empdens_increase$passenger_all,
      empdens_decrease$passenger_all,
      empdens_bau$freight_all,
      empdens_decrease$freight_all,
      empdens_increase$freight_all
    ),
    test_names
  )
})


test_that("Density changes have anticipated effect, Brooklyn Park", {
  popdens_decrease <- run_module_transportation(
    .selected_ctu = "Brooklyn Park",
    .pop_dens_pct_change = -0.2,
    .scenario = "pop_decrease"
  ) %>%
    suppressMessages() %>%
    suppressWarnings()

  popdens_increase <- run_module_transportation(
    .selected_ctu = "Brooklyn Park",
    .pop_dens_pct_change = 0.2,
    .scenario = "pop_increase"
  ) %>%
    suppressMessages() %>%
    suppressWarnings()

  popdens_bau <- run_module_transportation(
    .selected_ctu = "Brooklyn Park",
    .pop_dens_pct_change = 0,
    .scenario = "pop_bau"
  ) %>%
    suppressMessages() %>%
    suppressWarnings()

  empdens_decrease <- run_module_transportation(
    .selected_ctu = "Brooklyn Park",
    .emp_dens_pct_change = -0.2,
    .scenario = "emp_decrease"
  ) %>%
    suppressMessages() %>%
    suppressWarnings()

  empdens_increase <- run_module_transportation(
    .selected_ctu = "Brooklyn Park",
    .emp_dens_pct_change = 0.2,
    .scenario = "emp_increase"
  ) %>%
    suppressMessages() %>%
    suppressWarnings()

  empdens_bau <- run_module_transportation(
    .selected_ctu = "Brooklyn Park",
    .emp_dens_pct_change = -0,
    .scenario = "emp_bau",
  ) %>%
    suppressMessages() %>%
    suppressWarnings()

  dens_result <- popdens_decrease$passenger_all %>%
    bind_rows(popdens_decrease$freight_all) %>%
    bind_rows(popdens_bau$passenger_all) %>%
    bind_rows(popdens_bau$freight_all) %>%
    bind_rows(popdens_increase$passenger_all) %>%
    bind_rows(popdens_increase$freight_all) %>%
    bind_rows(empdens_decrease$passenger_all) %>%
    bind_rows(empdens_decrease$freight_all) %>%
    bind_rows(empdens_bau$passenger_all) %>%
    bind_rows(empdens_bau$freight_all) %>%
    bind_rows(empdens_increase$passenger_all) %>%
    bind_rows(empdens_increase$freight_all) %>%
    filter(year == "2040") %>%
    group_by(geog_name, scenario, year) %>% # mode, sector
    summarise(emissions = sum(dir_ghg, na.rm = T), .groups = "keep") %>%
    tidyr::separate(scenario, into = c("density type", "change"), sep = "_") %>%
    pivot_wider(names_from = change, values_from = emissions) %>%
    mutate(flag = ifelse(decrease < bau, "reducing density reduces emissions", NA_character_)) %>%
    data.frame()


  testthat::expect_equal(unique(dens_result$flag), NA_character_)


  test_names <- function(df) {
    df_names <- names(df)

    testthat::expect_equal(
      df_names,
      c(
        "type", "stock", "scenario",
        "geog_name", "geog_id", "year",
        "mode", "aeo_mode", "vmt", "class",
        "dir_ghg",
        "geog_level", "geog_id_type"
      )
    )
  }

  purrr::map(
    list(
      popdens_bau$passenger_all,
      popdens_increase$passenger_all,
      popdens_decrease$passenger_all,
      popdens_bau$freight_all,
      popdens_decrease$freight_all,
      popdens_increase$freight_all,
      empdens_bau$passenger_all,
      empdens_increase$passenger_all,
      empdens_decrease$passenger_all,
      empdens_bau$freight_all,
      empdens_decrease$freight_all,
      empdens_increase$freight_all
    ),
    test_names
  )
})


test_that("VMT does not change when adjusting stock proportions only", {
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
      pass_tb = transportation_data$passenger,
      freight_tb = transportation_data$freight,
      .selected_ctu = "Twin Cities Region",
      .parking_cost = region_parking,
      .vehicle_occupancy = region_avo,
      .bev_pct_stock = bev
    ) %>%
      suppressMessages() %>%
      suppressWarnings()
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


  baseline_diff <- function(x, baseline) {
    x$bau_mode_year %>%
      filter(emissions_year == 2050) %>%
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

  baseline <- run_module_transportation(
    .scenario = "BAU",
    pass_tb = transportation_data$passenger,
    freight_tb = transportation_data$freight,
    .selected_ctu = "Twin Cities Region",
    .parking_cost = region_parking,
    .vehicle_occupancy = region_avo
  ) %>%
    suppressMessages() %>%
    suppressWarnings()

  baseline_summary <- summarize_emiss(baseline)


  bev_percentages <- purrr::map(
    seq(0.01, 1, 0.7),
    run_transport
  )


  bev_percentages_summary <-
    bev_percentages %>%
    purrr::map(summarize_emiss)

  # TODO fix tolerance in future
  purrr::map_dfr(bev_percentages_summary, baseline_diff,
    baseline = baseline
  ) %>%
    filter(vmt_diff != 0) %>%
    nrow() %>%
    testthat::expect_equal(0, tolerance = 5)
})
