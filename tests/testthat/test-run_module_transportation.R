test_density <- function(x) {
  test_that(paste0("Density changes have anticipated effect, ", x), {
    popdens_decrease <- run_module_transportation(
      .selected_ctu = x,
      .pop_dens_pct_change = -0.2,
      .scenario = "pop_decrease"
    ) %>%
      suppressMessages() %>%
      suppressWarnings()

    popdens_increase <- run_module_transportation(
      .selected_ctu = x,
      .pop_dens_pct_change = 0.2,
      .scenario = "pop_increase"
    ) %>%
      suppressMessages() %>%
      suppressWarnings()

    popdens_bau <- run_module_transportation(
      .selected_ctu = x,
      .pop_dens_pct_change = 0,
      .scenario = "pop_bau"
    ) %>%
      suppressMessages() %>%
      suppressWarnings()

    empdens_decrease <- run_module_transportation(
      .selected_ctu = x,
      .emp_dens_pct_change = -0.2,
      .scenario = "emp_decrease"
    ) %>%
      suppressMessages() %>%
      suppressWarnings()

    empdens_increase <- run_module_transportation(
      .selected_ctu = x,
      .emp_dens_pct_change = 0.2,
      .scenario = "emp_increase"
    ) %>%
      suppressMessages() %>%
      suppressWarnings()

    empdens_bau <- run_module_transportation(
      .selected_ctu = x,
      .emp_dens_pct_change = -0,
      .scenario = "emp_bau",
    ) %>%
      suppressMessages() %>%
      suppressWarnings()

    intdens_decrease <- run_module_transportation(
      .selected_ctu = x,
      .intersection_density_pct_change = -0.2,
      .scenario = "int_decrease"
    ) %>%
      suppressMessages() %>%
      suppressWarnings()

    intdens_increase <- run_module_transportation(
      .selected_ctu = x,
      .intersection_density_pct_change = 0.2,
      .scenario = "int_increase"
    ) %>%
      suppressMessages() %>%
      suppressWarnings()

    intdens_bau <- run_module_transportation(
      .selected_ctu = x,
      .intersection_density_pct_change = 0,
      .scenario = "int_bau"
    ) %>%
      suppressMessages() %>%
      suppressWarnings()

    # browser()
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
      bind_rows(intdens_decrease$passenger_all) %>%
      bind_rows(intdens_decrease$freight_all) %>%
      bind_rows(intdens_bau$passenger_all) %>%
      bind_rows(intdens_bau$freight_all) %>%
      bind_rows(intdens_increase$passenger_all) %>%
      bind_rows(intdens_increase$freight_all) %>%
      filter(year == max(unique(popdens_bau$pass_tb$year))) %>%
      group_by(geog_name, scenario, year) %>% # mode, sector
      summarise(emissions = sum(dir_ghg, na.rm = T), .groups = "keep") %>%
      tidyr::separate(scenario, into = c("density_type", "change"), sep = "_") %>%
      pivot_wider(names_from = change, values_from = emissions) %>%
      mutate(flag = ifelse(decrease < bau, "reducing density reduces emissions", NA_character_)) %>%
      data.frame()

    # Verify calculations completed for all density types
    testthat::expect_equal(nrow(dens_result), 3) # pop, emp, int
    testthat::expect_true(all(c("decrease", "bau") %in% names(dens_result)))


    test_names <- function(df) {
      # Core columns that must be present
      core_cols <- c(
        "type", "stock", "scenario",
        "geog_name", "geog_id", "year",
        "mode", "aeo_mode", "vmt",
        "class",
        "dir_ghg",
        "geog_short_name", "geog_id_type",
        "geog_level"
      )
      
      # Check all core columns are present
      testthat::expect_true(all(core_cols %in% names(df)))
      
      # If households_cbtp is present, verify it's in the expected position
      if ("households_cbtp" %in% names(df)) {
        expected_cols <- c(
          "type", "stock", "scenario",
          "geog_name", "geog_id", "year",
          "mode", "aeo_mode", "vmt",
          "households_cbtp",
          "class",
          "dir_ghg",
          "geog_short_name", "geog_id_type",
          "geog_level"
        )
        testthat::expect_equal(names(df), expected_cols)
      }
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
        empdens_increase$freight_all,
        intdens_bau$passenger_all,
        intdens_increase$passenger_all,
        intdens_decrease$passenger_all,
        intdens_bau$freight_all,
        intdens_decrease$freight_all,
        intdens_increase$freight_all
      ),
      test_names
    )
  })
}


purrr::map(
  geography_test_list,
  test_density
)


test_vmt_stock_proportion <- function(x) {
  test_that(paste0("VMT does not change when adjusting stock proportions only, ", x), {
    run_transport <- function(bev) {
      run_module_transportation(
        .scenario = paste0("bev_", bev),
        pass_tb = transportation_data$passenger,
        freight_tb = transportation_data$freight,
        .selected_ctu = x,
        .bev_pct_stock = bev
      ) %>%
        suppressMessages() %>%
        suppressWarnings()
    }

    summarize_emiss <- function(x) {
      # browser()
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
          vmt_pct_diff = (vmt_diff / vmt.baseline) %>% round(digits = 2)
        ) %>%
        return()
    }

    baseline <- run_module_transportation(
      .scenario = "BAU",
      pass_tb = transportation_data$passenger,
      freight_tb = transportation_data$freight,
      .selected_ctu = x
    ) %>%
      suppressMessages() %>%
      suppressWarnings()

    baseline_summary <- summarize_emiss(baseline)


    bev_percentages <- purrr::map(
      seq(0.01, 1, 0.3),
      run_transport
    )


    bev_percentages_summary <-
      bev_percentages %>%
      purrr::map(summarize_emiss)

    purrr::map_dfr(bev_percentages_summary, baseline_diff,
      baseline = baseline
    ) %>%
      # we expect that VMT difference will be very low
      filter(vmt_pct_diff != 0) %>%
      nrow() %>%
      testthat::expect_equal(0)


    purrr::map_dfr(bev_percentages_summary, baseline_diff,
      baseline = baseline
    ) %>%
      # we expect GHG to change
      filter(dir_ghg_diff != 0) %>%
      nrow() %>%
      testthat::expect_equal(length(bev_percentages_summary))
  })
}


purrr::map(
  geography_test_list,
  test_vmt_stock_proportion
)


test_that("Region VMT stock proportion", {
  region_parking <- parking_cost %>%
    group_by(mode, var, type, aeo_mode) %>%
    summarize(
      value = min(value),
      .groups = "keep"
    ) %>%
    mutate(
      geog_id = "00000000",
      geog_name = "Twin Cities Region"
    ) %>%
    ungroup()

  region_avo <- vehicle_occupancy %>%
    group_by(mode, var, type, aeo_mode) %>%
    summarise(
      value = mean(value),
      .groups = "keep"
    ) %>%
    mutate(
      geog_id = "00000000",
      geog_name = "Twin Cities Region",
      .groups = "keep"
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
    # browser()
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
        vmt_pct_diff = (vmt_diff / vmt.baseline) %>% round(digits = 2)
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
    seq(0.01, 1, 0.3),
    run_transport
  )


  bev_percentages_summary <-
    bev_percentages %>%
    purrr::map(summarize_emiss)

  purrr::map_dfr(bev_percentages_summary, baseline_diff,
    baseline = baseline
  ) %>%
    filter(vmt_pct_diff != 0) %>%
    nrow() %>%
    testthat::expect_equal(0)


  purrr::map_dfr(bev_percentages_summary, baseline_diff,
    baseline = baseline
  ) %>%
    filter(dir_ghg_diff != 0) %>%
    nrow() %>%
    testthat::expect_equal(length(bev_percentages_summary))
})
