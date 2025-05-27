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


  test_names <- function(df){
    df_names <- names(df)

    testthat::expect_equal(
      df_names,
      c("type", "stock", "scenario",
        "geog_name", "geog_id", "year",
        "mode", "aeo_mode", "vmt", "class",
        "dir_ghg",
        "geog_level", "geog_id_type")
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
    test_names)



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


  test_names <- function(df){
    df_names <- names(df)

    testthat::expect_equal(
      df_names,
      c("type", "stock", "scenario",
        "geog_name", "geog_id", "year",
        "mode", "aeo_mode", "vmt", "class",
        "dir_ghg",
        "geog_level", "geog_id_type")
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
    test_names)

})
