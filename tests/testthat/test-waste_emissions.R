testthat::test_that("2020 emissions match ghg.cprg results",{
  ccap_emissions <- run_scenario_waste() %>%
    dplyr::filter(inventory_year == 2020) %>%
    dplyr::select(-ctu_name) %>%
    dplyr::arrange(ctu_id)

  inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_waste/data/"

  cprg_emissions <- readr::read_rds(paste0(inpath, "final_solid_waste_ctu_allyrs.RDS")) %>%
    dplyr::filter(inventory_year == 2020) %>%
    dplyr::mutate(ctu_id = ctuid) %>%
    dplyr::select(names(ccap_emissions)) %>%
    dplyr::arrange(ctu_id)

  # issue: ctu ids completely different

  testthat::expect_equal(ccap_emissions, cprg_emissions)
})
