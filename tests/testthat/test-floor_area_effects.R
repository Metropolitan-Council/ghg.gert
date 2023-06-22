testthat::test_that("floor area interventions", {
  mpls_res <- building_energy_bau_data$residential %>%
    filter(ctu_name == "Minneapolis")


  # strategies
  ## residential

  floor_area_leed <- calc_floor_area_leed(
    res_tb = building_energy_bau_data$residential,
    .selected_ctu = "Minneapolis",
    .new_homes_leed_gold_pct = 0.5,
    .enviro_factors = enviro_factors
  )


  retrofit <- calc_floor_area_retrofit(
    res_tb = building_energy_bau_data$residential,
    .selected_ctu = "Minneapolis",
    .existing_home_retrofit_pct = 0.80,
    .existing_home_ultra_retrofit_pct = 0.20,
    .enviro_factors = enviro_factors
  )

  behavior_change <- calc_floor_area_behavior_change(
    res_tb = building_energy_bau_data$residential,
    .selected_ctu = "Minneapolis",
    .home_behavior_change_pct = 1.00,
    .enviro_factors = enviro_factors
  )


  purrr::map(
    list(
      floor_area_leed,
      retrofit,
      behavior_change
    ),
    function(x) {
      # ctu floor area
      testthat::expect_lte(
        x %>%
          filter(
            var %in% c("single_family_average_floor_area_sqft_ctu"),
            year == 2040
          ) %>%
          magrittr::extract2("value"),
        mpls_res %>%
          filter(
            var %in% c("single_family_average_floor_area_sqft_ctu"),
            year == 2040
          ) %>%
          magrittr::extract2("value")
      )


      testthat::expect_lte(
        x %>%
          filter(
            var %in% c("multifamily_average_floor_area_sqft_ctu"),
            year == 2040
          ) %>%
          magrittr::extract2("value"),
        mpls_res %>%
          filter(
            var %in% c("multifamily_average_floor_area_sqft_ctu"),
            year == 2040
          ) %>%
          magrittr::extract2("value")
      )

      # county floor area
      # testthat::expect_lte(
      #   x %>%
      #     filter(var %in% c("single_family_average_floor_area_sqft_county"),
      #            year == 2040) %>%
      #     magrittr::extract2("value"),
      #
      #   mpls_res %>%
      #     filter(var %in% c("single_family_average_floor_area_sqft_county"),
      #            year == 2040) %>%
      #     magrittr::extract2("value"))


      testthat::expect_lte(
        x %>%
          filter(
            var %in% c("multifamily_average_floor_area_sqft_county"),
            year == 2040
          ) %>%
          magrittr::extract2("value"),
        mpls_res %>%
          filter(
            var %in% c("multifamily_average_floor_area_sqft_county"),
            year == 2040
          ) %>%
          magrittr::extract2("value")
      )
    }
  )
})
