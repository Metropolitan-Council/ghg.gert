# test-calc_housing_leed.R

test_leed <- function(ctu) {
  test_that(paste("LEED input validation -", ctu), {
    # pct > 1 should error
    expect_error(calc_housing_leed(
      res_tb = building_energy_data$residential,
      .selected_ctu = ctu,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 1.1,
      .new_mf_homes_leed_gold_pct = 1.1
    ))
  })

  test_that(paste("Zero LEED pct produces no LEED units -", ctu), {
    leed0 <- calc_housing_leed(
      res_tb = building_energy_data$residential,
      .selected_ctu = ctu,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0,
      .new_mf_homes_leed_gold_pct = 0
    )

    leed_units <- leed0 %>%
      dplyr::filter(efficiency_description == "new_leed") %>%
      dplyr::pull(efficiency_unit_value)

    expect_true(all(leed_units == 0))
  })

  test_that(paste("LEED units appear only after start year -", ctu), {
    start_year <- 2028
    leed6 <- calc_housing_leed(
      res_tb = building_energy_data$residential,
      .selected_ctu = ctu,
      .leed_start_year = start_year,
      .new_sf_homes_leed_gold_pct = 0.6,
      .new_mf_homes_leed_gold_pct = 0.6
    )

    pre_start <- leed6 %>%
      dplyr::filter(
        emissions_year < start_year,
        efficiency_description == "new_leed"
      )
    expect_true(all(pre_start$efficiency_unit_value == 0))
  })

  test_that(paste("Higher LEED pct produces more LEED units -", ctu), {
    leed4 <- calc_housing_leed(
      res_tb = building_energy_data$residential,
      .selected_ctu = ctu,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0.4,
      .new_mf_homes_leed_gold_pct = 0.4
    )

    leed8 <- calc_housing_leed(
      res_tb = building_energy_data$residential,
      .selected_ctu = ctu,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0.8,
      .new_mf_homes_leed_gold_pct = 0.8
    )

    total_4 <- leed4 %>%
      dplyr::filter(emissions_year == 2050, efficiency_description == "new_leed") %>%
      dplyr::pull(efficiency_unit_value) %>%
      sum()

    total_8 <- leed8 %>%
      dplyr::filter(emissions_year == 2050, efficiency_description == "new_leed") %>%
      dplyr::pull(efficiency_unit_value) %>%
      sum()

    if (total_4 > 0 || total_8 > 0) {
      expect_gt(total_8, total_4)
    }
  })

  test_that(paste("LEED + non-LEED units sum to total new units -", ctu), {
    leed6 <- calc_housing_leed(
      res_tb = building_energy_data$residential,
      .selected_ctu = ctu,
      .leed_start_year = 2028,
      .new_sf_homes_leed_gold_pct = 0.6,
      .new_mf_homes_leed_gold_pct = 0.6
    )

    unit_check <- leed6 %>%
      dplyr::filter(emissions_year == 2050) %>%
      dplyr::group_by(sp_categories) %>%
      dplyr::summarize(
        allocated = sum(efficiency_unit_value),
        new_units = dplyr::first(new_units),
        .groups = "drop"
      ) %>%
      dplyr::mutate(diff = abs(allocated - new_units))

    expect_true(all(unit_check$diff == 0),
                label = paste(
                  "LEED + non-LEED ≠ new_units for:",
                  paste(unit_check$sp_categories[unit_check$diff > 0], collapse = ", ")
                )
    )
  })
}

purrr::walk(geography_test_list, test_leed)
