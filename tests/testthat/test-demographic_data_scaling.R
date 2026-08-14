test_that("reaggregated housing and job subcategories match Imagine 2050 totals at decadal years", {
  imagine_targets <- readRDS(
    file.path(here::here(), "data-raw/meta/imagine_2050_forecasts.RDS")
  )

  decadal_years <- c(2020, 2030, 2040, 2050)

  housing_subtypes <- c(
    "single_family_detached", "single_family_attached",
    "multifamily_units", "manufactured_homes"
  )
  job_subtypes <- c("commercial_jobs", "industrial_jobs")

  # Reaggregate housing subtypes to a total
  reagg_hh <- demographic_data %>%
    filter(
      sp_categories %in% housing_subtypes,
      emissions_year %in% decadal_years
    ) %>%
    group_by(geog_id, emissions_year) %>%
    summarize(reagg_value = sum(value), .groups = "drop")

  # Reaggregate job subtypes to a total
  reagg_jobs <- demographic_data %>%
    filter(
      sp_categories %in% job_subtypes,
      emissions_year %in% decadal_years
    ) %>%
    group_by(geog_id, emissions_year) %>%
    summarize(reagg_value = sum(value), .groups = "drop")

  # Compare reaggregated housing against Imagine household targets
  imagine_hh <- imagine_targets %>%
    filter(
      sp_categories_match == "total_households",
      emissions_year %in% decadal_years
    )

  check_hh <- reagg_hh %>%
    inner_join(imagine_hh, by = c("geog_id", "emissions_year")) %>%
    mutate(abs_diff = abs(reagg_value - imagine_value))

  expect_gt(nrow(check_hh), 0)
  expect_true(
    all(check_hh$abs_diff < 0.5),
    label = paste0(
      "Household reagg max residual: ", round(max(check_hh$abs_diff), 4),
      " at geog_id=", check_hh$geog_id[which.max(check_hh$abs_diff)],
      " year=", check_hh$emissions_year[which.max(check_hh$abs_diff)]
    )
  )

  # Compare reaggregated jobs against Imagine employment targets
  imagine_jobs <- imagine_targets %>%
    filter(
      sp_categories_match == "jobs",
      emissions_year %in% decadal_years
    )

  check_jobs <- reagg_jobs %>%
    inner_join(imagine_jobs, by = c("geog_id", "emissions_year")) %>%
    mutate(abs_diff = abs(reagg_value - imagine_value))

  expect_gt(nrow(check_jobs), 0)
  expect_true(
    all(check_jobs$abs_diff < 0.5),
    label = paste0(
      "Jobs reagg max residual: ", round(max(check_jobs$abs_diff), 4),
      " at geog_id=", check_jobs$geog_id[which.max(check_jobs$abs_diff)],
      " year=", check_jobs$emissions_year[which.max(check_jobs$abs_diff)]
    )
  )

  # Verify all four decadal years are covered in both checks
  expect_equal(sort(unique(check_hh$emissions_year)), decadal_years)
  expect_equal(sort(unique(check_jobs$emissions_year)), decadal_years)
})
