test_that("scaled demographic_data matches Imagine 2050 forecasts at decadal years", {
  skip_if(!file.exists(file.path(here::here(), "data-raw/meta/imagine_2050_forecasts.RDS")))

  imagine_targets <- readRDS(
    file.path(here::here(), "data-raw/meta/imagine_2050_forecasts.RDS")
  )

  decadal_years <- c(2020, 2030, 2040, 2050)

  imagine_decadal <- imagine_targets %>%
    filter(emissions_year %in% decadal_years)

  household_subtypes <- c(
    "single_family_detached", "single_family_attached",
    "multifamily_units", "manufactured_homes"
  )
  job_subtypes <- c("commercial_jobs", "industrial_jobs")

  # --- Reaggregated housing subtypes match Imagine households ---
  reagg_hh <- demographic_data %>%
    filter(
      sp_categories %in% household_subtypes,
      emissions_year %in% decadal_years
    ) %>%
    group_by(geog_id, emissions_year) %>%
    summarize(reagg_value = sum(value), .groups = "drop") %>%
    inner_join(
      imagine_decadal %>% filter(sp_categories_match == "total_households"),
      by = c("geog_id", "emissions_year")
    ) %>%
    mutate(abs_diff = abs(reagg_value - imagine_value))

  expect_gt(nrow(reagg_hh), 0)
  expect_true(
    all(reagg_hh$abs_diff < 0.01),
    label = paste0(
      "Household subtypes max residual: ",
      round(max(reagg_hh$abs_diff), 4),
      " at ", reagg_hh$geog_name[which.max(reagg_hh$abs_diff)],
      " ", reagg_hh$emissions_year[which.max(reagg_hh$abs_diff)]
    )
  )

  # --- Reaggregated job subtypes match Imagine employment ---
  reagg_jobs <- demographic_data %>%
    filter(
      sp_categories %in% job_subtypes,
      emissions_year %in% decadal_years
    ) %>%
    group_by(geog_id, emissions_year) %>%
    summarize(reagg_value = sum(value), .groups = "drop") %>%
    inner_join(
      imagine_decadal %>% filter(sp_categories_match == "jobs"),
      by = c("geog_id", "emissions_year")
    ) %>%
    mutate(abs_diff = abs(reagg_value - imagine_value))

  expect_gt(nrow(reagg_jobs), 0)
  expect_true(
    all(reagg_jobs$abs_diff < 0.01),
    label = paste0(
      "Job subtypes max residual: ",
      round(max(reagg_jobs$abs_diff), 4),
      " at ", reagg_jobs$geog_name[which.max(reagg_jobs$abs_diff)],
      " ", reagg_jobs$emissions_year[which.max(reagg_jobs$abs_diff)]
    )
  )

  # --- Aggregate rows (total_households, jobs, population) match directly ---
  direct_check <- demographic_data %>%
    filter(
      sp_categories %in% c("total_households", "jobs", "population"),
      emissions_year %in% decadal_years
    ) %>%
    inner_join(
      imagine_decadal,
      by = c(
        "geog_name",
        "geog_id",
        "emissions_year",
        "sp_categories" = "sp_categories_match"
      )
    ) %>%
    mutate(abs_diff = abs(value - imagine_value))

  expect_gt(nrow(direct_check), 0)
  expect_true(
    all(direct_check$abs_diff < 0.01),
    label = paste0(
      "Direct totals max residual: ",
      round(max(direct_check$abs_diff), 4),
      " at ", direct_check$geog_name[which.max(direct_check$abs_diff)],
      " ", direct_check$emissions_year[which.max(direct_check$abs_diff)],
      " ", direct_check$sp_categories[which.max(direct_check$abs_diff)]
    )
  )

  # --- All four decadal years and all three variable types represented ---
  expect_equal(sort(unique(reagg_hh$emissions_year)), decadal_years)
  expect_equal(sort(unique(reagg_jobs$emissions_year)), decadal_years)
  expect_setequal(unique(direct_check$sp_categories), c("total_households", "jobs", "population"))
})
