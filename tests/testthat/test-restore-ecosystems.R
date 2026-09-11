testthat::test_that("restore_ecosystems", {
  expect_true(exists("restore_ecosystems"))
  expect_true(exists("get_restoration_potential"))

  # ── Setup ───────────────────────────────────────────────────────────────────

  tb_future <- dplyr::bind_rows(ghg.gert::natural_systems_data$projections)
  tb_seq <- ghg.gert::natural_systems_data$land_cover_carbon

  get_df_null <- function(ctu_name) {
    ghg.gert::filter_ctu(tb_future, ctu_name)
  }

  # Helper: get area for a specific land cover type at a given year
  get_area_by_type <- function(result, yr, type) {
    result %>%
      dplyr::filter(inventory_year == yr, land_cover_type == type) %>%
      dplyr::pull(area) %>%
      sum(na.rm = TRUE)
  }

  # Helper: get total area across all land cover types at a given year
  get_total_area <- function(result, yr) {
    result %>%
      dplyr::filter(inventory_year == yr) %>%
      dplyr::pull(area) %>%
      sum(na.rm = TRUE)
  }

  # Helper: compute sequestration at a given year (more negative = more sequestration)
  get_sequestration <- function(result, yr) {
    result %>%
      dplyr::filter(inventory_year == yr) %>%
      dplyr::left_join(tb_seq, by = "land_cover_type") %>%
      dplyr::mutate(value_emissions = area * seq_mtco2e_sqkm) %>%
      dplyr::pull(value_emissions) %>%
      sum(na.rm = TRUE)
  }

  # ── Robbinsdale (Urban) ─────────────────────────────────────────────────────

  test_that("Robbinsdale ecosystem restoration", {
    df_null <- get_df_null("Robbinsdale")
    max_yr <- max(df_null$inventory_year)
    start_yr <- 2028
    end_yr <- 2050
    mid_yr <- as.integer(round((start_yr + end_yr) / 2))

    pot <- get_restoration_potential(df_null)

    # No inputs returns unchanged data
    result_none <- restore_ecosystems(df_null = df_null)

    testthat::expect_identical(result_none, df_null)

    # ── Forest only ──────────────────────────────────────────────────────────

    forest_sqkm <- 0.5

    result_forest <- restore_ecosystems(
      df_null = df_null,
      forest_area_sqkm = forest_sqkm,
      start_yr = start_yr,
      end_yr = end_yr
    )

    # Tree area increases by forest_area_sqkm at end year
    tree_gain <- get_area_by_type(result_forest, end_yr, "Tree") -
      get_area_by_type(df_null, end_yr, "Tree")

    testthat::expect_equal(tree_gain, forest_sqkm, tolerance = 1e-6)

    # Sequestration improves
    testthat::expect_lt(
      get_sequestration(result_forest, end_yr),
      get_sequestration(df_null, end_yr)
    )

    # ── Grassland only ───────────────────────────────────────────────────────

    prairie_sqkm <- 0.5

    result_prairie <- restore_ecosystems(
      df_null = df_null,
      prairie_area_sqkm = prairie_sqkm,
      start_yr = start_yr,
      end_yr = end_yr
    )

    grassland_gain <- get_area_by_type(result_prairie, end_yr, "Grassland") -
      get_area_by_type(df_null, end_yr, "Grassland")

    testthat::expect_equal(grassland_gain, prairie_sqkm, tolerance = 1e-6)

    # ── Wetland only ─────────────────────────────────────────────────────────

    if (pot$wetland_potential_sqkm > 0.001) {
      result_wetland <- restore_ecosystems(
        df_null = df_null,
        restore_wetland = TRUE,
        wetland_ambition_pct = 50,
        start_yr = start_yr,
        end_yr = end_yr
      )

      wetland_gain <- get_area_by_type(result_wetland, end_yr, "Wetland") -
        get_area_by_type(df_null, end_yr, "Wetland")

      testthat::expect_gt(wetland_gain, 0)

      # Sequestration improves
      testthat::expect_lt(
        get_sequestration(result_wetland, end_yr),
        get_sequestration(df_null, end_yr)
      )
    }

    # ── Combined (all three) ─────────────────────────────────────────────────

    result_combined <- restore_ecosystems(
      df_null = df_null,
      restore_wetland = pot$wetland_potential_sqkm > 0.001,
      wetland_ambition_pct = 25,
      forest_area_sqkm = 1.0,
      prairie_area_sqkm = 0.5,
      start_yr = start_yr,
      end_yr = end_yr
    )

    testthat::expect_gt(
      get_area_by_type(result_combined, end_yr, "Tree"),
      get_area_by_type(df_null, end_yr, "Tree")
    )

    testthat::expect_gt(
      get_area_by_type(result_combined, end_yr, "Grassland"),
      get_area_by_type(df_null, end_yr, "Grassland")
    )

    testthat::expect_lt(
      get_sequestration(result_combined, end_yr),
      get_sequestration(df_null, end_yr)
    )

    # ── Linear ramp ──────────────────────────────────────────────────────────

    tree_baseline <- get_area_by_type(df_null, end_yr, "Tree")
    tree_mid <- get_area_by_type(result_forest, mid_yr, "Tree")
    tree_end <- get_area_by_type(result_forest, end_yr, "Tree")

    gain_mid <- tree_mid - tree_baseline
    gain_end <- tree_end - tree_baseline

    testthat::expect_equal(
      gain_mid / gain_end,
      (mid_yr - start_yr) / (end_yr - start_yr),
      tolerance = 0.05
    )

    # ── Soft limit warning (propose full jurisdiction area) ──────────────────

    total_area <- get_total_area(df_null, max_yr)

    result_over <- restore_ecosystems(
      df_null = df_null,
      forest_area_sqkm = total_area,
      start_yr = start_yr,
      end_yr = end_yr
    )

    warnings <- attr(result_over, "validation_warnings")
    testthat::expect_true(length(warnings) > 0)

    # ── Wetland DNR limit: 100% ambition gives full potential ────────────────

    if (pot$wetland_potential_sqkm > 0.001) {
      result_full_wetland <- restore_ecosystems(
        df_null = df_null,
        restore_wetland = TRUE,
        wetland_ambition_pct = 100,
        start_yr = start_yr,
        end_yr = end_yr
      )

      wetland_gain_full <- get_area_by_type(result_full_wetland, end_yr, "Wetland") -
        get_area_by_type(df_null, end_yr, "Wetland")

      testthat::expect_equal(
        wetland_gain_full,
        pot$wetland_potential_sqkm,
        tolerance = 1e-4
      )
    }

    # ── Wetland DNR limit: ambition > 100 errors ────────────────────────────

    testthat::expect_error(
      restore_ecosystems(
        df_null = df_null,
        restore_wetland = TRUE,
        wetland_ambition_pct = 150,
        start_yr = start_yr,
        end_yr = end_yr
      )
    )
  })

  # ── Chaska (Suburban) ─────────────────────────────────────────────────────

  test_that("Chaska ecosystem restoration", {
    df_null <- get_df_null("Chaska")
    max_yr <- max(df_null$inventory_year)
    start_yr <- 2028
    end_yr <- 2050

    pot <- get_restoration_potential(df_null)

    # Forest restoration increases Tree area
    result_forest <- restore_ecosystems(
      df_null = df_null,
      forest_area_sqkm = 0.5,
      start_yr = start_yr,
      end_yr = end_yr
    )

    tree_gain <- get_area_by_type(result_forest, end_yr, "Tree") -
      get_area_by_type(df_null, end_yr, "Tree")

    testthat::expect_equal(tree_gain, 0.5, tolerance = 1e-6)

    # Sequestration improves
    testthat::expect_lt(
      get_sequestration(result_forest, end_yr),
      get_sequestration(df_null, end_yr)
    )

    # Soft limit warning with full jurisdiction area
    total_area <- get_total_area(df_null, max_yr)

    result_over <- restore_ecosystems(
      df_null = df_null,
      forest_area_sqkm = total_area,
      start_yr = start_yr,
      end_yr = end_yr
    )

    warnings <- attr(result_over, "validation_warnings")
    testthat::expect_true(length(warnings) > 0)

    # Wetland: 100% ambition gives full DNR potential
    if (pot$wetland_potential_sqkm > 0.001) {
      result_wetland_full <- restore_ecosystems(
        df_null = df_null,
        restore_wetland = TRUE,
        wetland_ambition_pct = 100,
        start_yr = start_yr,
        end_yr = end_yr
      )

      wetland_gain <- get_area_by_type(result_wetland_full, end_yr, "Wetland") -
        get_area_by_type(df_null, end_yr, "Wetland")

      testthat::expect_equal(
        wetland_gain,
        pot$wetland_potential_sqkm,
        tolerance = 1e-4
      )
    }
  })

  # ── Hollywood Twp. (Rural) ────────────────────────────────────────────────────

  test_that("Hollywood Twp. ecosystem restoration", {
    df_null <- get_df_null("Hollywood Twp.")
    max_yr <- max(df_null$inventory_year)
    start_yr <- 2028
    end_yr <- 2050

    pot <- get_restoration_potential(df_null)

    # Small forest restoration
    result_forest <- restore_ecosystems(
      df_null = df_null,
      forest_area_sqkm = 0.1,
      start_yr = start_yr,
      end_yr = end_yr
    )

    tree_gain <- get_area_by_type(result_forest, end_yr, "Tree") -
      get_area_by_type(df_null, end_yr, "Tree")

    testthat::expect_equal(tree_gain, 0.1, tolerance = 1e-6)

    # Sequestration improves
    testthat::expect_lt(
      get_sequestration(result_forest, end_yr),
      get_sequestration(df_null, end_yr)
    )

    # Missing land cover row creation: if no Wetland rows exist,
    # restoring wetlands should still work by creating them
    has_wetland_rows <- "Wetland" %in% df_null$land_cover_type

    if (pot$wetland_potential_sqkm > 0.001) {
      result_wetland <- restore_ecosystems(
        df_null = df_null,
        restore_wetland = TRUE,
        wetland_ambition_pct = 50,
        start_yr = start_yr,
        end_yr = end_yr
      )

      # Wetland rows should exist in result regardless of whether they were in input
      testthat::expect_true("Wetland" %in% result_wetland$land_cover_type)

      testthat::expect_gt(
        get_area_by_type(result_wetland, end_yr, "Wetland"),
        0
      )
    }

    # Wetland DNR limit: ambition > 100 errors
    testthat::expect_error(
      restore_ecosystems(
        df_null = df_null,
        restore_wetland = TRUE,
        wetland_ambition_pct = 101,
        start_yr = start_yr,
        end_yr = end_yr
      )
    )
  })

  # ── get_restoration_potential ───────────────────────────────────────────────

  test_that("get_restoration_potential returns expected structure", {
    df_null <- get_df_null("Minneapolis")

    pot <- get_restoration_potential(df_null)

    testthat::expect_true(is.list(pot))
    testthat::expect_true("wetland_potential_sqkm" %in% names(pot))
    testthat::expect_true("soft_limit_sqkm" %in% names(pot))

    # Soft limit should be positive and less than total area
    total_area <- df_null %>%
      dplyr::filter(inventory_year == max(inventory_year)) %>%
      dplyr::pull(area) %>%
      sum(na.rm = TRUE)

    testthat::expect_gt(pot$soft_limit_sqkm, 0)
    testthat::expect_lt(pot$soft_limit_sqkm, total_area)

    # Wetland potential should be non-negative
    testthat::expect_gte(pot$wetland_potential_sqkm, 0)
  })
})
