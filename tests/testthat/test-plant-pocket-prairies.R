testthat::test_that("plant_pocket_prairies", {
  expect_true(exists("plant_pocket_prairies"))

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

  # Helper: compute sequestration at a given year (more negative = more sequestration)
  get_sequestration <- function(result, yr) {
    result %>%
      dplyr::filter(inventory_year == yr) %>%
      dplyr::left_join(tb_seq, by = "land_cover_type") %>%
      dplyr::mutate(value_emissions = area * seq_mtco2e_sqkm) %>%
      dplyr::pull(value_emissions) %>%
      sum(na.rm = TRUE)
  }

  # ── Saint Paul (Urban) ─────────────────────────────────────────────────────

  test_that("Saint Paul pocket prairies", {
    df_null <- get_df_null("Saint Paul")
    max_yr <- max(df_null$inventory_year)
    start_yr <- 2028
    end_yr <- 2050
    mid_yr <- as.integer(round((start_yr + end_yr) / 2))

    baseline_urban_grass <- get_area_by_type(df_null, max_yr, "Urban_Grassland")

    # Zero input returns unchanged data
    result_0 <- plant_pocket_prairies(
      df_null = df_null, start_yr = start_yr, end_yr = end_yr, area_pct = 0
    )

    testthat::expect_equal(
      get_area_by_type(result_0, max_yr, "Urban_Grassland"),
      baseline_urban_grass,
      tolerance = 1e-6
    )

    # 10% adoption increases Grassland
    result_10 <- plant_pocket_prairies(
      df_null = df_null, start_yr = start_yr, end_yr = end_yr, area_pct = 10
    )

    testthat::expect_gt(
      get_area_by_type(result_10, end_yr, "Grassland"),
      get_area_by_type(df_null, end_yr, "Grassland")
    )

    # 10% adoption decreases Urban_Grassland
    testthat::expect_lt(
      get_area_by_type(result_10, end_yr, "Urban_Grassland"),
      baseline_urban_grass
    )

    # Sequestration improves (Grassland rate > Urban_Grassland/turfgrass rate)
    testthat::expect_lt(
      get_sequestration(result_10, end_yr),
      get_sequestration(result_0, end_yr)
    )

    # Linear ramp: area change at midpoint ≈ half of area change at end
    grassland_baseline <- get_area_by_type(df_null, end_yr, "Grassland")
    grassland_mid <- get_area_by_type(result_10, mid_yr, "Grassland")
    grassland_end <- get_area_by_type(result_10, end_yr, "Grassland")

    gain_mid <- grassland_mid - grassland_baseline
    gain_end <- grassland_end - grassland_baseline

    testthat::expect_equal(
      gain_mid / gain_end,
      (mid_yr - start_yr) / (end_yr - start_yr),
      tolerance = 0.01
    )

    # 100% adoption: Grassland gain ≈ original Urban_Grassland area
    result_100 <- plant_pocket_prairies(
      df_null = df_null, start_yr = start_yr, end_yr = end_yr, area_pct = 100
    )

    grassland_gain_100 <- get_area_by_type(result_100, end_yr, "Grassland") -
      get_area_by_type(df_null, end_yr, "Grassland")

    testthat::expect_equal(
      grassland_gain_100,
      baseline_urban_grass,
      tolerance = 1e-4
    )

    # 100% adoption: Urban_Grassland → 0
    testthat::expect_equal(
      get_area_by_type(result_100, end_yr, "Urban_Grassland"),
      0,
      tolerance = 1e-6
    )
  })

  # ── New Brighton (Suburban) ─────────────────────────────────────────────────────

  test_that("New Brighton pocket prairies", {
    df_null <- get_df_null("New Brighton")
    max_yr <- max(df_null$inventory_year)
    start_yr <- 2028
    end_yr <- 2050

    baseline_urban_grass <- get_area_by_type(df_null, max_yr, "Urban_Grassland")

    # Skip if no turfgrass
    if (baseline_urban_grass < 0.001) {
      testthat::skip("Newport has no Urban_Grassland")
    }

    result_25 <- plant_pocket_prairies(
      df_null = df_null, start_yr = start_yr, end_yr = end_yr, area_pct = 25
    )

    # Grassland increases
    testthat::expect_gt(
      get_area_by_type(result_25, end_yr, "Grassland"),
      get_area_by_type(df_null, end_yr, "Grassland")
    )

    # Urban_Grassland decreases
    testthat::expect_lt(
      get_area_by_type(result_25, end_yr, "Urban_Grassland"),
      baseline_urban_grass
    )

    # Sequestration improves
    result_0 <- plant_pocket_prairies(
      df_null = df_null, start_yr = start_yr, end_yr = end_yr, area_pct = 0
    )

    testthat::expect_lt(
      get_sequestration(result_25, end_yr),
      get_sequestration(result_0, end_yr)
    )
  })

  # ── Stillwater Twp. (Rural) ────────────────────────────────────────────────────

  test_that("Stillwater Twp. pocket prairies", {
    df_null <- get_df_null("Stillwater Twp.")
    max_yr <- max(df_null$inventory_year)
    start_yr <- 2028
    end_yr <- 2050

    baseline_urban_grass <- get_area_by_type(df_null, max_yr, "Urban_Grassland")

    if (baseline_urban_grass < 0.001) {
      # No turfgrass — function should return data unchanged
      result <- plant_pocket_prairies(
        df_null = df_null, start_yr = start_yr, end_yr = end_yr, area_pct = 50
      )

      testthat::expect_equal(
        get_area_by_type(result, end_yr, "Grassland"),
        get_area_by_type(df_null, end_yr, "Grassland"),
        tolerance = 1e-6
      )
    } else {
      # Has some turfgrass — test it converts correctly
      result <- plant_pocket_prairies(
        df_null = df_null, start_yr = start_yr, end_yr = end_yr, area_pct = 50
      )

      testthat::expect_gt(
        get_area_by_type(result, end_yr, "Grassland"),
        get_area_by_type(df_null, end_yr, "Grassland")
      )
    }
  })
})
