testthat::test_that("plant_community_trees", {
  expect_true(exists("plant_community_trees"))

  # ── Setup ───────────────────────────────────────────────────────────────────

  tb_future <- dplyr::bind_rows(ghg.ccap::natural_systems_data$projections)

  get_df_null <- function(ctu_name) {
    ghg.ccap::filter_ctu(tb_future, ctu_name)
  }

  # Helper: get Urban_Tree area at a given year
  get_urban_tree_area <- function(result, yr) {
    result %>%
      dplyr::filter(inventory_year == yr, land_cover_type == "Urban_Tree") %>%
      dplyr::pull(area) %>%
      sum()
  }

  # Helper: get total area at a given year (should be conserved)
  get_total_area <- function(result, yr) {
    result %>%
      dplyr::filter(inventory_year == yr) %>%
      dplyr::pull(area) %>%
      sum()
  }

  # Helper: sum of developed class areas at a given year
  get_developed_area <- function(result, yr) {
    result %>%
      dplyr::filter(
        inventory_year == yr,
        land_cover_type %in% c("Developed_Low", "Developed_Med", "Developed_High")
      ) %>%
      dplyr::pull(area) %>%
      sum()
  }

  # Helper: compute sequestration at a given year
  get_sequestration <- function(result, yr) {
    carbon <- ghg.ccap::natural_systems_data$land_cover_carbon
    result %>%
      dplyr::filter(inventory_year == yr) %>%
      dplyr::left_join(carbon, by = "land_cover_type") %>%
      dplyr::mutate(seq = area * seq_mtco2e_sqkm) %>%
      dplyr::pull(seq) %>%
      sum(na.rm = TRUE)
  }


  # ── Minneapolis (Urban Center) ──────────────────────────────────────────────

  test_that("Minneapolis community trees", {
    df_null <- get_df_null("Minneapolis")
    max_yr <- max(df_null$inventory_year)
    baseline_total <- get_total_area(df_null, max_yr)

    # Zero trees = no change
    result_0 <- plant_community_trees(
      df_null = df_null,
      start_yr = 2028,
      end_yr = 2050,
      tree_count = 0
    )

    testthat::expect_equal(
      get_urban_tree_area(result_0, max_yr),
      get_urban_tree_area(df_null, max_yr),
      tolerance = 1e-6
    )

    # Planting trees increases Urban_Tree area
    result_5k <- plant_community_trees(
      df_null = df_null,
      start_yr = 2028,
      end_yr = 2050,
      tree_count = 5000
    )

    testthat::expect_gt(
      get_urban_tree_area(result_5k, max_yr),
      get_urban_tree_area(df_null, max_yr)
    )

    # Developed area decreases correspondingly
    testthat::expect_lt(
      get_developed_area(result_5k, max_yr),
      get_developed_area(df_null, max_yr)
    )

    # Total area is conserved
    testthat::expect_equal(
      get_total_area(result_5k, max_yr),
      baseline_total,
      tolerance = 1e-6
    )

    # More trees = more sequestration (more negative)
    result_10k <- plant_community_trees(
      df_null = df_null,
      start_yr = 2028,
      end_yr = 2050,
      tree_count = 10000
    )

    testthat::expect_lt(
      get_sequestration(result_10k, max_yr),
      get_sequestration(result_5k, max_yr)
    )

    # Monotonic: 10k trees > 5k trees in Urban_Tree area
    testthat::expect_gt(
      get_urban_tree_area(result_10k, max_yr),
      get_urban_tree_area(result_5k, max_yr)
    )

    # Linear ramp: no change before start_yr
    testthat::expect_equal(
      get_urban_tree_area(result_5k, 2027),
      get_urban_tree_area(df_null, 2027),
      tolerance = 1e-6
    )

    # Exceeding max is capped: result matches max exactly
    baseline_info <- ghg.ccap::community_tree_baseline %>%
      dplyr::filter(geog_id == unique(df_null$geog_id)[1])

    result_over <- plant_community_trees(
      df_null = df_null,
      start_yr = 2028,
      end_yr = 2050,
      tree_count = baseline_info$max_plantable_trees + 100000
    )

    result_at_max <- plant_community_trees(
      df_null = df_null,
      start_yr = 2028,
      end_yr = 2050,
      tree_count = baseline_info$max_plantable_trees
    )

    testthat::expect_equal(
      get_urban_tree_area(result_over, max_yr),
      get_urban_tree_area(result_at_max, max_yr),
      tolerance = 1e-6
    )

    # Metadata attribute is attached
    info <- attr(result_5k, "tree_planting_info")
    testthat::expect_true(!is.null(info))
    testthat::expect_equal(info$tree_count, 5000)
    testthat::expect_gt(info$area_converted_sqkm, 0)
  })


  # ── Newport (Suburban) ──────────────────────────────────────────────────────

  test_that("Newport community trees", {
    df_null <- get_df_null("Newport")
    max_yr <- max(df_null$inventory_year)
    baseline_total <- get_total_area(df_null, max_yr)

    result_0 <- plant_community_trees(
      df_null = df_null,
      start_yr = 2028,
      end_yr = 2050,
      tree_count = 0
    )

    testthat::expect_equal(
      get_urban_tree_area(result_0, max_yr),
      get_urban_tree_area(df_null, max_yr),
      tolerance = 1e-6
    )

    result_1k <- plant_community_trees(
      df_null = df_null,
      start_yr = 2028,
      end_yr = 2050,
      tree_count = 1000
    )

    # Trees increase
    testthat::expect_gt(
      get_urban_tree_area(result_1k, max_yr),
      get_urban_tree_area(df_null, max_yr)
    )

    # Area conserved
    testthat::expect_equal(
      get_total_area(result_1k, max_yr),
      baseline_total,
      tolerance = 1e-6
    )

    # Sequestration improves
    testthat::expect_lt(
      get_sequestration(result_1k, max_yr),
      get_sequestration(result_0, max_yr)
    )

    # Cap works
    baseline_info <- ghg.ccap::community_tree_baseline %>%
      dplyr::filter(geog_id == unique(df_null$geog_id)[1])

    result_over <- plant_community_trees(
      df_null = df_null,
      start_yr = 2028,
      end_yr = 2050,
      tree_count = baseline_info$max_plantable_trees + 50000
    )

    result_at_max <- plant_community_trees(
      df_null = df_null,
      start_yr = 2028,
      end_yr = 2050,
      tree_count = baseline_info$max_plantable_trees
    )

    testthat::expect_equal(
      get_urban_tree_area(result_over, max_yr),
      get_urban_tree_area(result_at_max, max_yr),
      tolerance = 1e-6
    )
  })


  # ── Benton Twp. (Agricultural/Rural) ────────────────────────────────────────

  test_that("Benton Twp. community trees", {
    df_null <- get_df_null("Benton Twp.")
    max_yr <- max(df_null$inventory_year)
    baseline_total <- get_total_area(df_null, max_yr)

    # Benton Twp. may have very little developed area — test that
    # the function handles small/rural communities gracefully

    result_0 <- plant_community_trees(
      df_null = df_null,
      start_yr = 2028,
      end_yr = 2050,
      tree_count = 0
    )

    testthat::expect_equal(
      get_urban_tree_area(result_0, max_yr),
      get_urban_tree_area(df_null, max_yr),
      tolerance = 1e-6
    )

    result_100 <- plant_community_trees(
      df_null = df_null,
      start_yr = 2028,
      end_yr = 2050,
      tree_count = 100
    )

    # Even a small number of trees should increase Urban_Tree area
    # (or at least not decrease it if area is tiny)
    testthat::expect_gte(
      get_urban_tree_area(result_100, max_yr),
      get_urban_tree_area(df_null, max_yr)
    )

    # Area conserved
    testthat::expect_equal(
      get_total_area(result_100, max_yr),
      baseline_total,
      tolerance = 1e-6
    )

    # Sequestration improves (or stays same if area is negligible)
    testthat::expect_lte(
      get_sequestration(result_100, max_yr),
      get_sequestration(result_0, max_yr)
    )

    # Cap works — rural township with small plantable area
    baseline_info <- ghg.ccap::community_tree_baseline %>%
      dplyr::filter(geog_id == unique(df_null$geog_id)[1])

    if (nrow(baseline_info) == 1) {
      result_over <- plant_community_trees(
        df_null = df_null,
        start_yr = 2028,
        end_yr = 2050,
        tree_count = baseline_info$max_plantable_trees + 10000
      )

      result_at_max <- plant_community_trees(
        df_null = df_null,
        start_yr = 2028,
        end_yr = 2050,
        tree_count = baseline_info$max_plantable_trees
      )

      testthat::expect_equal(
        get_urban_tree_area(result_over, max_yr),
        get_urban_tree_area(result_at_max, max_yr),
        tolerance = 1e-6
      )
    }
  })
})
