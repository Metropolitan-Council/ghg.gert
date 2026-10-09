testthat::test_that("plant_community_trees", {
  expect_true(exists("plant_community_trees"))

  # ── Setup ───────────────────────────────────────────────────────────────────

  tb_future <- dplyr::bind_rows(ghg.gert::natural_systems_data$projections)

  get_df_null <- function(ctu_name) {
    ghg.gert::filter_ctu(tb_future, ctu_name)
  }

  get_urban_tree_area <- function(result, yr) {
    result %>%
      dplyr::filter(inventory_year == yr, land_cover_type == "Urban_Tree") %>%
      dplyr::pull(area) %>%
      sum()
  }

  get_sequestration <- function(result, yr) {
    carbon <- ghg.gert::natural_systems_data$land_cover_carbon
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

    # Zero trees = no change
    result_0 <- plant_community_trees(
      df_null = df_null, start_yr = 2028, end_yr = 2050, tree_count = 0
    )

    testthat::expect_equal(
      get_urban_tree_area(result_0, max_yr),
      get_urban_tree_area(df_null, max_yr),
      tolerance = 1e-6
    )

    # Planting trees increases Urban_Tree area
    result_5k <- plant_community_trees(
      df_null = df_null, start_yr = 2028, end_yr = 2050, tree_count = 5000
    )

    testthat::expect_gt(
      get_urban_tree_area(result_5k, max_yr),
      get_urban_tree_area(df_null, max_yr)
    )

    # More trees = more sequestration (more negative)
    result_10k <- plant_community_trees(
      df_null = df_null, start_yr = 2028, end_yr = 2050, tree_count = 10000
    )

    testthat::expect_lt(
      get_sequestration(result_10k, max_yr),
      get_sequestration(result_5k, max_yr)
    )

    # Monotonic: 10k > 5k in Urban_Tree area
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

    # Exceeding max throws an error
    baseline_info <- ghg.gert::community_tree_baseline %>%
      dplyr::filter(geog_id == unique(df_null$geog_id)[1])

    testthat::expect_error(
      plant_community_trees(
        df_null = df_null, start_yr = 2028, end_yr = 2050,
        tree_count = baseline_info$max_plantable_trees + 1
      ),
      "exceeds max_plantable_trees"
    )

    # At max works fine
    testthat::expect_no_error(
      plant_community_trees(
        df_null = df_null, start_yr = 2028, end_yr = 2050,
        tree_count = baseline_info$max_plantable_trees
      )
    )

    # Metadata attribute is attached
    info <- attr(result_5k, "tree_planting_info")
    testthat::expect_true(!is.null(info))
    testthat::expect_equal(info$tree_count, 5000)
    testthat::expect_gt(info$area_added_sqkm, 0)
  })


  # ── Newport (Suburban) ──────────────────────────────────────────────────────

  test_that("Newport community trees", {
    df_null <- get_df_null("Newport")
    max_yr <- max(df_null$inventory_year)

    # Zero = no change
    result_0 <- plant_community_trees(
      df_null = df_null, start_yr = 2028, end_yr = 2050, tree_count = 0
    )

    testthat::expect_equal(
      get_urban_tree_area(result_0, max_yr),
      get_urban_tree_area(df_null, max_yr),
      tolerance = 1e-6
    )

    # Planting increases Urban_Tree
    result_1k <- plant_community_trees(
      df_null = df_null, start_yr = 2028, end_yr = 2050, tree_count = 1000
    )

    testthat::expect_gt(
      get_urban_tree_area(result_1k, max_yr),
      get_urban_tree_area(df_null, max_yr)
    )

    # Sequestration improves
    testthat::expect_lt(
      get_sequestration(result_1k, max_yr),
      get_sequestration(result_0, max_yr)
    )

    # Exceeding max throws an error
    baseline_info <- ghg.gert::community_tree_baseline %>%
      dplyr::filter(geog_id == unique(df_null$geog_id)[1])

    testthat::expect_error(
      plant_community_trees(
        df_null = df_null, start_yr = 2028, end_yr = 2050,
        tree_count = baseline_info$max_plantable_trees + 1
      ),
      "exceeds max_plantable_trees"
    )
  })


  # ── Benton Twp. (Agricultural/Rural) ────────────────────────────────────────

  test_that("Benton Twp. community trees", {
    df_null <- get_df_null("Benton Twp.")
    max_yr <- max(df_null$inventory_year)

    # Zero = no change
    result_0 <- plant_community_trees(
      df_null = df_null, start_yr = 2028, end_yr = 2050, tree_count = 0
    )

    testthat::expect_equal(
      get_urban_tree_area(result_0, max_yr),
      get_urban_tree_area(df_null, max_yr),
      tolerance = 1e-6
    )

    # Small planting increases Urban_Tree
    result_100 <- plant_community_trees(
      df_null = df_null, start_yr = 2028, end_yr = 2050, tree_count = 100
    )

    testthat::expect_gte(
      get_urban_tree_area(result_100, max_yr),
      get_urban_tree_area(df_null, max_yr)
    )

    # Sequestration improves (or stays same if area negligible)
    testthat::expect_lte(
      get_sequestration(result_100, max_yr),
      get_sequestration(result_0, max_yr)
    )

    # Exceeding max throws an error
    baseline_info <- ghg.gert::community_tree_baseline %>%
      dplyr::filter(geog_id == unique(df_null$geog_id)[1])

    if (nrow(baseline_info) == 1) {
      testthat::expect_error(
        plant_community_trees(
          df_null = df_null, start_yr = 2028, end_yr = 2050,
          tree_count = baseline_info$max_plantable_trees + 1
        ),
        "exceeds max_plantable_trees"
      )
    }
  })
})
