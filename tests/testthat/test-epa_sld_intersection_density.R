testthat::test_that("epa_sld_intersection_density has correct structure", {
    # Check required columns exist
    required_cols <- c("geog_id", "geog_name", "geog_id_type", "intersection_density")
    testthat::expect_true(all(required_cols %in% names(epa_sld_intersection_density)))

    # Check data types
    testthat::expect_type(epa_sld_intersection_density$geog_id, "character")
    testthat::expect_type(epa_sld_intersection_density$geog_name, "character")
    testthat::expect_type(epa_sld_intersection_density$geog_id_type, "character")
    testthat::expect_type(epa_sld_intersection_density$intersection_density, "double")
})

testthat::test_that("epa_sld_intersection_density has no missing key values", {
    # No missing geog_id
    testthat::expect_equal(sum(is.na(epa_sld_intersection_density$geog_id)), 0)

    # No missing geog_name
    testthat::expect_equal(sum(is.na(epa_sld_intersection_density$geog_name)), 0)

    # Intersection density can have some NAs but should be mostly complete
    na_count <- sum(is.na(epa_sld_intersection_density$intersection_density))
    total_count <- nrow(epa_sld_intersection_density)
    testthat::expect_lt(na_count / total_count, 0.1) # Less than 10% missing
})

testthat::test_that("epa_sld_intersection_density values are valid", {
    # Intersection density should be non-negative
    non_na_values <- epa_sld_intersection_density$intersection_density[
        !is.na(epa_sld_intersection_density$intersection_density)
    ]
    testthat::expect_true(all(non_na_values >= 0))

    # Intersection density should be reasonable (not impossibly high)
    # Based on EPA SLD documentation, max should be around 30-40 intersections per sq mi
    testthat::expect_true(all(non_na_values <= 100))
})

testthat::test_that("epa_sld_intersection_density geog_ids match geog_index", {
    # All geog_ids should be in geog_index
    missing_geog_ids <- setdiff(
        epa_sld_intersection_density$geog_id,
        geog_index$geog_id
    )
    testthat::expect_equal(length(missing_geog_ids), 0)
})

testthat::test_that("epa_sld_intersection_density has no duplicates", {
    # Each geog_id should appear only once
    duplicate_count <- epa_sld_intersection_density %>%
        dplyr::group_by(geog_id) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::filter(n > 1) %>%
        nrow()

    testthat::expect_equal(duplicate_count, 0)
})

testthat::test_that("epa_sld_intersection_density covers most CTUs", {
    # Should have data for most CTUs in geog_index (at least 90% coverage)
    ctu_count_in_geog <- nrow(geog_index)
    ctu_count_in_epa <- nrow(epa_sld_intersection_density)
    coverage_ratio <- ctu_count_in_epa / ctu_count_in_geog

    testthat::expect_gt(coverage_ratio, 0.9)

    # Also check that we have at least 180 CTUs
    testthat::expect_gte(ctu_count_in_epa, 180)
})

testthat::test_that("urban CTU has higher intersection density than rural CTU", {
    # Identify an urban and a rural CTU from geog_index

    urban_density <- epa_sld_intersection_density %>%
        dplyr::filter(geog_name == "Minneapolis") %>%
        dplyr::pull(intersection_density)

    rural_density <- epa_sld_intersection_density %>%
        dplyr::filter(geog_name == "Afton") %>%
        dplyr::pull(intersection_density)

    testthat::expect_gt(urban_density, rural_density)
})
