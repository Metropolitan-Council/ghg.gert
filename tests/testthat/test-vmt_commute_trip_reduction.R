test_commute_trip_reduction <- function(x) {

    testthat::test_that(paste0(x, " Commute trip reduction returns 1 before start_year"), {
        pass_tb_filtered <- transportation_data$passenger %>%
            filter(geog_name == x)

        ctr_adjust <- vmt_commute_trip_reduction(
            .pass_tb = pass_tb_filtered,
            .ctr_employees_targeted = 0.5,
            .ctr_voluntary = TRUE,
            .ctr_start_year = "2030",
            .commute_vmt_proportion = commute_vmt_proportion,
            .enviro_factors = enviro_factors
        )

        # Years before 2030 should have no effect (ctr_adj = 1)
        pre_2030 <- ctr_adjust %>%
            dplyr::filter(year %in% c("2015", "2018", "2020", "2025"))

        testthat::expect_equal(
            pre_2030$commute_trip_reduction_adj,
            c(1, 1, 1, 1)
        )
    })

    testthat::test_that(paste0(x, " Commute trip reduction applies after start_year"), {

        pass_tb_filtered <- transportation_data$passenger %>%
            filter(geog_name == x)

        ctr_adjust <- vmt_commute_trip_reduction(
            .pass_tb = pass_tb_filtered,
            .ctr_employees_targeted = 0.5,
            .ctr_voluntary = TRUE,
            .ctr_start_year = "2030",
            .commute_vmt_proportion = commute_vmt_proportion,
            .enviro_factors = enviro_factors
        )

        # Years at or after 2030 should have reduction
        post_2030 <- ctr_adjust %>%
            dplyr::filter(year >= "2030")

        testthat::expect_true(all(post_2030$commute_trip_reduction_adj < 1))
        testthat::expect_true(all(post_2030$commute_trip_reduction_adj > 0))
    })

    testthat::test_that(paste0(x, " Commute trip reduction voluntary vs mandatory"), {
        pass_tb_filtered <- transportation_data$passenger %>%
            filter(geog_name == x)

        # Voluntary program
        ctr_voluntary <- vmt_commute_trip_reduction(
            .pass_tb = pass_tb_filtered,
            .ctr_employees_targeted = 0.5,
            .ctr_voluntary = TRUE,
            .ctr_start_year = "2030",
            .commute_vmt_proportion = commute_vmt_proportion,
            .enviro_factors = enviro_factors
        ) %>%
            dplyr::filter(year >= "2030") %>%
            dplyr::pull(commute_trip_reduction_adj) %>%
            unique()

        # Mandatory program
        ctr_mandatory <- vmt_commute_trip_reduction(
            .pass_tb = pass_tb_filtered,
            .ctr_employees_targeted = 0.5,
            .ctr_voluntary = FALSE,
            .ctr_start_year = "2030",
            .commute_vmt_proportion = commute_vmt_proportion,
            .enviro_factors = enviro_factors
        ) %>%
            dplyr::filter(year >= "2030") %>%
            dplyr::pull(commute_trip_reduction_adj) %>%
            unique()

        # Mandatory should produce lower value (more reduction) than voluntary
        testthat::expect_lt(ctr_mandatory, ctr_voluntary)
    })

    testthat::test_that(paste0(x, " Commute trip reduction scales with employees targeted"), {

        pass_tb_filtered <- transportation_data$passenger %>%
            filter(geog_name == x)

        ctr_low <- vmt_commute_trip_reduction(
            .pass_tb = pass_tb_filtered,
            .ctr_employees_targeted = 0.25,
            .ctr_voluntary = TRUE,
            .ctr_start_year = "2030",
            .commute_vmt_proportion = commute_vmt_proportion,
            .enviro_factors = enviro_factors
        ) %>%
            dplyr::filter(year >= "2030") %>%
            dplyr::pull(commute_trip_reduction_adj) %>%
            unique()

        ctr_high <- vmt_commute_trip_reduction(
            .pass_tb = pass_tb_filtered,
            .ctr_employees_targeted = 0.75,
            .ctr_voluntary = TRUE,
            .ctr_start_year = "2030",
            .commute_vmt_proportion = commute_vmt_proportion,
            .enviro_factors = enviro_factors
        ) %>%
            dplyr::filter(year >= "2030") %>%
            dplyr::pull(commute_trip_reduction_adj) %>%
            unique()

        # Higher targeted employees should produce lower reduction multiplier
        testthat::expect_lt(ctr_high, ctr_low)
    })

    testthat::test_that(paste0(x, " Commute trip reduction capped at max reduction"), {

        pass_tb_filtered <- transportation_data$passenger %>%
            filter(geog_name == x)

        # With high employees targeted and mandatory, should hit cap
        ctr_adjust <- vmt_commute_trip_reduction(
            .pass_tb = pass_tb_filtered,
            .ctr_employees_targeted = 1.0,
            .ctr_voluntary = FALSE,
            .ctr_start_year = "2030",
            .commute_vmt_proportion = commute_vmt_proportion,
            .enviro_factors = enviro_factors
        )

        post_2030 <- ctr_adjust %>%
            dplyr::filter(year >= "2030") %>%
            dplyr::pull(commute_trip_reduction_adj) %>%
            unique()

        # Reduction should not exceed cap scaled to total VMT
        # capped_reduction floor = 1 + (-0.45) = 0.55
        # adj = 1 + (0.55 - 1) * vmt_proportion, so always < 1 and > 0
        vmt_prop <- commute_vmt_proportion %>%
            dplyr::filter(geog_name == x) %>%
            dplyr::pull(value)
        max_adj <- 1 + (1 + enviro_factors$MAX_COMMUTE_TRIP_REDUCTION_PCT - 1) *
            vmt_prop
        testthat::expect_true(all(post_2030 >= max_adj))
    })

    testthat::test_that(paste0(x, " Commute trip reduction with zero employees returns no effect"), {
        pass_tb_filtered <- transportation_data$passenger %>%
            filter(geog_name == x)

        ctr_adjust <- vmt_commute_trip_reduction(
            .pass_tb = pass_tb_filtered,
            .ctr_employees_targeted = 0,
            .ctr_voluntary = TRUE,
            .ctr_start_year = "2030",
            .commute_vmt_proportion = commute_vmt_proportion,
            .enviro_factors = enviro_factors
        )

        # All years should have ctr_adj = 1 (no reduction)
        testthat::expect_equal(
            ctr_adjust$commute_trip_reduction_adj,
            rep(1, nrow(ctr_adjust))
        )
    })

    testthat::test_that(paste0(x, " Commute trip reduction returns correct structure"), {

        pass_tb_filtered <- transportation_data$passenger %>%
            filter(geog_name == x)

        ctr_adjust <- vmt_commute_trip_reduction(
            .pass_tb = pass_tb_filtered,
            .ctr_employees_targeted = 0.5,
            .ctr_voluntary = TRUE,
            .ctr_start_year = "2030",
            .commute_vmt_proportion = commute_vmt_proportion,
            .enviro_factors = enviro_factors
        )

        # Verify tibble structure
        testthat::expect_s3_class(ctr_adjust, "tbl_df")
        testthat::expect_equal(
            nrow(ctr_adjust),
            nrow(pass_tb_filtered %>% dplyr::select(year) %>% dplyr::distinct())
        )
        testthat::expect_true("commute_trip_reduction_adj" %in% names(ctr_adjust))
    })
}

purrr::map(
    geography_test_list,
    test_commute_trip_reduction
)
