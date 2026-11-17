pkgload::load_all()
source("data-raw/meta/compile_townships.R")

# Sum fleet counts and freight activity without replacing passenger PMT.
for (nm in c("passenger", "freight")) {
    aggregate_vars <- transportation_data[[nm]]$var %>%
        unique() %>%
        stringr::str_subset("Stock|Sales|Exist")
    if (nm == "freight") {
        aggregate_vars <- c(aggregate_vars, "TMT")
    }

    member_data <- transportation_data[[nm]] %>%
        filter(var %in% aggregate_vars)

    group_data <- twp_group_sum(
        member_data,
        value_cols = "value",
        group_cols = c("mode", "var", "year", "aeo_mode", "type"),
        key = "geog_id"
    ) %>%
        select(all_of(names(member_data)))

    testthat::expect_setequal(group_data$geog_id, twp_group_meta()$geog_id)
    testthat::expect_true(all(is.finite(group_data$value)))
    testthat::expect_true(all(
        group_data %>%
            count(geog_id, mode, var, year, aeo_mode, type) %>%
            pull(n) == 1L
    ))

    aggregated_data <- twp_group_replace(member_data, group_data)
    twp_group_check_sum(aggregated_data, "value", nm, key = "geog_id", strict = TRUE)

    transportation_data[[nm]] <- bind_rows(
        transportation_data[[nm]] %>%
            filter(!var %in% aggregate_vars),
        aggregated_data
    ) %>%
        select(all_of(names(transportation_data[[nm]])))
}

usethis::use_data(transportation_data, overwrite = TRUE)
