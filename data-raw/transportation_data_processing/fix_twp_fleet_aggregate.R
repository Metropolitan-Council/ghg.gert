pkgload::load_all()
source("data-raw/meta/compile_townships.R")

# Sum fleet counts by fuel, mode, and year without replacing activity rows.
for (nm in c("passenger", "freight")) {
    fleet <- transportation_data[[nm]] %>%
        filter(stringr::str_detect(var, "Stock|Sales|Exist"))

    group_fleet <- twp_group_sum(
        fleet,
        value_cols = "value",
        group_cols = c("mode", "var", "year", "aeo_mode", "type"),
        key = "geog_id"
    ) %>%
        select(all_of(names(fleet)))

    testthat::expect_setequal(group_fleet$geog_id, twp_group_meta()$geog_id)
    testthat::expect_true(all(is.finite(group_fleet$value)))
    testthat::expect_true(all(
        group_fleet %>%
            count(geog_id, mode, var, year, aeo_mode, type) %>%
            pull(n) == 1L
    ))

    fleet <- twp_group_replace(fleet, group_fleet)
    twp_group_check_sum(fleet, "value", nm, key = "geog_id", strict = TRUE)

    transportation_data[[nm]] <- bind_rows(
        transportation_data[[nm]] %>%
            filter(!stringr::str_detect(var, "Stock|Sales|Exist")),
        fleet
    ) %>%
        select(all_of(names(transportation_data[[nm]])))
}

usethis::use_data(transportation_data, overwrite = TRUE)
