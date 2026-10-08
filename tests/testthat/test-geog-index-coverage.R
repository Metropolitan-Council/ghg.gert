# test-geog-index-coverage.R
# geog_index is the list of geographies the tool offers. Verify that every
# geog_index geography (at the levels a dataset is built for) has data.
# Extra geographies in a dataset that aren't in geog_index are fine.

all_levels <- unique(geog_index$geog_level)
ctu_levels <- c("CITY", "TOWNSHIP", "UNORGANIZED TERRITORY")

# helper: fail if any geog_index geography at `levels` is absent from `data`
expect_geog_coverage <- function(data, label,
                                 levels = all_levels,
                                 allow_missing = character(0)) {
  expected <- geog_index %>%
    dplyr::filter(geog_level %in% levels) %>%
    dplyr::pull(geog_name)

  missing <- setdiff(expected, c(unique(data$geog_name), allow_missing))

  expect(
    length(missing) == 0,
    paste0(
      label, " is missing ", length(missing), " geog_index geographies: ",
      paste(head(missing, 10), collapse = ", "),
      if (length(missing) > 10) ", ..." else ""
    )
  )
}

test_that("building energy data covers all geog_index geographies", {
  for (nm in c(
    "residential", "non_residential", "jobs",
    "electricity_inventory", "natgas_inventory", "propane_inventory"
  )) {
    expect_geog_coverage(building_energy_data[[nm]], paste("building_energy_data", nm))
  }
})

test_that("transportation data covers all geog_index geographies", {
  for (nm in c("passenger", "freight")) {
    expect_geog_coverage(transportation_data[[nm]], paste("transportation_data", nm))
  }
})

test_that("agriculture data covers all geog_index counties", {
  # agriculture is sparse below the county level (only CTUs with ag activity),
  # so only county coverage is checked
  expect_geog_coverage(agriculture_activity_data$livestock, "livestock", levels = "COUNTY")
  expect_geog_coverage(agriculture_activity_data$fertilizer, "fertilizer", levels = "COUNTY")
  expect_geog_coverage(agriculture_activity_data$crops, "crops",
    levels = "COUNTY",
    allow_missing = "Ramsey County" # TODO: confirm Ramsey has no reported cropland
  )
})

test_that("natural systems data covers all geog_index geographies", {
  expect_geog_coverage(natural_systems_data$inventory$ctu, "natural_systems ctu",
    levels = ctu_levels
  )
  expect_geog_coverage(natural_systems_data$inventory$county, "natural_systems county",
    levels = "COUNTY"
  )
})

test_that("waste data covers all geog_index geographies", {
  for (nm in c("inventory", "solid_waste_baseline", "projections")) {
    expect_geog_coverage(waste_data[[nm]], paste("waste_data", nm),
      levels = setdiff(all_levels, "REGION")
    )
  }
})

test_that("planned land use covers all geog_index cities and townships", {
  for (nm in names(planned_land_use)) {
    expect_geog_coverage(planned_land_use[[nm]], paste("planned_land_use", nm),
      levels = c("CITY", "TOWNSHIP")
    )
  }
})

test_that("geog_id values match between datasets and geog_index", {
  gi <- geog_index %>% select(geog_name, geog_id) %>% distinct()

  check_ids <- function(data, label) {
    mismatches <- data %>%
      select(geog_name, geog_id) %>%
      distinct() %>%
      inner_join(gi, by = "geog_name", suffix = c("_data", "_index")) %>%
      filter(geog_id_data != geog_id_index)

    expect(
      nrow(mismatches) == 0,
      paste(label, "geog_id mismatches:", paste(mismatches$geog_name, collapse = ", "))
    )
  }

  check_ids(transportation_data$passenger, "transportation")
  check_ids(natural_systems_data$inventory$ctu, "natural_systems")
  check_ids(waste_data$inventory, "waste")
})
