# test-geog_index_coverage.R
# Verify that geog_index can index all sector datasets

test_that("geog_index covers all building energy geog_names", {
  bed <- building_energy_data

  for (nm in c("residential", "non_residential", "jobs",
               "electricity_inventory", "natgas_inventory",
               "propane_inventory")) {
    data_names <- unique(bed[[nm]]$geog_name) %>% na.omit()
    missing <- setdiff(data_names, geog_index$geog_name)
    expect_equal(missing, character(0),
                 info = paste(nm, "missing:", paste(missing, collapse = ", "))
    )
  }
})

test_that("geog_index covers all transportation geog_names", {
  for (nm in c("passenger", "freight")) {
    data_names <- unique(transportation_data[[nm]]$geog_name)
    missing <- setdiff(data_names, geog_index$geog_name)
    expect_equal(missing, character(0),
                 info = paste(nm, "missing:", paste(missing, collapse = ", "))
    )
  }
})

test_that("geog_index covers all agriculture geog_names (7-county metro)", {
  metro_counties <- c("Anoka", "Carver", "Dakota", "Hennepin",
                      "Ramsey", "Scott", "Washington")

  # livestock (CTU-level, filter to metro counties)
  livestock_names <- agriculture_activity_data$livestock %>%
    filter(county_name %in% metro_counties) %>%
    pull(geog_name) %>% unique()
  missing <- setdiff(livestock_names, geog_index$geog_name)
  expect_equal(missing, character(0),
               info = paste("livestock missing:", paste(missing, collapse = ", "))
  )

  # crops (county-level)
  crop_names <- agriculture_activity_data$crops %>%
    filter(county_name %in% metro_counties) %>%
    pull(geog_name) %>% unique()
  missing <- setdiff(crop_names, geog_index$geog_name)
  expect_equal(missing, character(0),
               info = paste("crops missing:", paste(missing, collapse = ", "))
  )

  # fertilizer (county-level)
  fert_names <- agriculture_activity_data$fertilizer %>%
    filter(county_name %in% metro_counties) %>%
    pull(geog_name) %>% unique()
  missing <- setdiff(fert_names, geog_index$geog_name)
  expect_equal(missing, character(0),
               info = paste("fertilizer missing:", paste(missing, collapse = ", "))
  )
})

test_that("geog_index CTUs are present in natural systems data", {
  # natural systems also covers wider geography;
  # check that every geog_index CTU appears in the data (not the reverse)
  gi_names <- geog_index %>%
    filter(geog_level != "REGION") %>%
    pull(geog_name)

  ns_names <- c(
    unique(natural_systems_data$inventory$ctu$geog_name),
    unique(natural_systems_data$inventory$county$geog_name)
  )
  missing <- setdiff(gi_names, ns_names)
  expect_equal(missing, character(0),
               info = paste("geog_index entries missing from natural_systems:",
                            paste(missing, collapse = ", "))
  )
})

test_that("geog_index covers all waste geog_names", {
  for (nm in c("inventory", "solid_waste_baseline", "projections")) {
    data_names <- unique(waste_data[[nm]]$geog_name)
    missing <- setdiff(data_names, geog_index$geog_name)
    expect_equal(missing, character(0),
                 info = paste("waste", nm, "missing:", paste(missing, collapse = ", "))
    )
  }
})

test_that("geog_index covers all planned land use geog_names", {
  for (nm in names(planned_land_use)) {
    plu_names <- unique(planned_land_use[[nm]]$geog_name)
    missing <- setdiff(plu_names, geog_index$geog_name)
    expect_equal(missing, character(0),
                 info = paste("planned_land_use", nm, "missing:", paste(missing, collapse = ", "))
    )
  }
})

test_that("geog_id values match between datasets and geog_index", {
  gi <- geog_index %>% select(geog_name, geog_id) %>% distinct()

  # transportation
  transport_ids <- transportation_data$passenger %>%
    select(geog_name, geog_id) %>%
    distinct()

  joined <- inner_join(transport_ids, gi, by = "geog_name", suffix = c("_data", "_index"))
  mismatches <- joined %>% filter(geog_id_data != geog_id_index)
  expect_equal(nrow(mismatches), 0,
               info = paste("transportation geog_id mismatches:",
                            paste(mismatches$geog_name, collapse = ", "))
  )

  # natural systems CTU
  ns_ids <- natural_systems_data$inventory$ctu %>%
    filter(geog_name %in% gi$geog_name) %>%
    select(geog_name, geog_id) %>%
    distinct()

  joined <- inner_join(ns_ids, gi, by = "geog_name", suffix = c("_data", "_index"))
  mismatches <- joined %>% filter(geog_id_data != geog_id_index)
  expect_equal(nrow(mismatches), 0,
               info = paste("natural_systems geog_id mismatches:",
                            paste(mismatches$geog_name, collapse = ", "))
  )

  # waste
  waste_ids <- waste_data$inventory %>%
    select(geog_name, geog_id) %>%
    distinct()

  joined <- inner_join(waste_ids, gi, by = "geog_name", suffix = c("_data", "_index"))
  mismatches <- joined %>% filter(geog_id_data != geog_id_index)
  expect_equal(nrow(mismatches), 0,
               info = paste("waste geog_id mismatches:",
                            paste(mismatches$geog_name, collapse = ", "))
  )
})
