# test-building_energy_data_completeness.R
#
# Validates that every geography in geog_index has the required
# background data to run the building energy module end-to-end.

all_geographies <- ghg.ccap::geog_index %>%
  filter(imagine_designation != "Non-Council Community") %>%
  pull(geog_name)
ctu_geographies <- all_geographies[!grepl("County$", all_geographies)]
county_geographies <- all_geographies[grepl("County$", all_geographies)]

residential_categories <- c(
  "single_family_detached",
  "single_family_attached",
  "multifamily_units",
  "manufactured_homes"
)

inventory_years <- 2005:2022
projection_years <- 2005:2050


# --- parcel_ctu: all 4 housing types for every CTU ---

test_that("Every CTU has all 4 residential housing types in parcel_ctu", {
  parcel_coverage <- ghg.ccap::parcel_ctu %>%
    filter(geog_name %in% ctu_geographies) %>%
    group_by(geog_name) %>%
    summarize(
      n_categories = n_distinct(mc_classification),
      categories = list(sort(unique(mc_classification))),
      .groups = "drop"
    )

  # every CTU should be present
  missing_ctus <- setdiff(ctu_geographies, parcel_coverage$geog_name)
  expect_equal(length(missing_ctus), 0,
               label = paste("CTUs missing from parcel_ctu:", paste(missing_ctus, collapse = ", "))
  )

  # every CTU should have all 4 categories
  incomplete <- parcel_coverage %>% filter(n_categories < 4)
  expect_equal(nrow(incomplete), 0,
               label = paste("CTUs with incomplete housing types:",
                             paste(incomplete$geog_name, collapse = ", "))
  )
})

test_that("Every county has all 4 residential housing types in parcel_ctu", {
  parcel_county <- ghg.ccap::parcel_ctu %>%
    filter(geog_name %in% county_geographies) %>%
    group_by(geog_name) %>%
    summarize(n_categories = n_distinct(mc_classification), .groups = "drop")

  missing_counties <- setdiff(county_geographies, parcel_county$geog_name)
  expect_equal(length(missing_counties), 0,
               label = paste("Counties missing from parcel_ctu:", paste(missing_counties, collapse = ", "))
  )

  incomplete <- parcel_county %>% filter(n_categories < 4)
  expect_equal(nrow(incomplete), 0,
               label = paste("Counties with incomplete housing types:",
                             paste(incomplete$geog_name, collapse = ", "))
  )
})

test_that("parcel_ctu has no NA or zero sq_ft_use", {
  bad_rows <- ghg.ccap::parcel_ctu %>%
    filter(is.na(sq_ft_use) | sq_ft_use <= 0)
  expect_equal(nrow(bad_rows), 0,
               label = paste("Rows with NA/zero sq_ft_use:",
                             paste(unique(bad_rows$geog_name), collapse = ", "))
  )
})


# --- demographic_data: at least one housing type present in all years ---

test_that("Every CTU has residential housing data across all projection years", {
  housing <- ghg.ccap::demographic_data %>%
    filter(
      sp_categories %in% residential_categories,
      geog_name %in% ctu_geographies
    ) %>%
    group_by(geog_name) %>%
    summarize(
      year_range = list(sort(unique(inventory_year))),
      min_year = min(inventory_year),
      max_year = max(inventory_year),
      n_years = n_distinct(inventory_year),
      .groups = "drop"
    )

  missing_ctus <- setdiff(ctu_geographies, housing$geog_name)
  expect_equal(length(missing_ctus), 0,
               label = paste("CTUs missing housing data:", paste(missing_ctus, collapse = ", "))
  )

  incomplete <- housing %>% filter(min_year > 2005 | max_year < 2050)
  expect_equal(nrow(incomplete), 0,
               label = paste("CTUs with incomplete year range:",
                             paste(incomplete$geog_name, collapse = ", "))
  )
})


# --- building_energy_data$residential: present for all CTUs ---

test_that("Every CTU has residential building data", {
  res_ctus <- building_energy_data$residential %>%
    distinct(geog_name) %>%
    pull()

  missing <- setdiff(ctu_geographies, res_ctus)
  expect_equal(length(missing), 0,
               label = paste("CTUs missing from residential:", paste(missing, collapse = ", "))
  )
})


# --- jobs: present in all projection years ---

test_that("Every CTU has jobs data across all projection years", {
  jobs <- building_energy_data$jobs %>%
    filter(geog_name %in% ctu_geographies) %>%
    group_by(geog_name) %>%
    summarize(
      min_year = min(emissions_year),
      max_year = max(emissions_year),
      .groups = "drop"
    )

  missing <- setdiff(ctu_geographies, jobs$geog_name)
  expect_equal(length(missing), 0,
               label = paste("CTUs missing jobs data:", paste(missing, collapse = ", "))
  )

  incomplete <- jobs %>% filter(min_year > 2005 | max_year < 2050)
  expect_equal(nrow(incomplete), 0,
               label = paste("CTUs with incomplete jobs year range:",
                             paste(incomplete$geog_name, collapse = ", "))
  )
})


# --- electricity inventory: 2005-2022 for every geography ---

test_that("Every geography has complete electricity inventory 2005-2022", {
  elec <- building_energy_data$electricity_inventory %>%
    filter(sector == "Residential") %>%
    group_by(geog_name) %>%
    summarize(
      n_years = n_distinct(emissions_year),
      min_year = min(emissions_year),
      max_year = max(emissions_year),
      .groups = "drop"
    )

  missing <- setdiff(all_geographies, elec$geog_name)
  expect_equal(length(missing), 0,
               label = paste("Geographies missing electricity inventory:",
                             paste(missing, collapse = ", "))
  )

  incomplete <- elec %>%
    filter(min_year > 2005 | max_year < 2022 | n_years < length(inventory_years))
  expect_equal(nrow(incomplete), 0,
               label = paste("Geographies with incomplete electricity years:",
                             paste(incomplete$geog_name, collapse = ", "))
  )
})


# --- natural gas inventory: 2005-2022 for every geography ---

test_that("Every geography has complete natgas inventory 2005-2022", {
  natgas <- building_energy_data$natgas_inventory %>%
    filter(sector == "Residential") %>%
    group_by(geog_name) %>%
    summarize(
      n_years = n_distinct(emissions_year),
      min_year = min(emissions_year),
      max_year = max(emissions_year),
      .groups = "drop"
    )

  missing <- setdiff(all_geographies, natgas$geog_name)
  expect_equal(length(missing), 0,
               label = paste("Geographies missing natgas inventory:",
                             paste(missing, collapse = ", "))
  )

  incomplete <- natgas %>%
    filter(min_year > 2005 | max_year < 2022 | n_years < length(inventory_years))
  expect_equal(nrow(incomplete), 0,
               label = paste("Geographies with incomplete natgas years:",
                             paste(incomplete$geog_name, collapse = ", "))
  )
})


# --- propane inventory: present for all CTUs ---

test_that("Every CTU has propane inventory data", {
  propane_ctus <- building_energy_data$propane_inventory %>%
    distinct(geog_name) %>%
    pull()

  missing <- setdiff(ctu_geographies, propane_ctus)
  expect_equal(length(missing), 0,
               label = paste("CTUs missing propane inventory:", paste(missing, collapse = ", "))
  )
})


# --- energy profiles: non-empty with required scenarios (CI only, expensive) ---

if (testthat:::on_ci()) {
  required_scenarios <- c("baseline", "retrofit", "heatpump", "combination",
                          "new_build", "new_build_heatpump", "new_build_leed")

  test_that("calc_building_energy returns complete profiles for every CTU", {
    purrr::walk(ctu_geographies, function(ctu) {
      profiles <- calc_building_energy(.selected_ctu = ctu)

      expect_gt(nrow(profiles), 0,
                label = paste("non-empty profiles for", ctu))

      missing_scenarios <- setdiff(required_scenarios, unique(profiles$scenario))
      expect_equal(length(missing_scenarios), 0,
                   label = paste(ctu, "missing scenarios:", paste(missing_scenarios, collapse = ", "))
      )

      # no NA energy values
      na_rows <- profiles %>% filter(is.na(scenario_mwh) | is.na(scenario_mcf))
      expect_equal(nrow(na_rows), 0,
                   label = paste(ctu, "has NA energy values in profiles"))
    })
  })
}
