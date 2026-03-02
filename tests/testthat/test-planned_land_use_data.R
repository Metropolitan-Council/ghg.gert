## 2040 planned land use -----

testthat::test_that("Total planned land use equals Thrive 2040", {
  testthat::expect_equal(
    planned_land_use$ctu_planned_land_use_council %>%
      magrittr::extract2("acres") %>%
      sum(),
    2684671,
    tolerance = 10000
  )
})


testthat::test_that("Parcel land use cutout is less than generalized planned land use", {
  testthat::expect_gt(
    planned_land_use$ctu_planned_land_use_council %>%
      magrittr::extract2("acres") %>%
      sum(),
    planned_land_use$ctu_planned_land_use_parcel %>%
      magrittr::extract2("acres") %>%
      sum()
  )
})


testthat::test_that("Minneapolis expected density is greater than Lake Elmo", {
  testthat::expect_gt(
    planned_land_use$ctu_planned_land_use_council %>%
      filter(geog_name == "Minneapolis") %>%
      summarise(weighted_mean = sum(unit_mean * acres) / sum(acres)) %>%
      pull(weighted_mean),
    planned_land_use$ctu_planned_land_use_council %>%
      filter(geog_name == "Lake Elmo") %>%
      summarise(weighted_mean = sum(unit_mean * acres) / sum(acres)) %>%
      pull(weighted_mean)
  )
})

testthat::test_that("Eagan expected density is greater than Rosemount", {
  testthat::expect_gt(
    planned_land_use$ctu_planned_land_use_council %>%
      filter(geog_name == "Eagan") %>%
      summarise(weighted_mean = sum(unit_mean * acres) / sum(acres)) %>%
      pull(weighted_mean),
    planned_land_use$ctu_planned_land_use_council %>%
      filter(geog_name == "Rosemount") %>%
      summarise(weighted_mean = sum(unit_mean * acres) / sum(acres)) %>%
      pull(weighted_mean)
  )
})


## planned land use function tests ----

# function rely on user input in shiny app (ghg.ccap.app)
# going to input some mock data for four cities to simulate this input
# Eagan - no change
# Mpls - add 5 to every unit_minimum
# Rosemount - remove 100 acres from highest density and place into lowest density
# Lake Elmo - add new high density land category and remove 50 acres from low dens


# Eagan - no change


eagan_test <- filter_ctu(planned_land_use$ctu_planned_land_use_parcel,
  .selected_ctu = "Eagan"
) %>%
  mutate(
    dupe_index = ave(seq_along(ctu_landuse_desc), ctu, ctu_landuse_desc, FUN = seq_along),
    dupe_count = ave(ctu_landuse_desc, ctu, ctu_landuse_desc, FUN = length),
    ctu_landuse_desc = if_else(dupe_count > 1,
      paste0(ctu_landuse_desc, " - ", dupe_index),
      ctu_landuse_desc
    )
  ) %>%
  select(-dupe_index, -dupe_count) %>%
  mutate(
    acre_change = 0,
    unit_mean = (unit_minimum + unit_maximum) / 2
  ) %>%
  select(
    geog_name,
    ctu_landuse_desc,
    unit_mean,
    acre_change
  )

testthat::test_that("Density should be equal with no change - Eagan", {
  dens_table <- run_scenario_land_use(
    tb = planned_land_use$ctu_planned_land_use_parcel,
    tb_strategy = eagan_test,
    .selected_ctu = "Eagan"
  )
  testthat::expect_equal(
    dens_table$expected_density[1],
    dens_table$expected_density[2]
  )
})

# Mpls - add 5 to every unit_minimum

mpls_test <- filter_ctu(planned_land_use$ctu_planned_land_use_parcel,
  .selected_ctu = "Minneapolis"
) %>%
  mutate(
    unit_minimum = unit_minimum + 5,
    unit_mean = (unit_minimum + unit_maximum) / 2,
    acre_change = 0
  ) %>%
  select(
    geog_name,
    ctu_landuse_desc,
    unit_mean,
    acre_change
  )

testthat::test_that("Density should increase - Minneapolis", {
  dens_table <- run_scenario_land_use(
    tb = planned_land_use$ctu_planned_land_use_parcel,
    tb_strategy = mpls_test,
    .selected_ctu = "Minneapolis"
  )
  testthat::expect_lt(
    dens_table$expected_density[1],
    dens_table$expected_density[2]
  )
})

# Rosemount - remove 100 acres from high density and place into low density

rosemount_test <- filter_ctu(planned_land_use$ctu_planned_land_use_parcel,
  .selected_ctu = "Rosemount"
) %>%
  mutate(acre_change = case_when(
    ctu_landuse_desc == "High Density Residential" ~ -100,
    ctu_landuse_desc == "Low Density Residential" ~ 100,
    TRUE ~ 0
  )) %>%
  select(
    geog_name,
    ctu_landuse_desc,
    unit_mean,
    acre_change
  )

testthat::test_that("Density should decrease - Rosemount", {
  dens_table <- run_scenario_land_use(
    tb = planned_land_use$ctu_planned_land_use_parcel,
    tb_strategy = rosemount_test,
    .selected_ctu = "Rosemount"
  )
  testthat::expect_gt(
    dens_table$expected_density[1],
    dens_table$expected_density[2]
  )
})

# Lake Elmo - add new high density land category and remove 50 acres from low dens

elmo_test <- filter_ctu(planned_land_use$ctu_planned_land_use_parcel,
  .selected_ctu = "Lake Elmo"
) %>%
  mutate(acre_change = case_when(
    ctu_landuse_desc == "Low Density Residential" ~ -100,
    TRUE ~ 0
  )) %>%
  select(
    geog_name,
    ctu_landuse_desc,
    unit_mean,
    acre_change
  ) %>%
  bind_rows(data.frame(
    geog_name = "Lake Elmo",
    ctu_landuse_desc = "New high density",
    unit_mean = 15,
    acre_change = 100
  ))

testthat::test_that("Density should increase - Lake Elmo", {
  dens_table <- run_scenario_land_use(
    tb = planned_land_use$ctu_planned_land_use_parcel,
    tb_strategy = elmo_test,
    .selected_ctu = "Lake Elmo"
  )
  testthat::expect_lt(
    dens_table$expected_density[1],
    dens_table$expected_density[2]
  )
})
