testthat::test_that("bus mpg correct", {
  testthat::expect_equal(
    fuel_economy %>%
      filter(
        mode == "BU",
        var == "BCIMPG"
      ) %>%
      select(year, var, value),
    tibble::tribble(
      ~year, ~var, ~value,
      "2015", "BCIMPG", 4.6805,
      "2018", "BCIMPG", 4.69454,
      "2020", "BCIMPG", 4.7039,
      "2025", "BCIMPG", 4.75106,
      "2030", "BCIMPG", 4.82268,
      "2035", "BCIMPG", 4.91986,
      "2040", "BCIMPG", 5.04409,
      "2045", "BCIMPG", 5.19732,
      "2050", "BCIMPG", 5.38198
    )
  )
})

testthat::test_that("no abbreviated Saint names", {
  testthat::expect_equal(
    transportation_data$passenger %>%
      select(geog_name) %>%
      unique() %>%
      filter(stringr::str_detect(geog_name, "Saint")) %>%
      nrow(),
    13
  )
})


testthat::test_that("no unorg. suffix", {
  testthat::expect_equal(
    transportation_data$passenger %>%
      select(geog_name) %>%
      unique() %>%
      filter(stringr::str_detect(geog_name, "unorg")) %>%
      nrow(),
    0
  )
})


testthat::test_that("bus AVO correct", {
  testthat::expect_equal(
    vehicle_occupancy %>%
      filter(mode == "BU", var == "AVO") %>%
      select(geog_name, value) %>%
      unique(),
    tibble::tribble(
      ~geog_name, ~value,
      "Afton", 25.64,
      "Andover", 9.1,
      "Anoka", 9.1,
      "Apple Valley", 9.1,
      "Arden Hills", 9.1,
      "Bayport", 8.09,
      "Baytown Twp.", 9.1,
      "Belle Plaine", 25.64,
      "Belle Plaine Twp.", 25.64,
      "Benton Twp.", 15.38,
      "Bethel", 25.64,
      "Birchwood Village", 9.1,
      "Blaine", 9.1,
      "Blakeley Twp.", 25.64,
      "Bloomington", 8.09,
      "Brooklyn Center", 8.09,
      "Brooklyn Park", 8.09,
      "Burnsville", 9.1,
      "Camden Twp.", 25.64,
      "Carver", 15.38,
      "Castle Rock Twp.", 25.64,
      "Cedar Lake Twp.", 25.64,
      "Centerville", 15.38,
      "Champlin", 9.1,
      "Chanhassen", 9.1,
      "Chaska", 15.38,
      "Circle Pines", 9.1,
      "Coates", 25.64,
      "Cologne", 25.64,
      "Columbia Heights", 8.09,
      "Columbus", 15.38,
      "Coon Rapids", 9.1,
      "Corcoran", 15.38,
      "Cottage Grove", 15.38,
      "Credit River", 9.1,
      "Crystal", 8.09,
      "Dahlgren Twp.", 15.38,
      "Dayton", 9.1,
      "Deephaven", 15.38,
      "Dellwood", 9.1,
      "Denmark Twp.", 9.1,
      "Douglas Twp.", 25.64,
      "Eagan", 9.1,
      "East Bethel", 25.64,
      "Eden Prairie", 9.1,
      "Edina", 8.09,
      "Elko New Market", 25.64,
      "Empire", 9.1,
      "Eureka Twp.", 15.38,
      "Excelsior", 15.38,
      "Falcon Heights", 8.09,
      "Farmington", 9.1,
      "Forest Lake", 15.38,
      "Fort Snelling", 8.09,
      "Fridley", 8.09,
      "Gem Lake", 9.1,
      "Golden Valley", 8.09,
      "Grant", 9.1,
      "Greenfield", 25.64,
      "Greenvale Twp.", 25.64,
      "Greenwood", 15.38,
      "Grey Cloud Island Twp.", 15.38,
      "Ham Lake", 9.1,
      "Hamburg", 25.64,
      "Hampton", 25.64,
      "Hampton Twp.", 25.64,
      "Hancock Twp.", 25.64,
      "Hanover", 25.64,
      "Hastings", 8.09,
      "Helena Twp.", 25.64,
      "Hilltop", 8.09,
      "Hollywood Twp.", 25.64,
      "Hopkins", 8.09,
      "Hugo", 15.38,
      "Independence", 25.64,
      "Inver Grove Heights", 8.09,
      "Jackson Twp.", 9.1,
      "Jordan", 25.64,
      "Lake Elmo", 9.1,
      "Lake Saint Croix Beach", 25.64,
      "Lakeland", 25.64,
      "Lakeland Shores", 25.64,
      "Laketown Twp.", 15.38,
      "Lakeville", 9.1,
      "Landfall", 9.1,
      "Lauderdale", 8.09,
      "Lexington", 9.1,
      "Lilydale", 8.09,
      "Lino Lakes", 9.1,
      "Linwood Twp.", 25.64,
      "Little Canada", 8.09,
      "Long Lake", 25.64,
      "Loretto", 25.64,
      "Louisville Twp.", 15.38,
      "Mahtomedi", 9.1,
      "Maple Grove", 9.1,
      "Maple Plain", 25.64,
      "Maplewood", 9.44,
      "Marine on Saint Croix", 25.64,
      "Marshan Twp.", 25.64,
      "May Twp.", 25.64,
      "Mayer", 25.64,
      "Medicine Lake", 9.1,
      "Medina", 9.1,
      "Mendota", 8.09,
      "Mendota Heights", 8.09,
      "Miesville", 25.64,
      "Minneapolis", 9.44,
      "Minnetonka", 8.09,
      "Minnetonka Beach", 15.38,
      "Minnetrista", 15.38,
      "Mound", 15.38,
      "Mounds View", 9.1,
      "New Brighton", 8.09,
      "New Germany", 25.64,
      "New Hope", 9.1,
      "New Market Twp.", 25.64,
      "New Prague", 25.64,
      "New Trier", 25.64,
      "Newport", 9.1,
      "Nininger Twp.", 8.09,
      "North Oaks", 9.1,
      "North Saint Paul", 9.1,
      "Northfield", 25.64,
      "Norwood Young America", 25.64,
      "Nowthen", 25.64,
      "Oak Grove", 25.64,
      "Oak Park Heights", 8.09,
      "Oakdale", 9.1,
      "Orono", 15.38,
      "Osseo", 9.1,
      "Pine Springs", 9.1,
      "Plymouth", 9.1,
      "Prior Lake", 9.1,
      "Ramsey", 9.1,
      "Randolph", 25.64,
      "Randolph Twp.", 25.64,
      "Ravenna Twp.", 15.38,
      "Richfield", 8.09,
      "Robbinsdale", 8.09,
      "Rockford", 25.64,
      "Rogers", 15.38,
      "Rosemount", 9.1,
      "Roseville", 9.44,
      "San Francisco Twp.", 25.64,
      "Sand Creek Twp.", 25.64,
      "Savage", 9.1,
      "Scandia", 25.64,
      "Sciota Twp.", 25.64,
      "Shakopee", 9.1,
      "Shoreview", 9.1,
      "Shorewood", 9.1,
      "South Saint Paul", 8.09,
      "Spring Lake Park", 9.1,
      "Spring Lake Twp.", 15.38,
      "Spring Park", 15.38,
      "Saint Anthony", 8.09,
      "Saint Bonifacius", 25.64,
      "Saint Francis", 25.64,
      "Saint Lawrence Twp.", 25.64,
      "Saint Louis Park", 9.44,
      "Saint Marys Point", 25.64,
      "Saint Paul", 9.44,
      "Saint Paul Park", 9.1,
      "Stillwater", 8.09,
      "Stillwater Twp.", 9.1,
      "Sunfish Lake", 9.1,
      "Tonka Bay", 15.38,
      "Vadnais Heights", 9.1,
      "Vermillion", 25.64,
      "Vermillion Twp.", 25.64,
      "Victoria", 15.38,
      "Waconia", 15.38,
      "Waconia Twp.", 15.38,
      "Waterford Twp.", 25.64,
      "Watertown", 25.64,
      "Watertown Twp.", 15.38,
      "Wayzata", 9.1,
      "West Lakeland Twp.", 15.38,
      "West Saint Paul", 8.09,
      "White Bear Lake", 9.1,
      "White Bear Twp.", 9.1,
      "Willernie", 9.1,
      "Woodbury", 9.1,
      "Woodland", 15.38,
      "Young America Twp.", 25.64,
      "Anoka County", 8.09,
      "Carver County", 8.09,
      "Dakota County", 8.09,
      "Hennepin County", 8.09,
      "Ramsey County", 8.09,
      "Scott County", 8.09,
      "Washington County", 8.09
    )
  )
})


testthat::test_that("All transit is the sum of each transit mode", {
  at_total <- transportation_data$passenger %>%
    filter(
      mode == "AT",
      var == "PMT"
    ) %>%
    group_by(geog_name, year) %>%
    summarise(value = sum(value))

  transit_total <- transportation_data$passenger %>%
    filter(
      mode %in% c(
        "BU",
        "BRT",
        "RU",
        "RI"
      ),
      var == "PMT"
    ) %>%
    group_by(geog_name, year) %>%
    summarise(value = sum(value))


  testthat::expect_equal(at_total,
    transit_total,
    tolerance = 2
  )
})


testthat::test_that("All transit is the sum of each transit mode", {
  brt_total <- transportation_data$passenger %>%
    filter(
      mode %in% c(
        "BRT"
      )
    ) %>%
    group_by(geog_name, year) %>%
    summarise(value = sum(value))


  testthat::expect_equal(
    nrow(brt_total),
    0
  )
})


testthat::test_that("enviro factors and elasticities correct", {
  testthat::expect_equal(
    enviro_factors$TRANSIT_SERVICE_ELAST, 0.9
  )

  testthat::expect_equal(min(elast$vmt_elast), -0.34)
})


testthat::test_that("minimum parking value correct", {
  parking_cost %>%
    filter(var == "PARK") %>%
    ungroup() %>%
    filter(value == min(value)) %>%
    magrittr::extract2("value") %>%
    unique() %>%
    testthat::expect_equal(0.00)
})


testthat::test_that("PHEVPr correct", {
  transportation_data$passenger %>%
    filter(var == "PHEVPr") %>%
    nrow() %>%
    testthat::expect_equal(0)
})


testthat::test_that("no negative PMT values in passenger data", {
  pmt_data <- transportation_data$passenger %>%
    filter(var == "PMT")

  negative_count <- sum(pmt_data$value < 0, na.rm = TRUE)

  testthat::expect_equal(
    negative_count,
    0,
    label = "Number of negative PMT values",
    info = paste(
      "Found", negative_count, "negative PMT values.",
      "PMT (Passenger Miles Traveled) should not be negative."
    )
  )

  # Also check that minimum is non-negative
  testthat::expect_gte(
    min(pmt_data$value, na.rm = TRUE),
    0,
    label = "Minimum PMT value"
  )
})


testthat::test_that("no negative TMT values in freight data", {
  tmt_data <- transportation_data$freight %>%
    filter(var == "TMT")

  negative_count <- sum(tmt_data$value < 0, na.rm = TRUE)

  negative_rows <- tmt_data %>%
    filter(value < 0) %>%
    arrange(value)


  testthat::expect_equal(
    negative_count,
    6,
    label = "Number of negative TMT values"
  )

  # we expect that negative TMT values are only in Laketown Twp.
  # because we expect it will be absorbed into another CTU in the future
  testthat::expect_equal(
    negative_rows$geog_name %>% unique(),
    "Laketown Twp."
  )
})


testthat::test_that("county totals > major city totals for PMT, TMT, and Stock", {
  # Test PMT: Hennepin County > Minneapolis, Ramsey County > Saint Paul
  county_pmt <- transportation_data$passenger %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level == "COUNTY",
      var == "PMT",
      mode == "PLDV",
      geog_name %in% c("Hennepin County", "Ramsey County")
    ) %>%
    select(geog_name, year, value)

  city_pmt <- transportation_data$passenger %>%
    filter(
      var == "PMT",
      mode == "PLDV",
      geog_name %in% c("Minneapolis", "Saint Paul")
    ) %>%
    select(geog_name, year, value)

  minneapolis_pmt <- city_pmt %>% filter(geog_name == "Minneapolis")
  hennepin_pmt <- county_pmt %>% filter(geog_name == "Hennepin County")

  comparison_mpls_hennepin <- minneapolis_pmt %>%
    inner_join(hennepin_pmt, by = "year", suffix = c("_city", "_county"))

  testthat::expect_true(
    all(comparison_mpls_hennepin$value_county > comparison_mpls_hennepin$value_city),
    label = "Hennepin County PMT > Minneapolis PMT"
  )

  stpaul_pmt <- city_pmt %>% filter(geog_name == "Saint Paul")
  ramsey_pmt <- county_pmt %>% filter(geog_name == "Ramsey County")

  comparison_stpaul_ramsey <- stpaul_pmt %>%
    inner_join(ramsey_pmt, by = "year", suffix = c("_city", "_county"))

  testthat::expect_true(
    all(comparison_stpaul_ramsey$value_county > comparison_stpaul_ramsey$value_city),
    label = "Ramsey County PMT > Saint Paul PMT"
  )

  # Test TMT: Hennepin County > Minneapolis, Ramsey County > Saint Paul
  county_tmt <- transportation_data$freight %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level == "COUNTY",
      var == "TMT",
      mode == "CUT",
      geog_name %in% c("Hennepin County", "Ramsey County")
    ) %>%
    select(geog_name, year, value)

  city_tmt <- transportation_data$freight %>%
    filter(
      var == "TMT",
      mode == "CUT",
      geog_name %in% c("Minneapolis", "Saint Paul")
    ) %>%
    select(geog_name, year, value)

  minneapolis_tmt <- city_tmt %>% filter(geog_name == "Minneapolis")
  hennepin_tmt <- county_tmt %>% filter(geog_name == "Hennepin County")

  comparison_mpls_hennepin_tmt <- minneapolis_tmt %>%
    inner_join(hennepin_tmt, by = "year", suffix = c("_city", "_county"))

  testthat::expect_true(
    all(comparison_mpls_hennepin_tmt$value_county > comparison_mpls_hennepin_tmt$value_city),
    label = "Hennepin County TMT > Minneapolis TMT"
  )

  stpaul_tmt <- city_tmt %>% filter(geog_name == "Saint Paul")
  ramsey_tmt <- county_tmt %>% filter(geog_name == "Ramsey County")

  comparison_stpaul_ramsey_tmt <- stpaul_tmt %>%
    inner_join(ramsey_tmt, by = "year", suffix = c("_city", "_county"))

  testthat::expect_true(
    all(comparison_stpaul_ramsey_tmt$value_county > comparison_stpaul_ramsey_tmt$value_city),
    label = "Ramsey County TMT > Saint Paul TMT"
  )

  # Test TotStock: Hennepin County > Minneapolis, Ramsey County > Saint Paul
  county_stock <- transportation_data$passenger %>%
    left_join(
      geog_index %>% select(geog_id, geog_level),
      by = "geog_id"
    ) %>%
    filter(
      geog_level == "COUNTY",
      var == "TotStock",
      mode == "PLDV",
      geog_name %in% c("Hennepin County", "Ramsey County")
    ) %>%
    select(geog_name, year, value)

  city_stock <- transportation_data$passenger %>%
    filter(
      var == "TotStock",
      mode == "PLDV",
      geog_name %in% c("Minneapolis", "Saint Paul")
    ) %>%
    select(geog_name, year, value)

  minneapolis_stock <- city_stock %>% filter(geog_name == "Minneapolis")
  hennepin_stock <- county_stock %>% filter(geog_name == "Hennepin County")

  comparison_mpls_hennepin_stock <- minneapolis_stock %>%
    inner_join(hennepin_stock, by = "year", suffix = c("_city", "_county"))

  testthat::expect_true(
    all(comparison_mpls_hennepin_stock$value_county > comparison_mpls_hennepin_stock$value_city),
    label = "Hennepin County TotStock > Minneapolis TotStock"
  )

  stpaul_stock <- city_stock %>% filter(geog_name == "Saint Paul")
  ramsey_stock <- county_stock %>% filter(geog_name == "Ramsey County")

  comparison_stpaul_ramsey_stock <- stpaul_stock %>%
    inner_join(ramsey_stock, by = "year", suffix = c("_city", "_county"))

  testthat::expect_true(
    all(comparison_stpaul_ramsey_stock$value_county > comparison_stpaul_ramsey_stock$value_city),
    label = "Ramsey County TotStock > Saint Paul TotStock"
  )
})

testthat::test_that("Counties have truck fleets with all fuel types", {
  # BEV can be 0 in starting years
  transportation_data$freight %>%
    filter(
      stringr::str_detect(var, "Stock"),
      mode %in% c("CUT", "SUT"),
      geog_name %in% c("Hennepin County", "Ramsey County", "Dakota County", "Washington County", "Scott County", "Anoka County", "Carver County")
    ) %>%
    filter(value == 0) %>%
    select(year) %>%
    unique() %>%
    testthat::expect_equal(
      tibble::tibble(year = c("2015", "2018", "2020")),
      ignore_attr = TRUE
    )
})



test_that("commute_vmt_proportion covers all transportation geographies", {
  trans_geogs <- unique(geog_index$geog_id)
  cvp_geogs <- unique(commute_vmt_proportion$geog_id)
  missing <- setdiff(trans_geogs, cvp_geogs)

  testthat::expect_true(
    length(missing) == 0,
    info = paste0(
      length(missing), " geographies in transportation_data missing from commute_vmt_proportion"
      # paste(sort(missing), collapse = ", ")
    )
  )
})

test_that("epa_sld_intersection_density covers all transportation geographies", {
  trans_geogs <- unique(geog_index$geog_id)
  epa_geogs <- unique(epa_sld_intersection_density$geog_id)
  missing <- setdiff(trans_geogs, epa_geogs)

  testthat::expect_true(
    length(missing) == 0,
    info = paste0(
      length(missing), " geographies in transportation_data missing from epa_sld_intersection_density"
      # paste(sort(missing), collapse = ", ")
    )
  )
})

