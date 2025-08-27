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
    transportation_data$passenger %>%
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
      "Young America Twp.", 25.64
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
                         tolerance = 1
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


  testthat::expect_equal(nrow(brt_total),
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
  transportation_data$passenger %>%
    filter(mode == "PLDV", var == "PARK") %>%
    ungroup() %>%
    filter(value == min(value)) %>%
    magrittr::extract2("value") %>% unique() %>%

  testthat::expect_equal(0.01)

})
