units_fxn <- function(.pct, .selected_ctu = "all") {
  adj_unit_counts(
    .new_homes_to_multifamily_pct = .pct,
    .selected_ctu = .selected_ctu,
    res_tb = building_data$residential
  ) %>%
    filter(
      # scen == "scen",
      year == "2040",
      var %in% c(
        "multifamily_units"
        # "single_family_units"
      )
    ) %>%
    group_by(geog_name, year) %>%
    summarise(value = sum(value, na.rm = T))
}


mfunits_60 <- units_fxn(.pct = .6)
mfunits_0 <- units_fxn(.pct = 0)

mfunits <- mfunits_60 %>%
  mutate(scen = "mfunits_60") %>%
  bind_rows(mfunits_0 %>%
    mutate(scen = "mf_units_0")) %>%
  pivot_wider(names_from = scen, values_from = value)

# this looks like where the error is!!

# ctus where multifamily replacing single family decreases multifamily units - this seems problematic
mfunits %>%
  filter(mfunits_60 < mf_units_0) %>%
  select(geog_name) %>%
  paste(collapse = "")
# now, MF units increase
# [1] "character(0)"

# [1] "c(\"Anoka\", \"Baytown Twp.\", \"Belle Plaine Twp.\", \"Benton Twp.\", \"Blakeley Twp.\", \"Camden Twp.\", \"Dahlgren Twp.\", \"Greenwood\", \"Grey Cloud Island Twp.\", \"Hanover\", \"Laketown Twp.\", \"Lauderdale\", \"Little Canada\", \"Louisville Twp.\", \"Marshan Twp.\", \"Minneapolis\", \"Mounds View\", \"New Market Twp.\", \"Oak Park Heights\", \"Randolph Twp.\", \"Richfield\", \"Robbinsdale\", \"Roseville\", \"Sand Creek Twp.\", \"St. Lawrence Twp.\", \"St. Louis Park\", \"St. Paul\", \"West St. Paul\", \"White Bear Lake\", \"White Bear Twp.\", \"Willernie\"\n)"

# emissions increase results from first test:
# > [1] "c(\"Anoka\", \"Baytown Twp.\", \"Belle Plaine Twp.\", \"Benton Twp.\", \"Blakeley Twp.\", \"Camden Twp.\", \"Cologne\", \"Dahlgren Twp.\", \"Greenwood\", \"Grey Cloud Island Twp.\", \"Hamburg\", \"Hanover\", \"Lake St. Croix Beach\", \"Laketown Twp.\", \"Lauderdale\", \"Little Canada\", \"Louisville Twp.\", \"Marshan Twp.\", \"Mayer\", \"Minneapolis\", \"Mounds View\", \"New Germany\", \"New Market Twp.\", \"Newport\", \"Norwood Young America\", \"Oak Park Heights\", \"Oakdale\", \"Randolph Twp.\", \"Richfield\", \"Robbinsdale\", \"Roseville\", \"Sand Creek Twp.\", \n\"St. Lawrence Twp.\", \"St. Louis Park\", \"St. Paul\", \"St. Paul Park\", \"Watertown\", \"West St. Paul\", \"White Bear Lake\", \"White Bear Twp.\")"

# ctus where multifamily replacing single family increases multifamily units - this seems correct!!
mfunits %>%
  filter(mfunits_60 > mf_units_0) %>%
  select(geog_name) %>%
  paste(collapse = "")
# [1] "c(\"Afton\", \"Andover\", \"Apple Valley\", \"Arden Hills\", \"Bayport\", \"Belle Plaine\", \"Bethel\", \"Birchwood Village\", \"Blaine\", \"Bloomington\", \"Brooklyn Center\", \"Brooklyn Park\", \"Burnsville\", \"Carver\", \"Castle Rock Twp.\", \"Cedar Lake Twp.\", \"Centerville\", \"Champlin\", \"Chanhassen\", \"Chaska\", \"Circle Pines\", \"Coates\", \"Cologne\", \"Columbia Heights\", \"Columbus\", \"Coon Rapids\", \"Corcoran\", \"Cottage Grove\", \"Credit River Twp.\", \"Crystal\", \"Dayton\", \"Deephaven\", \"Dellwood\", \"Denmark Twp.\", \"Douglas Twp.\", \"Eagan\", \n\"East Bethel\", \"Eden Prairie\", \"Edina\", \"Elko New Market\", \"Empire Twp.\", \"Excelsior\", \"Farmington\", \"Forest Lake\", \"Fridley\", \"Gem Lake\", \"Golden Valley\", \"Grant\", \"Greenfield\", \"Greenvale Twp.\", \"Ham Lake\", \"Hamburg\", \"Hampton\", \"Hampton Twp.\", \"Hancock Twp.\", \"Hastings\", \"Helena Twp.\", \"Hollywood Twp.\", \"Hopkins\", \"Hugo\", \"Independence\", \"Inver Grove Heights\", \"Jackson Twp.\", \"Jordan\", \"Lake Elmo\", \"Lake St. Croix Beach\", \"Lakeland\", \"Lakeland Shores\", \"Lakeville\", \"Lexington\", \"Lilydale\", \"Lino Lakes\", \n\"Linwood Twp.\", \"Long Lake\", \"Loretto\", \"Mahtomedi\", \"Maple Grove\", \"Maple Plain\", \"Maplewood\", \"Marine on St. Croix\", \"May Twp.\", \"Mayer\", \"Medicine Lake\", \"Medina\", \"Mendota\", \"Mendota Heights\", \"Miesville\", \"Minnetonka\", \"Minnetonka Beach\", \"Minnetrista\", \"Mound\", \"New Brighton\", \"New Germany\", \"New Hope\", \"New Prague\", \"New Trier\", \"Newport\", \"Nininger Twp.\", \"North Oaks\", \"North St. Paul\", \"Northfield\", \"Norwood Young America\", \"Nowthen\", \"Oak Grove\", \"Oakdale\", \"Orono\", \"Pine Springs\", \"Plymouth\", \n\"Prior Lake\", \"Ramsey\", \"Randolph\", \"Ravenna Twp.\", \"Rockford\", \"Rosemount\", \"San Francisco Twp.\", \"Savage\", \"Scandia\", \"Sciota Twp.\", \"Shakopee\", \"Shoreview\", \"Shorewood\", \"South St. Paul\", \"Spring Lake Park\", \"Spring Lake Twp.\", \"Spring Park\", \"St. Anthony\", \"St. Bonifacius\", \"St. Francis\", \"St. Marys Point\", \"St. Paul Park\", \"Stillwater\", \"Stillwater Twp.\", \"Sunfish Lake\", \"Tonka Bay\", \"Vadnais Heights\", \"Vermillion\", \"Vermillion Twp.\", \"Victoria\", \"Waconia\", \"Waconia Twp.\", \"Waterford Twp.\", \n\"Watertown\", \"Watertown Twp.\", \"Wayzata\", \"West Lakeland Twp.\", \"Woodbury\", \"Woodland\", \"Young America Twp.\")
#
# emissions decrease results from first test:
#> [1] "c(\"Afton\", \"Andover\", \"Apple Valley\", \"Arden Hills\", \"Bayport\", \"Belle Plaine\", \"Bethel\", \"Birchwood Village\", \"Blaine\", \"Bloomington\", \"Brooklyn Center\", \"Brooklyn Park\", \"Burnsville\", \"Carver\", \"Castle Rock Twp.\", \"Cedar Lake Twp.\", \"Centerville\", \"Champlin\", \"Chanhassen\", \"Chaska\", \"Circle Pines\", \"Coates\", \"Columbia Heights\", \"Columbus\", \"Coon Rapids\", \"Corcoran\", \"Cottage Grove\", \"Crystal\", \"Dayton\", \"Deephaven\", \"Dellwood\", \"Denmark Twp.\", \"Douglas Twp.\", \"Eagan\", \"East Bethel\", \"Eden Prairie\", \n\"Edina\", \"Elko New Market\", \"Empire Twp.\", \"Excelsior\", \"Farmington\", \"Forest Lake\", \"Fridley\", \"Gem Lake\", \"Golden Valley\", \"Grant\", \"Greenfield\", \"Greenvale Twp.\", \"Ham Lake\", \"Hampton\", \"Hampton Twp.\", \"Hancock Twp.\", \"Hastings\", \"Helena Twp.\", \"Hollywood Twp.\", \"Hopkins\", \"Hugo\", \"Independence\", \"Inver Grove Heights\", \"Jackson Twp.\", \"Jordan\", \"Lake Elmo\", \"Lakeland\", \"Lakeland Shores\", \"Lakeville\", \"Lexington\", \"Lino Lakes\", \"Linwood Twp.\", \"Long Lake\", \"Loretto\", \"Mahtomedi\", \"Maple Grove\", \n\"Maple Plain\", \"Maplewood\", \"Marine on St. Croix\", \"May Twp.\", \"Medicine Lake\", \"Medina\", \"Mendota\", \"Mendota Heights\", \"Miesville\", \"Minnetonka\", \"Minnetonka Beach\", \"Minnetrista\", \"Mound\", \"New Brighton\", \"New Hope\", \"New Prague\", \"New Trier\", \"Nininger Twp.\", \"North Oaks\", \"North St. Paul\", \"Northfield\", \"Nowthen\", \"Oak Grove\", \"Orono\", \"Pine Springs\", \"Plymouth\", \"Prior Lake\", \"Ramsey\", \"Randolph\", \"Ravenna Twp.\", \"Rockford\", \"Rosemount\", \"San Francisco Twp.\", \"Savage\", \"Scandia\", \"Sciota Twp.\", \n\"Shakopee\", \"Shoreview\", \"Shorewood\", \"South St. Paul\", \"Spring Lake Park\", \"Spring Lake Twp.\", \"Spring Park\", \"St. Anthony\", \"St. Bonifacius\", \"St. Francis\", \"St. Marys Point\", \"Stillwater\", \"Stillwater Twp.\", \"Sunfish Lake\", \"Tonka Bay\", \"Vadnais Heights\", \"Vermillion\", \"Vermillion Twp.\", \"Victoria\", \"Waconia\", \"Waconia Twp.\", \"Waterford Twp.\", \"Watertown Twp.\", \"Wayzata\", \"West Lakeland Twp.\", \"Willernie\", \"Woodbury\", \"Woodland\", \"Young America Twp.\")"

# ctus where multifamily replacing single family does *not* change multifamily units - these are ctus without single-family unit growth
mfunits %>%
  filter(mfunits_60 == mf_units_0) %>%
  select(geog_name) %>%
  paste(collapse = "")
# now, just four
# [1] "c(\"Eureka Twp.\", \"Falcon Heights\", \"Landfall\", \"Osseo\")"


# [1] "c(\"Eureka Twp.\", \"Falcon Heights\", \"Fort Snelling (unorg.)\", \"Hilltop\", \"Landfall\", \"Osseo\", \"Rogers\")"
# emissions stay the same results from first test:
#> [1] "c(\"Credit River Twp.\", \"Eureka Twp.\", \"Falcon Heights\", \"Fort Snelling (unorg.)\", \"Hilltop\", \"Landfall\", \"Lilydale\", \"Osseo\", \"Rogers\")"
