library(ghg.sp)
library(tidyverse)


stock_fxn <-  function(.pct){
  run_scenario_building(
    .new_homes_to_multifamily_pct = .pct,
    ) %>%
    filter(
      scen == "scen",
      year == "2040",
      var %in% c(
        "residential_electricity_emissions_kg_co",
        "residential_natural_gas_emissions_kg_co",
        "total_industrial_commercial_emissions"
      )
    ) %>%
    group_by(ctu_name, scen, year) %>%
    summarise(value = sum(value, na.rm = T))
}


mfstock_60 <- stock_fxn(.pct = .6)
mfstock_0 <- stock_fxn(.pct = 0)

stock <- mfstock_60 %>%
  mutate(scen = "mfstock_60") %>%
  bind_rows(mfstock_0 %>%
              mutate(scen = "mf_stock_0")) %>%
  pivot_wider(names_from = scen, values_from = value)

#ctus where multifamily replacing single family increases emissions - this seems problematic
stock %>%
  filter(mfstock_60 > mf_stock_0) %>%
  select(ctu_name) %>%
  paste(collapse= "")
#> [1] "c(\"Anoka\", \"Baytown Twp.\", \"Belle Plaine Twp.\", \"Benton Twp.\", \"Blakeley Twp.\", \"Camden Twp.\", \"Cologne\", \"Dahlgren Twp.\", \"Greenwood\", \"Grey Cloud Island Twp.\", \"Hamburg\", \"Hanover\", \"Lake St. Croix Beach\", \"Laketown Twp.\", \"Lauderdale\", \"Little Canada\", \"Louisville Twp.\", \"Marshan Twp.\", \"Mayer\", \"Minneapolis\", \"Mounds View\", \"New Germany\", \"New Market Twp.\", \"Newport\", \"Norwood Young America\", \"Oak Park Heights\", \"Oakdale\", \"Randolph Twp.\", \"Richfield\", \"Robbinsdale\", \"Roseville\", \"Sand Creek Twp.\", \n\"St. Lawrence Twp.\", \"St. Louis Park\", \"St. Paul\", \"St. Paul Park\", \"Watertown\", \"West St. Paul\", \"White Bear Lake\", \"White Bear Twp.\")"

#ctus where multifamily replacing single family decreases emissions - this seems correct!!
stock %>%
  filter(mfstock_60 < mf_stock_0) %>%
  select(ctu_name) %>%
  paste(collapse= "")
#> [1] "c(\"Afton\", \"Andover\", \"Apple Valley\", \"Arden Hills\", \"Bayport\", \"Belle Plaine\", \"Bethel\", \"Birchwood Village\", \"Blaine\", \"Bloomington\", \"Brooklyn Center\", \"Brooklyn Park\", \"Burnsville\", \"Carver\", \"Castle Rock Twp.\", \"Cedar Lake Twp.\", \"Centerville\", \"Champlin\", \"Chanhassen\", \"Chaska\", \"Circle Pines\", \"Coates\", \"Columbia Heights\", \"Columbus\", \"Coon Rapids\", \"Corcoran\", \"Cottage Grove\", \"Crystal\", \"Dayton\", \"Deephaven\", \"Dellwood\", \"Denmark Twp.\", \"Douglas Twp.\", \"Eagan\", \"East Bethel\", \"Eden Prairie\", \n\"Edina\", \"Elko New Market\", \"Empire Twp.\", \"Excelsior\", \"Farmington\", \"Forest Lake\", \"Fridley\", \"Gem Lake\", \"Golden Valley\", \"Grant\", \"Greenfield\", \"Greenvale Twp.\", \"Ham Lake\", \"Hampton\", \"Hampton Twp.\", \"Hancock Twp.\", \"Hastings\", \"Helena Twp.\", \"Hollywood Twp.\", \"Hopkins\", \"Hugo\", \"Independence\", \"Inver Grove Heights\", \"Jackson Twp.\", \"Jordan\", \"Lake Elmo\", \"Lakeland\", \"Lakeland Shores\", \"Lakeville\", \"Lexington\", \"Lino Lakes\", \"Linwood Twp.\", \"Long Lake\", \"Loretto\", \"Mahtomedi\", \"Maple Grove\", \n\"Maple Plain\", \"Maplewood\", \"Marine on St. Croix\", \"May Twp.\", \"Medicine Lake\", \"Medina\", \"Mendota\", \"Mendota Heights\", \"Miesville\", \"Minnetonka\", \"Minnetonka Beach\", \"Minnetrista\", \"Mound\", \"New Brighton\", \"New Hope\", \"New Prague\", \"New Trier\", \"Nininger Twp.\", \"North Oaks\", \"North St. Paul\", \"Northfield\", \"Nowthen\", \"Oak Grove\", \"Orono\", \"Pine Springs\", \"Plymouth\", \"Prior Lake\", \"Ramsey\", \"Randolph\", \"Ravenna Twp.\", \"Rockford\", \"Rosemount\", \"San Francisco Twp.\", \"Savage\", \"Scandia\", \"Sciota Twp.\", \n\"Shakopee\", \"Shoreview\", \"Shorewood\", \"South St. Paul\", \"Spring Lake Park\", \"Spring Lake Twp.\", \"Spring Park\", \"St. Anthony\", \"St. Bonifacius\", \"St. Francis\", \"St. Marys Point\", \"Stillwater\", \"Stillwater Twp.\", \"Sunfish Lake\", \"Tonka Bay\", \"Vadnais Heights\", \"Vermillion\", \"Vermillion Twp.\", \"Victoria\", \"Waconia\", \"Waconia Twp.\", \"Waterford Twp.\", \"Watertown Twp.\", \"Wayzata\", \"West Lakeland Twp.\", \"Willernie\", \"Woodbury\", \"Woodland\", \"Young America Twp.\")"

#ctus where multifamily replacing single family does not change emissions - this seems strange, but okay.
stock %>%
  filter(mfstock_60 == mf_stock_0) %>%
  select(ctu_name) %>%
  paste(collapse= "")
#> [1] "c(\"Credit River Twp.\", \"Eureka Twp.\", \"Falcon Heights\", \"Fort Snelling (unorg.)\", \"Hilltop\", \"Landfall\", \"Lilydale\", \"Osseo\", \"Rogers\")"


#has nothing to do with industrial emissions (maintain consistent through scenario)
#however both residential and natural gas emissions do this

#next steps: check individually
# adj_unit_counts
# FIXED!!
#
# units_fxn <-  function(.pct, .selected_ctu = "all"){
#   adj_unit_counts(
#     .new_homes_to_multifamily_pct = .pct,
#     .selected_ctu = .selected_ctu,
#     res_tb = building_data$residential
#   ) %>%
#     filter(
#       #scen == "scen",
#       year == "2040",
#       var %in% c(
#         "multifamily_units"
#         #"single_family_units"
#       )
#     ) %>%
#     group_by(ctu_name, year) %>%
#     summarise(value = sum(value, na.rm = T))
# }
#
#
# mfunits_60 <- units_fxn(.pct = .6)
# mfunits_0 <- units_fxn(.pct = 0)
#
# mfunits <- mfunits_60 %>%
#   mutate(scen = "mfunits_60") %>%
#   bind_rows(mfunits_0 %>%
#               mutate(scen = "mf_units_0")) %>%
#   pivot_wider(names_from = scen, values_from = value)
#
# # this looks like where the error is!!
#
# #ctus where multifamily replacing single family decreases mf units - this seems problematic
# mfunits %>%
#   filter(mfunits_60 < mf_units_0) %>%
#   select(ctu_name) %>%
#   paste(collapse= "")
# #> [1] "c(\"Anoka\", \"Baytown Twp.\", \"Belle Plaine Twp.\", \"Benton Twp.\", \"Blakeley Twp.\", \"Camden Twp.\", \"Cologne\", \"Dahlgren Twp.\", \"Greenwood\", \"Grey Cloud Island Twp.\", \"Hamburg\", \"Hanover\", \"Lake St. Croix Beach\", \"Laketown Twp.\", \"Lauderdale\", \"Little Canada\", \"Louisville Twp.\", \"Marshan Twp.\", \"Mayer\", \"Minneapolis\", \"Mounds View\", \"New Germany\", \"New Market Twp.\", \"Newport\", \"Norwood Young America\", \"Oak Park Heights\", \"Oakdale\", \"Randolph Twp.\", \"Richfield\", \"Robbinsdale\", \"Roseville\", \"Sand Creek Twp.\", \n\"St. Lawrence Twp.\", \"St. Louis Park\", \"St. Paul\", \"St. Paul Park\", \"Watertown\", \"West St. Paul\", \"White Bear Lake\", \"White Bear Twp.\")"
#
# #ctus where multifamily replacing single family increases units - this seems correct!!
# mfunits %>%
#   filter(mfunits_60 > mf_units_0) %>%
#   select(ctu_name) %>%
#   paste(collapse= "")
# #> [1] "c(\"Afton\", \"Andover\", \"Apple Valley\", \"Arden Hills\", \"Bayport\", \"Belle Plaine\", \"Bethel\", \"Birchwood Village\", \"Blaine\", \"Bloomington\", \"Brooklyn Center\", \"Brooklyn Park\", \"Burnsville\", \"Carver\", \"Castle Rock Twp.\", \"Cedar Lake Twp.\", \"Centerville\", \"Champlin\", \"Chanhassen\", \"Chaska\", \"Circle Pines\", \"Coates\", \"Columbia Heights\", \"Columbus\", \"Coon Rapids\", \"Corcoran\", \"Cottage Grove\", \"Crystal\", \"Dayton\", \"Deephaven\", \"Dellwood\", \"Denmark Twp.\", \"Douglas Twp.\", \"Eagan\", \"East Bethel\", \"Eden Prairie\", \n\"Edina\", \"Elko New Market\", \"Empire Twp.\", \"Excelsior\", \"Farmington\", \"Forest Lake\", \"Fridley\", \"Gem Lake\", \"Golden Valley\", \"Grant\", \"Greenfield\", \"Greenvale Twp.\", \"Ham Lake\", \"Hampton\", \"Hampton Twp.\", \"Hancock Twp.\", \"Hastings\", \"Helena Twp.\", \"Hollywood Twp.\", \"Hopkins\", \"Hugo\", \"Independence\", \"Inver Grove Heights\", \"Jackson Twp.\", \"Jordan\", \"Lake Elmo\", \"Lakeland\", \"Lakeland Shores\", \"Lakeville\", \"Lexington\", \"Lino Lakes\", \"Linwood Twp.\", \"Long Lake\", \"Loretto\", \"Mahtomedi\", \"Maple Grove\", \n\"Maple Plain\", \"Maplewood\", \"Marine on St. Croix\", \"May Twp.\", \"Medicine Lake\", \"Medina\", \"Mendota\", \"Mendota Heights\", \"Miesville\", \"Minnetonka\", \"Minnetonka Beach\", \"Minnetrista\", \"Mound\", \"New Brighton\", \"New Hope\", \"New Prague\", \"New Trier\", \"Nininger Twp.\", \"North Oaks\", \"North St. Paul\", \"Northfield\", \"Nowthen\", \"Oak Grove\", \"Orono\", \"Pine Springs\", \"Plymouth\", \"Prior Lake\", \"Ramsey\", \"Randolph\", \"Ravenna Twp.\", \"Rockford\", \"Rosemount\", \"San Francisco Twp.\", \"Savage\", \"Scandia\", \"Sciota Twp.\", \n\"Shakopee\", \"Shoreview\", \"Shorewood\", \"South St. Paul\", \"Spring Lake Park\", \"Spring Lake Twp.\", \"Spring Park\", \"St. Anthony\", \"St. Bonifacius\", \"St. Francis\", \"St. Marys Point\", \"Stillwater\", \"Stillwater Twp.\", \"Sunfish Lake\", \"Tonka Bay\", \"Vadnais Heights\", \"Vermillion\", \"Vermillion Twp.\", \"Victoria\", \"Waconia\", \"Waconia Twp.\", \"Waterford Twp.\", \"Watertown Twp.\", \"Wayzata\", \"West Lakeland Twp.\", \"Willernie\", \"Woodbury\", \"Woodland\", \"Young America Twp.\")"
#
# #ctus where multifamily replacing single family does not change units - this seems strange, but okay.
# mfunits %>%
#   filter(mfunits_60 == mf_units_0) %>%
#   select(ctu_name) %>%
#   paste(collapse= "")
# #> [1] "c(\"Credit River Twp.\", \"Eureka Twp.\", \"Falcon Heights\", \"Fort Snelling (unorg.)\", \"Hilltop\", \"Landfall\", \"Lilydale\", \"Osseo\", \"Rogers\")"

# res_tb <- building_data$residential
# res_tb <- res_tb %>%
#   dplyr::filter(var %in% c(
#     "multifamily_units",
#     "single_family_units"
#   )) %>%
#   dplyr::group_by(ctu_name, var) %>%
#   tidyr::pivot_wider(names_from = c(var, year), values_from = value, names_sep = ".")%>%
#   dplyr::mutate(
#     new_sf_homes = single_family_units.2040 - single_family_units.2018,
#     new_mf_homes = multifamily_units.2040 - multifamily_units.2018
#   )
#
# res_tb %>%
#   filter(new_mf_homes < 0) %>%
#   select(ctu_name) %>%
#   paste(collapse= "")

#now we gotta figure out the other stuff

floor_fxn <-  function(.pct, .selected_ctu = "all"){

  # B.R1 (MF to SF)
  tb01 <- adj_unit_counts(
    res_tb = building_data$residential,
    .selected_ctu = .selected_ctu,
    .new_homes_to_multifamily_pct = .pct
  )

  # B.R2 (Affordable Floor Area)
  calc_affordable_floor_area(
    res_tb = tb01,
    .selected_ctu = .selected_ctu,
    .single_family_floor_area_growth_pct = .05
  )
}

# do they all have higher floor area multifamily?

#[1] "c(\"Cologne\", \"Hamburg\", \"Lake St. Croix Beach\", \"Mayer\", \"New Germany\", \"Newport\", \"Norwood Young America\", \"Oakdale\", \"St. Paul Park\", \"Watertown\")"
# YUP

# comes from building_data
