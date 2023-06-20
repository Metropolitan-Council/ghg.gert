library(ghg.sp)
library(tidyverse)


floorarea_fxn <-  function(.pct){
  run_scenario_building(
    .single_family_floor_area_growth_pct = .pct,
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


floorarea_05 <- floorarea_fxn(.pct = .05) #this is bau, as discussed on friday
floorarea_15 <- floorarea_fxn(.pct = .15)

floorarea <- floorarea_05 %>%
  mutate(scen = "fa_05") %>%
  bind_rows(floorarea_15 %>%
              mutate(scen = "fa_15")) %>%
  pivot_wider(names_from = scen, values_from = value)

#ctus where floor area increasing decreases emissions - this seems problematic
floorarea %>%
  filter(fa_05 > fa_15) %>%
  select(ctu_name) %>%
  paste(collapse= "")
#> [1] "c(\"Arden Hills\", \"Benton Twp.\", \"Camden Twp.\", \"Carver\", \"Chanhassen\", \"Chaska\", \"Cologne\", \"Dahlgren Twp.\", \"Falcon Heights\", \"Gem Lake\", \"Hamburg\", \"Hancock Twp.\", \"Hollywood Twp.\", \"Laketown Twp.\", \"Lauderdale\", \"Little Canada\", \"Maplewood\", \"Mayer\", \"Mounds View\", \"New Brighton\", \"New Germany\", \"North Oaks\", \"North St. Paul\", \"Norwood Young America\", \"Roseville\", \"San Francisco Twp.\", \"Shoreview\", \"Shorewood\", \"St. Paul\", \"Vadnais Heights\", \"Victoria\", \"Waconia\", \"Waconia Twp.\", \"Watertown\", \n\"Watertown Twp.\", \"White Bear Twp.\", \"Young America Twp.\")"

#ctus where floor area increasing increases emissions - this seems correct!!
floorarea %>%
  filter(fa_05 < fa_15) %>%
  select(ctu_name) %>%
  paste(collapse= "")
#> [1] "c(\"Afton\", \"Andover\", \"Anoka\", \"Apple Valley\", \"Bayport\", \"Baytown Twp.\", \"Belle Plaine\", \"Belle Plaine Twp.\", \"Bethel\", \"Birchwood Village\", \"Blaine\", \"Blakeley Twp.\", \"Bloomington\", \"Brooklyn Center\", \"Brooklyn Park\", \"Burnsville\", \"Castle Rock Twp.\", \"Cedar Lake Twp.\", \"Centerville\", \"Champlin\", \"Circle Pines\", \"Coates\", \"Columbia Heights\", \"Columbus\", \"Coon Rapids\", \"Corcoran\", \"Cottage Grove\", \"Crystal\", \"Dayton\", \"Deephaven\", \"Dellwood\", \"Denmark Twp.\", \"Douglas Twp.\", \"Eagan\", \"East Bethel\", \n\"Eden Prairie\", \"Edina\", \"Elko New Market\", \"Empire Twp.\", \"Eureka Twp.\", \"Excelsior\", \"Farmington\", \"Forest Lake\", \"Fridley\", \"Golden Valley\", \"Grant\", \"Greenfield\", \"Greenvale Twp.\", \"Greenwood\", \"Grey Cloud Island Twp.\", \"Ham Lake\", \"Hampton\", \"Hampton Twp.\", \"Hanover\", \"Hastings\", \"Helena Twp.\", \"Hopkins\", \"Hugo\", \"Independence\", \"Inver Grove Heights\", \"Jackson Twp.\", \"Jordan\", \"Lake Elmo\", \"Lake St. Croix Beach\", \"Lakeland\", \"Lakeland Shores\", \"Lakeville\", \"Lexington\", \"Lino Lakes\", \"Linwood Twp.\", \n\"Long Lake\", \"Loretto\", \"Louisville Twp.\", \"Mahtomedi\", \"Maple Grove\", \"Maple Plain\", \"Marine on St. Croix\", \"Marshan Twp.\", \"May Twp.\", \"Medicine Lake\", \"Medina\", \"Mendota\", \"Mendota Heights\", \"Miesville\", \"Minneapolis\", \"Minnetonka\", \"Minnetonka Beach\", \"Minnetrista\", \"Mound\", \"New Hope\", \"New Market Twp.\", \"New Prague\", \"New Trier\", \"Newport\", \"Nininger Twp.\", \"Northfield\", \"Nowthen\", \"Oak Grove\", \"Oak Park Heights\", \"Oakdale\", \"Orono\", \"Osseo\", \"Pine Springs\", \"Plymouth\", \"Prior Lake\", \"Ramsey\", \n\"Randolph\", \"Randolph Twp.\", \"Ravenna Twp.\", \"Richfield\", \"Robbinsdale\", \"Rockford\", \"Rosemount\", \"Sand Creek Twp.\", \"Savage\", \"Scandia\", \"Sciota Twp.\", \"Shakopee\", \"South St. Paul\", \"Spring Lake Park\", \"Spring Lake Twp.\", \"Spring Park\", \"St. Anthony\", \"St. Bonifacius\", \"St. Francis\", \"St. Lawrence Twp.\", \"St. Louis Park\", \"St. Marys Point\", \"St. Paul Park\", \"Stillwater\", \"Stillwater Twp.\", \"Sunfish Lake\", \"Tonka Bay\", \"Vermillion\", \"Vermillion Twp.\", \"Waterford Twp.\", \"Wayzata\", \"West Lakeland Twp.\", \n\"West St. Paul\", \"White Bear Lake\", \"Willernie\", \"Woodbury\", \"Woodland\")"

#ctus where floor area increasing does not change emissions - this seems strange, but okay.
floorarea %>%
  filter(fa_05 == fa_15) %>%
  select(ctu_name) %>%
  paste(collapse= "")
#> [1] "c(\"Credit River Twp.\", \"Fort Snelling (unorg.)\", \"Hilltop\", \"Landfall\", \"Lilydale\", \"Rogers\")"
