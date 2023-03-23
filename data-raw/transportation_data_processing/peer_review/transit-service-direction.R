library(ghg.sp)
library(tidyverse)

transitservice_60 <- run_scenario_transportation(.transit_service_pct = .6, .scenario = "ts60")
transitservice_0 <- run_scenario_transportation(.transit_service_pct = 0, .scenario = "ts0")

transit <- transitservice_60$passenger_all %>%
  bind_rows(transitservice_60$freight_all) %>%
  bind_rows(transitservice_0$passenger_all) %>%
  bind_rows(transitservice_0$freight_all) %>%
  filter(year == "2040") %>%
  group_by(ctu, scenario, year) %>% # mode, sector
  summarise(emissions = sum(dir_ghg, na.rm = T)) %>%
  pivot_wider(names_from = scenario, values_from = emissions)
#> `summarise()` has grouped output by 'ctu', 'scenario'. You can override using
#> the `.groups` argument.

#ctus where more transit increases emissions - this seems problematic
transit %>%
  filter(ts60 > ts0) %>%
  select(ctu) %>%
  paste(collapse= "")
#> [1] "c(\"Andover\", \"Apple Valley\", \"Arden Hills\", \"Bayport\", \"Baytown Twp.\", \"Benton Twp.\", \"Birchwood Village\", \"Blaine\", \"Bloomington\", \"Brooklyn Center\", \"Brooklyn Park\", \"Burnsville\", \"Carver\", \"Centerville\", \"Champlin\", \"Chanhassen\", \"Chaska\", \"Circle Pines\", \"Cologne\", \"Columbia Heights\", \"Columbus\", \"Coon Rapids\", \"Corcoran\", \"Cottage Grove\", \"Credit River Twp.\", \"Crystal\", \"Dahlgren Twp.\", \"Dayton\", \"Deephaven\", \"Dellwood\", \"Denmark Twp.\", \"Eagan\", \"Eden Prairie\", \"Edina\", \"Empire Twp.\", \"Falcon Heights\", \n\"Farmington\", \"Forest Lake\", \"Fort Snelling UT\", \"Gem Lake\", \"Golden Valley\", \"Grant\", \"Greenfield\", \"Grey Cloud Island Twp.\", \"Ham Lake\", \"Hastings\", \"Hilltop\", \"Hopkins\", \"Hugo\", \"Inver Grove Heights\", \"Jackson Twp.\", \"Lake Elmo\", \"Lakeville\", \"Landfall\", \"Lauderdale\", \"Lexington\", \"Lilydale\", \"Lino Lakes\", \"Little Canada\", \"Mahtomedi\", \"Maple Grove\", \"Maplewood\", \"Mayer\", \"Medicine Lake\", \"Medina\", \"Mendota\", \"Mendota Heights\", \"Minneapolis\", \"Minnetonka\", \"Minnetrista\", \"Mound\", \"Mounds View\", \n\"New Brighton\", \"New Hope\", \"Newport\", \"Nininger Twp.\", \"North Oaks\", \"North St. Paul\", \"Oak Park Heights\", \"Oakdale\", \"Orono\", \"Osseo\", \"Pine Springs\", \"Plymouth\", \"Prior Lake\", \"Ramsey\", \"Richfield\", \"Robbinsdale\", \"Rogers\", \"Rosemount\", \"Roseville\", \"San Francisco Twp.\", \"Savage\", \"Shakopee\", \"Shoreview\", \"Shorewood\", \"South St. Paul\", \"Spring Lake Park\", \"Spring Lake Twp.\", \"Spring Park\", \"St. Anthony\", \"St. Louis Park\", \"St. Paul\", \"St. Paul Park\", \"Stillwater\", \"Stillwater Twp.\", \"Sunfish Lake\", \n\"Vadnais Heights\", \"Victoria\", \"Waconia\", \"Waconia Twp.\", \"Wayzata\", \"West Lakeland Twp.\", \"West St. Paul\", \"White Bear Lake\", \"White Bear Twp.\", \"Willernie\", \"Woodbury\", \"Woodland\")"

#ctus where more transit decreases emissions
transit %>%
  filter(ts60 < ts0) %>%
  select(ctu) %>%
  paste(collapse= "")
#> [1] "c(\"Afton\", \"Anoka\", \"Belle Plaine\", \"Belle Plaine Twp.\", \"Bethel\", \"Castle Rock Twp.\", \"Cedar Lake Twp.\", \"Coates\", \"East Bethel\", \"Elko New Market\", \"Eureka Twp.\", \"Excelsior\", \"Fridley\", \"Greenvale Twp.\", \"Greenwood\", \"Hampton\", \"Hampton Twp.\", \"Hanover\", \"Independence\", \"Jordan\", \"Lake St. Croix Beach\", \"Lakeland\", \"Lakeland Shores\", \"Linwood Twp.\", \"Long Lake\", \"Loretto\", \"Louisville Twp.\", \"Maple Plain\", \"Marine on St. Croix\", \"Marshan Twp.\", \"May Twp.\", \"Minnetonka Beach\", \"New Market Twp.\", \n\"New Prague\", \"New Trier\", \"Northfield\", \"Norwood Young America\", \"Nowthen\", \"Oak Grove\", \"Sand Creek Twp.\", \"Scandia\", \"Sciota Twp.\", \"St. Bonifacius\", \"St. Francis\", \"St. Marys Point\", \"Tonka Bay\", \"Vermillion\", \"Vermillion Twp.\", \"Waterford Twp.\", \"Young America Twp.\")"

#ctus where more transit does not impact emissions
transit %>%
  filter(ts60 == ts0) %>%
  select(ctu) %>%
  paste(collapse= "")
#> [1] "c(\"Blakeley Twp.\", \"Camden Twp.\", \"Douglas Twp.\", \"Hamburg\", \"Hancock Twp.\", \"Helena Twp.\", \"Hollywood Twp.\", \"Laketown Twp.\", \"Miesville\", \"New Germany\", \"Randolph\", \"Randolph Twp.\", \"Ravenna Twp.\", \"Rockford\", \"St. Lawrence Twp.\", \"Watertown\", \"Watertown Twp.\")"
