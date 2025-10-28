#' @title Environmental factors for users
#'
#' @format A named list
#' - **TRANSIT_SERVICE_ELAST**: Effect of increase in transit service on increase in transit ridership and decrease in PLDV. Citation forthcoming.
#' - **TRANSIT_SERVICE_AVO_MIN**: Minimum percent of transit service increase that transit vehicle occupancy must increase by.
#' - **SI_FUEL_COST_GAL**: Gasoline fuel cost in dollars per gallon. EIA 2024 Annual estimate.
#' - **CI_FUEL_COST_GAL**: Diesel fuel cost in dollars per gallon. EIA 2024 Annual estimate.
#' - **ELEC_FUEL_COST_KWH**: Electric fuel cost in dollars per kWh. Regular residential rate, June through September. [Xcel Energy, 2024](https://www.xcelenergy.com/staticfiles/xe-responsive/Company/Rates%20&%20Regulations/24-01-406-MN-Res-ElecRates-MN-Res-E-2002.pdf).
#' - **F_FRACT**: Fraction of truck TVMT inside MSP (i.e., under jurisdiction of application for VMT fee).
#' - **AUTO_COST_MI**: 2024 [AAA driving costs](https://exchange.aaa.com/automotive/aaas-your-driving-costs/) (assume mid-distance of 15,000 miles).
#' - **TIME_COST_MI**: Cents per mile. Time cost per mile informed by [Transportation Cost and Benefit Analysis - Travel Time Costs](https://www.vtpi.org/tca/tca0502.pdf).
#' - **F_TIME_COST_MI**: Cents per mile according to [TTI](https://static.tti.tamu.edu/tti.tamu.edu/documents/TTI-2017-10.pdf).
#' - **INS_COST_MI**: Average annual minimum coverage insurance ($621 via NerdWallet, 2025) divided by annual VMT per person (daily VMT per person (metro, MnDOT 2023) multiplied by annualization factor of 340)
#' - **CONG_VMT**: Congested VMT as a proportion of total VMT.
#' - **BUS_AV**: Factors for % change in bus for a 1% change in AV penetration.
#' - **RAIL_AV**: Factors for % change in rail for a 1% change in AV penetration.
#' - **VMT_AV**: Increase in VMT due to AV.
#' - **EVCS_VMT**: Additional VMT due to charging for PHEV and BEV DRS.
#' - **MPG_AV**: 15% reduction in fuel consumption with AV. Based on:
#'     _Forecasting the Impact of Connected and Automated Vehicles on Energy Use: A Microeconomic Study of Induced Travel and Energy Rebound_ — Taiebat, Stolper, Xu.
#' - **MAX_5D_DR**: Maximum 5D impact on driving.
#' - **MAX_5D_ACT**: Maximum 5D impact on active mode (walk and bike).
#' - **MAX_5D_TRANS**: Maximum 5D impact on transit.
#' - **MARG_TELEWORK**: Telework marginal effect percent change in PMT (per household). From Kim et al. (2015).
#' - **KG_CO2E_PER_THERM_BASELINE**: Kilograms of CO₂ equivalent emitted per therm of natural gas in 2018.
#' - **KG_CO2E_PER_THERM_FORECAST**: Kilograms of CO₂ equivalent emitted per therm of natural gas in 2040.
#' - **KG_CO2E_PER_MHW_BASELINE**: Kilograms of CO₂ equivalent emitted per megawatt hour of electricity in 2018.
#' - **KG_CO2E_PER_MHW_FORECAST**: Kilograms of CO₂ equivalent emitted per megawatt hour of electricity in 2040.
#' - **LEED_GOLD_REDUCTION_PCT**: Reduction in single family home energy use intensity (EUI) by building to LEED Gold (25 kBTU/sf).
#' - **EXISTING_HOME_RETROFIT_REDUCTION_PCT**: Reduction in residential energy use intensity via performance-based high efficiency retrofits.
#' - **EXISTING_HOME_ULTRA_RETROFIT_REDUCTION_PCT**: Reduction in residential energy use via high passive housing retrofits.
#' - **BEHAVIOR_CHANGE_REDUCTION_PCT**: Household energy reduction from messaging, in-home displays, and smart meters.
#' - **SMART_GRID_IMPACT**: Percentage reduction in electricity use from non-residential smart grid implementation.
#'
#' @family datasets
# enviro_factors -----
"enviro_factors"


#' @title General elasticities and cross elasticities
#'
#' @description Values are specific to forecast year
#' @format A tibble with 9 columns and 10 observations.
#' - **year**: Forecast year.
#' - **vmt_elast**: Elasticity for VMT pricing effect on VMT. INFRAS (2000) and Luk (1999) for range; Hymel and Small (2015) for mean.
#' - **gas_elast**: Elasticity for gas tax effect on VMT. Goodwin, Dargay, and Hanly (2003) for range; Small (2007) for mean.
#' - **cong_elast**: Elasticity for congestion pricing effect on VMT. TRACE (1999) and Litman (2019).
#' - **park_elast**: Elasticity for parking pricing effect on VMT. TRACE (1999) and Litman (2019).
#' - **freight_vmt_elast**: Elasticity for freight vehicle pricing effect on freight VMT. Small and Winston (1999), quoted in Litman (2011).
#' - **vehicle_ownership_elast**: Elasticity for vehicle ownership in response to price changes.
#' - **vmt_cross**: Cross elasticity for transit/walk/bike with respect to PLDV VMT price. Affects VMT. Litman (2019). [Source](https://www.vtpi.org/elasticities.pdf)
#' - **park_active**: Elasticity for parking price effect on active transportation VMT. TRACE (1999).
#' - **park_transit**: Elasticity for parking price effect on transit VMT. TRACE (1999).
#'
#' @family datasets
#' @examples
#' library(ghg.ccap)
#' elast
# elast -----
"elast"


#' @title 5D elasticities
#'
#' @description Values are specific to forecast year
#' @format A tibble with 27 columns and 9 observations.
#' - **year**: Forecast year.
#' - **type**: Transportation mode. One of `"DRIVE"`, `"WALK"`, or `"TRANSIT"`.
#' - **population_density**: Elasticity for population density effect on VMT.
#' - **employment_density**: Elasticity for employment population density effect on VMT.
#' - **diversity**: Elasticity for land use diversity effect on VMT.
#' - **design**: Elasticity for intersection design effect on VMT.
#' - **job_access**: Elasticity for job accessibility via transit effect on VMT.
#' - **distance**: Elasticity for minimum distance to transit stops effect on VMT.
#' - **combined_density**: Combined effect of all land use elasticities.
#'
#' @family datasets
#' @examples
#' library(ghg.ccap)
#' elast_5d
# elast_5d-----
"elast_5d"

#' @title Annual energy outlook, cost, and greenhouse gas factors
#' @description  Annual energy outlook, cost, and greenhouse gas factors
#'     by scenario, mode, year, and source
#' @format A list with the following elements:
#' - **aeo**: A tibble with columns `aeo_scen`, `mode`, `metric`, `year`, `value`.
#' - **cost**: A tibble with columns `mode`, `var`, `is_av`, `year`, `value`.
#' - **ghg**: A tibble with columns `source`, `year`, `value`.
#'
#' @family datasets
# factor_values-----
"factor_values"


#' @title A list of transportation and freight data input tables
#'
#' @format A named list of two tibbles
#' - **passenger**: A tibble with columns:
#'   `mode`, `var`, `geog_name`, `geog_id`,  `year`, `value`, `aeo_mode`, `type`.
#' - **freight**: A tibble columns:
#'   `mode`, `var`, `geog_name`, `geog_id`, `year`, `value`, `aeo_mode`, `type`.
#'
#' @family datasets
#'
#' @examples
#' library(ghg.ccap)
#' transportation_data$passenger
#' transportation_data$freight
# transportation_data -----
"transportation_data"



#' @title Reference index for abbreviations, data sources, identifiers
#'
#' @format A list of tibbles with identifiers, abbreviations, and descriptions
#'     for each emission source, variable, transportation mode, and AEO scenario.
#' - **emission_sources**: A tibble with columns `source_id`, `source_abbrev`, and `source_description`.
#' - **variables**: A tibble with columns `var_id`, `var_name`, `var_description`, and `var_description_2`.
#' - **modes**: A tibble with columns `mode_id`, `mode_abbrev`, `mode_description_1`, and `mode_description_2`.
#' - **aeo**: A tibble with columns `aeo_scen`, `name`, and `description`.
#' - **data_sources**: A tibble with columns `mode`, `var`, `source`, `source_1`, `source_2`, `source_3`, and `source_short`.

#'
#' @family datasets
#' @examples
#' library(ghg.ccap)
#' transportation_index$emission_sources
#' transportation_index$variables
#' transportation_index$modes
#' transportation_index$data_sources
# transportation_index -----
"transportation_index"

#' @title Database table names
#'
#' @format Nested, named list of table names available in the CD_Emissions database
#' - **mod_1**: A named list of 1 table name.
#' - **mod_2**: A named list of 3 table names.
#' - **mod_3**: A named list of 8 table names.
#' - **metro_demos**: A named list of 7 table names.
#' - **state_demos**: A named list of 2 table names.
#' - **metro_energy**: A named list of 9 table names.
#' - **state_energy**: A named list of 1 table name.
#'
#' @family datasets
#' @examples
#' library(ghg.ccap)
#' db_table_names$mod_1
#' db_table_names$metro_demos
# db_table_names -----
"db_table_names"

#' @title North American Industry Classification System (NAICS) and Local Employment Dynamics (LED) codes
#'
#' @format Nested, named list
#' - **commercial** NAICS codes considered commercial
#' - **industrial** NAICS codes considered industrial
#' - **led_commercial** NAICS codes considered commercial for the LED dataset
#' - **led_industrial** NAICS codes considered industrial for the LED dataset
#'
#' @family datasets
#' @examples
#' library(ghg.ccap)
#' naics_codes$commercial
#' naics_codes$industrial
# naics_codes -----
"naics_codes"


#' @title Fuel economy
#' @format tibble
#' - **year** Forecast year.
#' - **mode** Transportation mode
#' - **aeo_mod** Equivalent Annual Energy Outlook mode
#' - **var** Fuel type specific variable
#' - **value** Fuel economy value
#' - **metadata** Metadata, where available
#' @family datasets
#' @examples
#' library(ghg.ccap)
# fuel_economy -----
"fuel_economy"




#' @title Geographic index of cities, names, types
#' @format tibble
#' - **ctu_name** CTU name, concurrent with original data tables
#' - **geog_name** Geographic name
#' - **geog_level** City, township, unorganized territory, county
#' - **geog_id** ID value
#' - **geog_id_type** ID value type
#' @family datasets
#' @examples
#' library(ghg.ccap)
#' geog_index
# geog_index -----
"geog_index"


#' @title Land cover type by county and year (2001 to 2021)
#'
#' @format Single dataframe
#' \describe{
#'   \item{county_id}{County GEOID (5-digit)}
#'   \item{county_name}{County name}
#'   \item{state_name}{State name}
#'   \item{inventory_year}{Year}
#'   \item{land_cover_main}{Land cover type (Built-Up includes Urban_Tree,
#'   Urban_Grassland, and the remaining Built-Up)}
#'   \item{area}{Land cover area in square kilometers}
#'   \item{total_area}{Total area by county}
#'   \item{tcc_available}{Tree canopy data available for current year}
#'   \item{source}{Indicates whether data come directly from NLCD or are
#'   extrapolated values}
#' }
#'
#' @family datasets
#' @examples
#' library(ghg.ccap)
#' unique(lc_county$land_cover_type)
#' unique(lc_county$county_name)
# lc_county -----
"lc_county"



#' @title Future city and county demographic information from UrbanSim
#' @format tibble
#' - **inventory_year** year
#' - **geog_name** Geographic name
#' - **geog_level** City, township, unorganized territory, county
#' - **geog_id** ID value
#' - **geog_id_type** ID value type
#' - **sp_categories** alue category. One of "households", "jobs", "multifamily_units", "population", "single_family_units"
#' - **value** value
#' @family datasets
#' @examples
#' library(ghg.ccap)
#' demographic_data
# demographic_data -----
"demographic_data"



#' @title A list of solid waste activity data tables
#'
#' @format A named list of five tibbles
#' \describe{
#'   \item{ctu$baseline}{tibble with 20,088 rows and 8 columns, `inventory_year`,
#'       `geog_id`, `geog_name`, `geog_level`, `source`, `value_activity`,
#'       `units_activity`, `data_type`}
#'   \item{ctu$projections}{tibble with 31,248 rows and 8 columns, `inventory_year`,
#'       `geog_id`, `geog_name`, `geog_level`, `source`, `value_activity`,
#'       `units_activity`, `data_type`}
#'   \item{county$baseline}{tibble with 756 rows and 8 columns, `inventory_year`,
#'       `geog_id`, `geog_name`, `geog_level`, `source`, `value_activity`,
#'       `units_activity`, `data_type`}
#'   \item{county$projections}{tibble with 1,176 rows and 8 columns, `inventory_year`,
#'       `geog_id`, `geog_name`, `geog_level`, `source`, `value_activity`,
#'       `units_activity`, `data_type`}
#'   \item{characterization}{tibble with 50 rows and 5 columns, `Material`,
#'   `Mean`, `Lower`, `Upper`, `Category`}
#' }
#' @family datasets
#'
#' @source https://data.pca.state.mn.us/views/SCOREoverview/SCOREOverview
#'
#' @examples
#' library(ghg.ccap)
#' waste_data$ctu$baseline
#' waste_data$ctu$projections
#' waste_data$county$baseline
#' waste_data$county$projections
#' waste_data$characterization
# waste_data -----
"waste_data"
