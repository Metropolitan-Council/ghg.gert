#' @title Environmental factors for users
#'
#' @format A named list
#' - **TRANSIT_SERVICE_ELAST**: Effect of increase in transit service on increase in transit ridership and decrease in PLDV. Citation forthcoming.
#' - **TRANSIT_SERVICE_AVO_MIN**: Minimum percent of transit service increase that transit vehicle occupancy must increase by.
#' - **SI_FUEL_COST_GAL**: Gasoline fuel cost in dollars per gallon. EIA 2024 Annual estimate.
#' - **CI_FUEL_COST_GAL**: Diesel fuel cost in dollars per gallon. EIA 2024 Annual estimate.
#' - **ELEC_FUEL_COST_KWH**: Electric fuel cost in dollars per kWh. Regular residential rate, June through September. [Xcel Energy, 2024](https://www.xcelenergy.com/staticfiles/xe-responsive/Company/Rates%20&%20Regulations/24-01-406-MN-Res-ElecRates-MN-Res-E-2002.pdf).
#' - **F_FRACT**: Fraction of truck TVMT inside MSP (i.e., under jurisdiction of application for VMT fee).
#' - **MAX_PARKING_REDUCTION_PCT**: Maximum parking pricing policy impact: -30% reduction in VMT. Citation: CAPCOA Handbook.
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
#' - **CBTP_PARTICIPATION_PCT**: Community Based Travel Planning: proportion of targeted residences that participate. CAPCOA Handbook
#' - **CBTP_TRIP_REDUCTION_PCT**:  Community Based Travel Planning: vehicle trip reduction by participating residences. CAPCOA Handbook
#' - **MAX_TRIP_REDUCTION_PCT**: Trip reduction program: maximum VMT reduction cap (2.3%). Citation: CAPCOA Handbook
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
#' library(ghg.gert)
#' elast
# elast -----
"elast"


#' @title 5D elasticities
#'
#' @description Values are specific to forecast year
#' @format A tibble with 27 columns and 10 observations.
#' - **year**: Forecast year.
#' - **type**: Transportation mode. One of `"DRIVE"`, `"WALK"`, or `"TRANSIT"`.
#' - **population_density**: Elasticity for population density effect on VMT.
#' - **employment_density**: Elasticity for employment population density effect on VMT.
#' - **diversity**: Elasticity for land use diversity effect on VMT.
#' - **design**: Elasticity for intersection design (% 4-way stops) effect on VMT.
#' - **intersection_density**: Elasticity for intersection density (intersections per square mile) effect on VMT.
#' - **job_access**: Elasticity for job accessibility via transit effect on VMT.
#' - **distance**: Elasticity for minimum distance to transit stops effect on VMT.
#' - **combined_density**: Combined effect of all land use elasticities.
#'
#' @family datasets
#' @examples
#' library(ghg.gert)
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
#' library(ghg.gert)
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
#' library(ghg.gert)
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
#' library(ghg.gert)
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
#' library(ghg.gert)
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
#' library(ghg.gert)
# fuel_economy -----
"fuel_economy"


#' @title Geographic index of cities, townships, counties, and region
#' @description One row per geography available in the tool: CTUs (cities,
#'   townships, unorganized territory) in the seven-county metro, the seven
#'   counties, the Twin Cities region, and two township groups ("Carver
#'   Townships", "Scott Townships") that aggregate each county's townships.
#'   Township group membership and designations are defined in
#'   `data-raw/meta/compile_townships.R`.
#' @format tibble
#' - **geog_name** Geographic name (townships carry a " Twp." suffix)
#' - **geog_short_name** Short name
#' - **geog_level** `"CITY"`, `"TOWNSHIP"`, `"UNORGANIZED"`, `"COUNTY"`,
#'   `"REGION"`, or `"TOWNSHIP_GROUP"`
#' - **geog_id** ID value: GNIS ID (CTUs), county FIPS, `"00000000"` (region),
#'   or county FIPS + `"T"` (township groups)
#' - **imagine_designation** Imagine 2050 community designation (CTUs and
#'   township groups), or `"County"` / `"Region"`
#' - **geog_id_type** ID value type: `"ctu_gnis"`, `"county_fips"`, `"region"`,
#'   or `"township_group"`
#' @family datasets
#' @examples
#' library(ghg.gert)
#' geog_index
# geog_index -----
"geog_index"


#' @title Boundaries for every geography in geog_index
#' @description Simple features object with one (multi)polygon per row of
#'   [geog_index]. CTU and county boundaries are from the Census (via
#'   councilR); CTUs split by a county line are unioned to a single feature,
#'   and the region and township groups are unioned from their parts.
#'   Built in `data-raw/meta/compile_geog_geometry.R`.
#' @format sf object (MULTIPOLYGON, NAD83 / UTM zone 15N, EPSG:26915) with the
#'   [geog_index] columns plus **geometry**.
#' @family datasets
#' @examples
#' library(ghg.gert)
#' geog_geometry
# geog_geometry -----
"geog_geometry"


#' @title Share of each CTU's area in each county
#' @description One row per CTU-county piece, for assigning CTUs (and township
#'   groups) to counties, e.g. in [county_assign()]. Township groups each sit
#'   wholly within one county. Built in `data-raw/meta/compile_geog_geometry.R`.
#' @format tibble
#' - **geog_name** CTU or township group name
#' - **county_name** County name
#' - **geog_id** CTU GNIS ID or township group ID
#' - **pct_of_ctu_area** Share of the CTU's area in that county (0 to 1)
#' @family datasets
#' @examples
#' library(ghg.gert)
#' ctu_county_area
# ctu_county_area -----
"ctu_county_area"


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
#' library(ghg.gert)
#' unique(lc_county$land_cover_type)
#' unique(lc_county$county_name)
# lc_county -----
"lc_county"


#' @title Annual demographic data for CTUs, counties, region, and township groups
#' @description UrbanSim projections (2005 to 2050, interpolated annually),
#'   scaled to Imagine 2050 local forecasts. Township groups are the sum of
#'   their member townships after scaling.
#' @format tibble
#' - **emissions_year** Year
#' - **geog_name** Geographic name
#' - **geog_level** See [geog_index]
#' - **geog_id** ID value
#' - **geog_id_type** ID value type
#' - **sp_categories** Value category. One of `"population"`,
#'   `"total_households"`, `"single_family_detached"`,
#'   `"single_family_attached"`, `"multifamily_units"`, `"manufactured_homes"`,
#'   `"jobs"`, `"commercial_jobs"`, `"industrial_jobs"`,
#'   `"commercial_job_space"`, `"industrial_job_space"`
#' - **value** Value
#' - **value_change_from_base** Change in value from 2022
#' @family datasets
#' @examples
#' library(ghg.gert)
#' demographic_data
# demographic_data -----
"demographic_data"


#' @title Waste activity data (solid waste and wastewater)
#'
#' @description County solid waste from MPCA SCORE reports through the
#'   reference year (2022), downscaled to CTUs by population using each
#'   county's per capita rates; projections carry 2022 per capita rates
#'   forward. Wastewater rows use population as the activity scalar. Includes
#'   CTUs, counties, the region (sum of counties), and township groups (sum of
#'   member townships). Built in `data-raw/waste_data_processing/waste_data.R`.
#'
#' @format A named list
#' \describe{
#'   \item{inventory}{tibble of annual activity through 2022 with columns
#'       `inventory_year`, `geog_id`, `geog_name`, `geog_level`, `geog_pop`,
#'       `source`, `value_activity`, `units_activity`, `data_type`}
#'   \item{projections}{tibble of annual activity after 2022, same columns as
#'       `inventory`}
#'   \item{solid_waste_baseline}{tibble of 2022 solid waste by source with
#'       `pct_of_total`, MPCA 2030 targets (`target2030`), `changeFromTarget`,
#'       and `action`}
#'   \item{mpca}{list of MPCA targets: `waste_reduction` (reduction by year)
#'       and `source_diversions` (target share by source and year)}
#'   \item{characterization}{tibble of MPCA waste composition with columns
#'       `Material`, `Mean`, `Lower`, `Upper`, `Category`}
#'   \item{epa}{list of EPA wastewater inputs: `wastewater_constants` and
#'       `protein_consumption`}
#' }
#' @family datasets
#'
#' @source https://data.pca.state.mn.us/views/SCOREoverview/SCOREOverview
#'
#' @examples
#' library(ghg.gert)
#' waste_data$inventory
#' waste_data$projections
#' waste_data$solid_waste_baseline
#' waste_data$characterization
# waste_data -----
"waste_data"


#' @title Default values for transportation inputs
#' @format named list
#' @family datasets
#' @examples
#' library(ghg.gert)
#' transportation_defaults
# transportation_defaults -----
"transportation_defaults"

#' @title Commute share of passenger light-duty VMT
#'
#' @description Estimated proportion of passenger light-duty vehicle miles
#' traveled that is attributable to commute trips, by geography. Estimates are
#' derived from the Travel Behavior Inventory and the geography community designation (for cities),
#' or geography level (for counties and region).
#'
#' In cases where there were no commute data for a given community designation,
#' CTUs were assigned the region-level estimate. Township groups take the
#' estimate for their assigned designation (see [geog_index]).
#'
#' @format A tibble with 6 columns.
#' - **mode**: Transportation mode, `"PLDV"`.
#' - **var**: Variable name, `"COMMUTE_VMT_PROP"`.
#' - **geog_name**: Geographic name.
#' - **geog_id** ID value
#' - **value**: Proportion of passenger light-duty VMT attributable to commute trips.
#' - **type**: Data type, `"P"`.
#' @family datasets
#' @examples
#' library(ghg.gert)
#' commute_vmt_proportion
# commute_vmt_proportion -----
"commute_vmt_proportion"

#' @title EPA Smart Location Database (2021) edition intersection density data aggregated to CTU level.
#' @description Compiled from the EPA Smart Location Database (SLD) 2021 edition, this dataset provides intersection density metrics aggregated from
#'   2019 block groups (GEOID20) to the CTU level. Intersection density is aggregated using a weighted mean based on the area of the intersected block groups.
#'   EPA cites 2018 HERE Maps and NAVSTREETS as data sources and uses acres of land as the denominator for intersection density calculations.
#'   Counties, the region, and township groups are area-weighted over the same block group pieces.
#'   Zotero key chapmanSmartLocationDatabase2021
#' @format A tibble with 4 columns.
#' - **geog_id**: ID value (see [geog_index])
#' - **geog_name**: Geographic name
#' - **geog_id_type**: ID value type
#' - **intersection_density**: Total road network density measured in intersections per square mile
#' @family datasets
# epa_sld_intersection_density -----
"epa_sld_intersection_density"
