#' @title Environmental factors for users
#'
#' @format A named list of 19 values.
#' \describe{
#'   \item{TRANSIT_SERVICE_ELAST}{Effect of increase in transit service on
#'       increase in transit ridership and decrease in PLDV. Citation forthcoming.}
#'   \item{TRANSIT_SERVICE_AVO_MIN}{Minimum percent of transit service increase
#'     that transit vehicle occupancy must increase by.}
#'   \item{SI_FUEL_COST_GAL}{Gasoline fuel cost in cents per gal
#'       https://www.eia.gov/dnav/pet/pet_pri_gnd_a_epm0_pte_dpgal_a.htm
#'       (about 8.8 cents per mile, so lower than Barnes estimate because mpg went up)}
#'   \item{CI_FUEL_COST_GAL}{Diesel fuel in cents per gal
#'       https://www.eia.gov/dnav/pet/pet_pri_gnd_a_epm0_pte_dpgal_a.htm
#'       (about 44 cents per mile, so about equal to Barnes estimate)}
#'   \item{ELEC_FUEL_COST_KWH}{Electric fuel cost in cents per kWh
#'       https://www.xcelenergy.com/staticfiles/xe/PDF/Marketing/MN-SST-Interim-Rates.pdf}
#'   \item{F_FRACT}{Fraction of truck TVMT inside MSP (i.e., under jurisdiction of application for VMT fee)}
#'   \item{AUTO_COST_MI}{https://exchange.aaa.com/automotive/driving-costs/#.YG7-L-hKiUk
#'       (assume mid-distance of 15,000 miles)}
#'   \item{TIME_COST_MI}{cents per mile according to https://www.vtpi.org/tca/tca0502.pdf
#'       and adjusted to 2015 using average CPI}
#'   \item{F_TIME_COST_MI}{cents per mile according to
#'       https://static.tti.tamu.edu/tti.tamu.edu/documents/TTI-2017-10.pdf}
#'   \item{INS_COST_MI}{https://www.forbes.com/advisor/car-insurance/state/minnesota/ and
#'       https://www.dot.state.mn.us/traffic/data/reports/vmt/92-17_per_capita_vmt.pdf}
#'   \item{CONG_VMT}{Congested VMT as a proportion of total VMT}
#'   \item{BUS_AV}{Factors for % change in bus for a 1% change in AV penetration}
#'   \item{RAIL_AV}{Factors for % change in rail for a 1% change in AV penetration}
#'   \item{VMT_AV}{Increase in VMT due to AV}
#'   \item{EVCS_VMT}{Need to account for additional VMT due to charging for PHEV and BEV DRS}
#'   \item{MPG_AV}{15% reduction in consumption of fuel with AV based on
#'       Forecasting the Impact of Connected and Automated Vehicles on Energy
#'       Use: A Microeconomic Study of Induced Travel and Energy Rebound
#'       Morteza Taiebata,b, Samuel Stolpera, Ming Xua,b}
#'   \item{MAX_5D_DR}{Maximum 5D impact on driving}
#'   \item{MAX_5D_ACT}{Maximum 5D impact on active mode (walk and bike)}
#'   \item{MAX_5D_TRANS}{Maximum 5d impact on transit}
#'   \item{MARG_TELEWORK}{Telework marginal effect percent change in PMT (per household). From Kim et al. (2015)}
#'   \item{KG_CO2E_PER_THERM_BASELINE}{Kilograms of CO_2_ equivalent emitted
#'       per therm of natural gas in 2018}
#'   \item{KG_CO2E_PER_THERM_FORECAST}{Kilograms of CO_2_ equivalent emitted
#'       per therm of natural gas in 2040}
#'   \item{KG_CO2E_PER_MHW_BASELINE}{Kilograms of CO_2_ equivalent emitted
#'       per megawatt hour of electricity in 2018}
#'   \item{KG_CO2E_PER_MHW_FORECAST}{Kilograms of CO_2_ equivalent emitted
#'       per megawatt hour of electricity in 2040}
#'   \item{LEED_GOLD_REDUCTION_PCT}{Reduction in single family home energy use intensity (EUI) by building according to LEED Gold standards  (25 kBTU/sf).}
#'   \item{EXISTING_HOME_RETROFIT_REDUCTION_PCT}{Reduction in residential energy use intensity by retrofitting to performance-based high efficiency standards.}
#'   \item{EXISTING_HOME_ULTRA_RETROFIT_REDUCTION_PCT}{Reduction in residential energy use intensity by retrofitting to high passive housing standards.}
#'   \item{BEHAVIOR_CHANGE_REDUCTION_PCT}{Reduction in household energy usage by recieving effective messaging, use an in-home energy usage display, and smart meters.}
#' }
#'
#' @family datasets
# enviro_factors -----
"enviro_factors"


#' @title General elasticities and cross elasticities
#'
#' @description Values are specific to forecast year
#' @format A tibble with 9 columns and 10 observations.
#' \describe{
#'   \item{year}{Forecast year}
#'   \item{vmt_elast}{Elasticity for VMT pricing effect on VMT. INFRAS (2000) and Luk (1999) for range and Hymel and Small (2015) for mean}
#'   \item{gas_elast}{Elasticity for gas tax effect on VMT. Goodwin, Dargay, and Hanly (2003) for range and Small (2007) for mean}
#'   \item{cong_elast}{Elasticity for congestion pricing effect on VMT. TRACE (1999) and Litman (2019)}
#'   \item{park_elast}{Elasticity for parking pricing effect on VMT. TRACE (1999) and Litman (2019)}
#'   \item{freight_vmt_elast}{Elasticity for freight vehicle pricing effect on freight VMT. Small and Winston (1999) quoted in (Litman 2011)}
#'   \item{vehicle_ownership_elast}{Elasticity for vehicle ownership in response to price changes}
#'   \item{vmt_cross}{Cross elasticity for transit/walk/bike with regard to PLDV VMT price. Affects VMT.
#'       (Litman 2019). https://www.vtpi.org/elasticities.pdf}
#'   \item{park_active}{Elasticity for parking price effect on active transportation VMT TRACE (1999)}
#'   \item{park_transit}{Elasticity for parking price effect on transit VMT. TRACE (1999)}
#' }
#'
#' @family datasets
#' @examples
#' library(ghg.sp)
#' elast
# elast -----
"elast"


#' @title 5D elasticities
#'
#' @description Values are specific to forecast year
#' @format A tibble with 27 columns and 9 observations.
#' \describe{
#'   \item{year}{Forecast year}
#'   \item{type}{Transportation mode. One of `"DRIVE"`, `"WALK"`, or `"TRANSIT`}
#'   \item{population_density}{Elasticity for population density effect on VMT}
#'   \item{employment_density}{Elasticity employment population density effect on VMT}
#'   \item{diversity}{Elasticity for land use diversity effect on VMT}
#'   \item{design}{Elasticity for intersection design effect on VMT}
#'   \item{job_access}{Elasticity for job accessibility via transit effect on VMT}
#'   \item{distance}{Elasticity for minimum distance to transit stops effect on VMT}
#'   \item{combined_density}{Combined effect of all land use elasticities}
#' }
#'
#' @family datasets
#' @examples
#' library(ghg.sp)
#' elast_5d
# elast_5d-----
"elast_5d"

#' @title Annual energy outlook, cost, and greenhouse gas factors
#' @description  Annual energy outlook, cost, and greenhouse gas factors
#'     by scenario, mode, year, and source
#' @format A named list of three tibbles
#' \describe{
#'   \item{aeo}{tibble with columns `aeo_scen`, `mode`, `metric`, `year`, `value`}
#'   \item{cost}{tibble with columns `mode`, `var`, `is_av`, `year`, `value`}
#'   \item{ghg}{tibble with columns `source`, `year`, `value`}
#' }
#'
#' @family datasets
# factor_values-----
"factor_values"


#' @title A list of transportation and freight data input tables
#'
#' @format A named list of two tibbles
#' \describe{
#'   \item{passenger}{tibble with 105,652 rows and 7 columns, `mode`,
#'       `var`, `ctu`, `year`, `value`, `aeo_mode`, `type`}
#'   \item{freight}{tibble with 43,362 rows and 7 columns,
#'       `mode`, `var`, `ctu`, `year`, `value`,`aeo_mode`, `type`}
#' }
#' @family datasets
#'
#' @examples
#' library(ghg.sp)
#' transportation_data$passenger
#' transportation_data$freight
# transportation_data -----
"transportation_data"



#' @title Reference index for abbreviations
#'
#' @format A list of tibbles with identifiers, abbreviations, and descriptions
#'     for each emission source, variable, transportation mode, and AEO scenario.
#' \describe{
#'   \item{emission_sources}{tibble with columns `source_id`, `source_abbrev`,
#'       and `source_description`}
#'   \item{variables}{tibble with columns `var_id`, `var_name`,
#'       `var_description`, and `var_description_2`}
#'   \item{modes}{tibble with columns `mode_id`, `mode_abbrev`,
#'       `mode_description_1`, and `mode_description_2`}
#'   \item{aeo}{tibble with columns `aeo_scen`, `name`, and `description`}
#'   \item{data_sources}{tibble with columns `mode`, `var`, `source`, `source_1`,
#'       `source_2`, `source_3`, and `source_short`}
#' }
#'
#' @family datasets
#' @examples
#' library(ghg.sp)
#' transportation_index$emission_sources
#' transportation_index$variables
#' transportation_index$modes
#' transportation_index$data_sources
# transportation_index -----
"transportation_index"

#' @title Database table names
#'
#' @format Nested, named list of table names available in the CD_Emissions database
#' \describe{
#'   \item{mod_1}{named list of 1 table name}
#'   \item{mod_2}{named list of 3 table names}
#'   \item{mod_3}{named list of 8 table names }
#'   \item{metro_demos}{named list of 7 table names}
#'   \item{state_demos}{named list of 2 table names}
#'   \item{metro_energy}{named list of 9 table names}
#'   \item{state_energy}{named list of 1 table name}
#' }
#'
#' @family datasets
#' @examples
#' library(ghg.sp)
#' db_table_names$mod_1
#' db_table_names$metro_demos
# db_table_names -----
"db_table_names"

#' @title North American Industry Classification System (NAICS) and Local Employment Dynamics (LED) codes
#'
#' @format Nested, named list
#' \describe{
#'   \item{commercial}{NAICS codes considered commercial}
#'   \item{industrial}{NAICS codes considered industrial}
#'   \item{led_commercial}{NAICS codes considered
#'       commercial for the LED dataset}
#'   \item{led_industrial}{NAICS codes considered industrial
#'       for the LED dataset}

#' }
#'
#' @family datasets
#' @examples
#' library(ghg.sp)
#' naics_codes$commercial
#' naics_codes$industrial
# naics_codes -----
"naics_codes"
