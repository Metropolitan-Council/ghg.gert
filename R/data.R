#' Environmental factors for users
#'
#' @format A named list of 19 values.
#' \describe{
#'   \item{PLDV_TRANSIT_RATIO}{ 100 transit trips replace 47 LDV trips (APTA, 2009)}
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
#' }
#'
#' @family datasets
"enviro_factors"


#' General elasticities and cross elasticities
#'
#'
#' @format A tibble with 9 columns and 10 observations.
#' \describe{
#'   \item{year}{forecast year}
#'   \item{vmt_elast}{INFRAS (2000) and Luk (1999) for range and Hymel and Small (2015) for mean}
#'   \item{gas_elast}{Goodwin, Dargay, and Hanly (2003) for range and Small (2007) for mean}
#'   \item{cong_elast}{TRACE (1999) and Litman (2019)}
#'   \item{park_elast}{TRACE (1999) and Litman (2019)}
#'   \item{freight_vmt_elast}{Small and Winston (1999) quoted in (Litman 2011)}
#'   \item{vehicle_ownership_elast}{}
#'   \item{vmt_cross}{Cross elasticity for transit/walk/bike with regard to PLDV price (VMT)
#'       (Litman 2019. https://www.vtpi.org/elasticities.pdf)}
#'   \item{park_active}{TRACE (1999)}
#'   \item{park_transit}{TRACE (1999)}
#' }
#'
#' @family datasets
"elast"


#' Annual energy outlook, cost, and greenhouse gas factors
#'     by scenario, mode, year, and source
#'
#'
#' @format A named list of three tibbles
#' \describe{
#'   \item{aeo}{tibble with columns `aeo_scen`, `mode`, `metric`, `year`, `value`}
#'   \item{cost}{tibble with columns `mode`, `var`, `is_av`, `year`, `value`}
#'   \item{ghg}{tibble with columns `source`, `year`, `value`}
#' }
#'
#' @family datasets
"factor_values"


#' A list of transportation and freight data input tables
#'
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
"transportation_data"



#' Reference index for abbreviations
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
#' }
#'
#' @family datasets
#' @examples
#' library(ghg.sp)
#' transportation_index$emission_sources
#' transportation_index$variables
#' transportation_index$modes
"transportation_index"
