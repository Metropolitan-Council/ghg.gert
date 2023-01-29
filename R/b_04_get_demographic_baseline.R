#' @title Get Demographic Baseline
#' @family buildings
#' @description Compiles the demographic characteristics from the 'Emissions' database
#'     required for running the building energy module.
#'     The purpose of this function is to calculate various demographic characteristics at the county and CTU level,
#'     including the average floor area of single family and multifamily properties,
#'     the number of commercial and industrial workers, the population and number of households, and the number of jobs by industry.
#'     These characteristics are stored in a list and returned by the function.
#'
#' @inheritParams run_scenario_transportation
#' @return Tibble.
#'
#' @export
#'
get_demographic_baseline <- function(tb = building_energy_data) {
  # COUNTY DEMOGRAPHIC BASELINE ----
  message("* obtaining baseline demographic characteristics at the CTU level")
  demographic_characteristics <- c()

  ## ----- get average floor area for single family from ZTRAX in 'Emissions' -----
  county_average_floor_area_single_family <-
    tb$ztrax_sqft_summary_county %>%
    dplyr::select(co_name, property_land_use, mean_sqft) %>%
    dplyr::filter(property_land_use == "Single family residential") %>%
    dplyr::group_by(co_name) %>%
    dplyr::mutate(
      var = "single_family_average_floor_area_sqft_county",
      year = 2018
    ) %>%
    dplyr::rename(value = mean_sqft) %>%
    dplyr::select(co_name, year, var, value)

  ## ----- get average multifamily floor area per county from ZTRAX in 'Emissions' ----
  county_average_floor_area_multifamily <-
    tb$ztrax_sqft_summary_county %>%
    dplyr::select(co_name, property_land_use, mean_sqft) %>%
    dplyr::filter(property_land_use == "Condominium") %>%
    dplyr::mutate(
      var = "multifamily_average_floor_area_sqft_county",
      year = 2018
    ) %>%
    dplyr::rename(value = mean_sqft) %>%
    dplyr::select(co_name, year, var, value)

  ## ----- obtain commercial/industrial workers from 'Emissions' ----
  county_workers <-
    tb$led_industry_county %>%
    dplyr::filter(year == 2018) %>%
    dplyr::filter(jw_indicator == "W") %>%
    dplyr::mutate(
      var =
        dplyr::case_when(
          (industry %in% naics_codes$led_commercial) ~ "commercial_workers_county",
          (industry %in% naics_codes$led_industrial) ~ "industrial_workers_county"
        )
    ) %>%
    dplyr::select(co_name, year, var, count) %>%
    dplyr::group_by(co_name, year, var) %>%
    dplyr::summarise(value = sum(count, na.rm = T), .groups = "keep")

  ## ---- compile county baseline demographic characteristic -----
  demographic_characteristics$county <-
    dplyr::bind_rows(
      county_average_floor_area_single_family,
      county_average_floor_area_multifamily,
      county_workers
    )

  # baseline demographics
  # CTU DEMOGRAPHICS ----

  ## ---- get population & households from 'Emissions'----
  ctu_population <-
    tb$ctu_population %>%
    dplyr::select(ctu_name, year, population, households) %>%
    dplyr::filter(year == 2018) %>%
    dplyr::group_by(ctu_name, year) %>%
    tidyr::pivot_longer(
      cols = c("population", "households"),
      names_to = "var",
      values_to = "value"
    ) %>%
    dplyr::group_by(ctu_name, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T), .groups = "keep")
  # portions of a city that fall in more than one county
  # are aggregated.

  ## ----- get jobs by industry from 'Emissions' -----
  ctu_jobs <-
    tb$ctu_qcew_ctu %>%
    dplyr::filter(naicstitle == "Total, All Industries") %>%
    dplyr::filter(year == 2018) %>%
    dplyr::select(ctu_name, year, emp) %>%
    dplyr::rename(value = emp) %>%
    dplyr::mutate(var = "jobs") %>%
    dplyr::select(ctu_name, year, var, value)


  ## ----- aggregate industrial jobs by NAICS -----
  ctu_industrial_jobs <-
    tb$ctu_qcew_ctu %>%
    dplyr::filter(
      naicstitle %in% c(
        "Natural Resources and Mining",
        "Construction",
        "Trade, Transportation and Utilities"
      )
    ) %>%
    dplyr::filter(year == 2018) %>%
    dplyr::select(ctu_name, year, naicstitle, emp) %>%
    dplyr::group_by(ctu_name, year) %>%
    dplyr::summarise(value = sum(emp, na.rm = TRUE), .groups = "keep") %>%
    dplyr::mutate(var = "industrial_jobs") %>%
    dplyr::select(ctu_name, year, var, value)


  ## ----- aggregate commercial jobs by NAICS ----
  ctu_commercial_jobs <-
    tb$ctu_qcew_ctu %>%
    dplyr::filter(
      naicstitle %in% c(
        "Financial Activities",
        "Professional and Business Services",
        "Education and Health Services",
        "Leisure and Hospitality",
        "Public Administration"
      )
    ) %>%
    dplyr::filter(year == 2018) %>%
    dplyr::select(ctu_name, year, naicstitle, emp) %>%
    dplyr::group_by(ctu_name, year) %>%
    dplyr::summarise(value = sum(emp, na.rm = TRUE), .groups = "keep") %>%
    dplyr::mutate(var = "commercial_jobs") %>%
    dplyr::select(ctu_name, year, var, value)


  ## ----- get forecast of single and multifamily units from 'Emissions' ----
  ctu_housing_stock <-
    tb$forecast_lu_ctu %>%
    dplyr::filter(
      var %in% c("SFD_Units", "MF_Units"),
      year == 2018
    ) %>%
    dplyr::mutate(
      var =
        dplyr::case_when(
          (var == "MF_Units") ~ "multifamily_units",
          (var == "SFD_Units") ~ "single_family_units"
        )
    ) %>%
    dplyr::group_by(ctu_name, year)

  ## ----- estimate single family average floor area from ZTRAX ----
  ctu_average_floor_area_single_family <-
    tb$ztrax_sqft_summary_ctu %>%
    dplyr::select(ctu_name, property_land_use, mean_sqft) %>%
    unique() %>%
    dplyr::filter(property_land_use == "Single family residential") %>%
    dplyr::group_by(ctu_name) %>%
    dplyr::mutate(
      var = "single_family_average_floor_area_sqft_ctu",
      year = 2018,
      value = mean(mean_sqft, na.rm = T)
    ) %>%
    dplyr::select(ctu_name, year, var, value)


  ## ---- get average multifamily floor area from ZTRAX ----
  ctu_average_floor_area_multifamily <-
    tb$ztrax_sqft_summary_ctu %>%
    dplyr::select(ctu_name, property_land_use, mean_sqft) %>%
    dplyr::filter(stringr::str_detect(property_land_use, "Condominium")) %>%
    dplyr::mutate(
      var = "multifamily_average_floor_area_sqft_ctu",
      year = 2018
    ) %>%
    dplyr::rename(value = mean_sqft) %>%
    dplyr::select(ctu_name, year, var, value)


  ## ---- get county multifamily floor area for when ctu equivalent is missing ----
  ctu_county <- demographic_characteristics$county %>%
    dplyr::left_join(tb$ctu_county, by = "co_name") %>%
    dplyr::filter(var == "multifamily_average_floor_area_sqft_county") %>%
    dplyr::group_by(ctu_name, year, var) %>%
    dplyr::mutate(value = value * pct_population) %>%
    dplyr::select(ctu_name, year, var, value) %>%
    dplyr::group_by(ctu_name, year, var) %>%
    dplyr::summarise(value = sum(value), .groups = "keep")
  # uses weighted average based on population

  #---- compile ctu demographic baseline characterics -----
  demographic_characteristics$ctu <-
    dplyr::bind_rows(
      ctu_population,
      ctu_jobs,
      ctu_commercial_jobs,
      ctu_industrial_jobs,
      ctu_housing_stock,
      ctu_average_floor_area_single_family,
      ctu_average_floor_area_multifamily,
      ctu_county
    ) %>%
    unique()

  return(demographic_characteristics)
}
