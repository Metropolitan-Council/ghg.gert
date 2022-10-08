#' Get Demographic Baseline
#'
#' @param tb
#'
#' @return
#' @export
#'
#' @examples
get_demographic_baseline <- function(tb = building_energy_data){

  # COUNTY DEMOGRAPHIC BASELINE ----

  ## ----- get average floor area for single family from ZTRAX in 'Emissions' -----
  county_average_floor_area_single_family <-
   tb$ztrax_sqft_summary_county %>%
    select(co_name, property_land_use, mean_sqft) %>%
    filter(property_land_use == "Single family residential") %>%
    group_by(co_name) %>%
    mutate(
      var = "single_family_average_floor_area_sqft_county",
      year = 2018
    ) %>%
    rename(value = mean_sqft) %>%
    select(co_name, year, var, value)

  ## ----- get average multifamily floor area per county from ZTRAX in 'Emissions' ----
  county_average_floor_area_multifamily <-
    tb$ztrax_sqft_summary_county %>%
    select(co_name, property_land_use, mean_sqft) %>%
    filter(property_land_use == "Condominium") %>%
    mutate(
      var = "multifamily_average_floor_area_sqft_county",
      year = 2018
    ) %>%
    rename(value = mean_sqft) %>%
    select(co_name, year, var, value)

  ## ----- obtain commercial/industrial workers from 'Emissions' ----
  county_workers <-
    tb$led_industry_county %>%
    filter(year == 2018) %>%
    filter(jw_indicator == "W") %>%
    mutate(
      var =
        case_when(
          (industry %in% naics_codes$led_commercial) ~ "commercial_workers_county",
          (industry %in% naics_codes$led_industrial) ~ "industrial_workers_county"
        )
    ) %>%
    select(co_name, year, var, count) %>%
    group_by(co_name, year, var) %>%
    summarise(value = sum(count, na.rm = T), .groups = "keep")

  ## ---- compile county baseline demographic characteristic -----
  county_characteristics <-
    bind_rows(
      county_average_floor_area_single_family,
      county_average_floor_area_multifamily,
      county_workers
    )

  # baseline demographics
  # CTU DEMOGRAPHICS ----

  ## ---- get population & households from 'Emissions'----
  ctu_population <-
    tb$ctu_population %>%
    select(ctu_name, year, population, households) %>%
    filter(year == 2018) %>%
    group_by(ctu_name, year) %>%
    pivot_longer(
      cols = c("population", "households"),
      names_to = "var",
      values_to = "value"
    ) %>%
    group_by(ctu_name, year, var) %>%
    summarise(value = sum(value, na.rm = T), .groups = "keep")
  # portions of a city that fall in more than one county
  # are aggregated.

  ## ----- get jobs by industry from 'Emissions' -----
  ctu_jobs <-
    tb$ctu_qcew_ctu %>%
    filter(naicstitle == "Total, All Industries") %>%
    filter(year == 2018) %>%
    select(ctu_name, year, emp) %>%
    rename(value = emp) %>%
    mutate(var = "total_jobs") %>%
    select(ctu_name, year, var, value)


  ## ----- aggregate industrial jobs by NAICS -----
  ctu_industrial_jobs <-
    tb$ctu_qcew_ctu %>%
    filter(
      naicstitle %in% c(
        "Natural Resources and Mining",
        "Construction",
        "Trade, Transportation and Utilities"
      )
    ) %>%
    filter(year == 2018) %>%
    select(ctu_name, year, naicstitle, emp) %>%
    group_by(ctu_name, year) %>%
    summarise(value = sum(emp, na.rm = TRUE), .groups = "keep") %>%
    mutate(var = "industrial_jobs") %>%
    select(ctu_name, year, var, value)


  ## ----- aggregate commercial jobs by NAICS ----
  ctu_commercial_jobs <-
    tb$ctu_qcew_ctu %>%
    filter(
      naicstitle %in% c(
        "Financial Activities",
        "Professional and Business Services",
        "Education and Health Services",
        "Leisure and Hospitality",
        "Public Administration"
      )
    ) %>%
    filter(year == 2018) %>%
    select(ctu_name, year, naicstitle, emp) %>%
    group_by(ctu_name, year) %>%
    summarise(value = sum(emp, na.rm = TRUE), .groups = "keep") %>%
    mutate(var = "commercial_jobs") %>%
    select(ctu_name, year, var, value)


  ## ----- get forecast of single and multifamily units from 'Emissions' ----
  ctu_housing_stock <-
    tb$forecast_lu_ctu %>%
    filter(var %in% c("SFD_Units", "MF_Units")) %>%
    filter(year == 2018) %>%
    group_by(ctu_name, year)


  ## ----- estimate single family average floor area from ZTRAX ----
  ctu_average_floor_area_single_family <-
    tb$ztrax_sqft_summary_ctu %>%
    select(ctu_name, property_land_use, mean_sqft) %>%
    unique() %>%
    filter(property_land_use == "Single family residential") %>%
    group_by(ctu_name) %>%
    mutate(
      var = "single_family_average_floor_area_sqft_ctu",
      year = 2018,
      value = mean(mean_sqft, na.rm = T)
    ) %>%
    select(ctu_name, year, var, value)


  ## ---- get average multifamily floor area from ZTRAX ----
  ctu_average_floor_area_multifamily <-
    tb$ztrax_sqft_summary_ctu %>%
    select(ctu_name, property_land_use, mean_sqft) %>%
    filter(stringr::str_detect(property_land_use, "Condominium")) %>%
    mutate(
      var = "multifamily_average_floor_area_sqft_ctu",
      year = 2018
    ) %>%
    rename(value = mean_sqft) %>%
    select(ctu_name, year, var, value)


  ## ---- get county multifamily floor area for when ctu equivalent is missing ----
  ctu_county <- county_characteristics %>%
    left_join(t_ctu_county, by = "co_name") %>%
    filter(var == "multifamily_average_floor_area_sqft_county") %>%
    group_by(ctu_name, year, var) %>%
    mutate(value = value * pct_population) %>%
    select(ctu_name, year, var, value) %>%
    group_by(ctu_name, year, var) %>%
    summarise(value = sum(value), .groups = "keep")
  # uses weighted average based on population

  #---- compile ctu demographic baseline characterics -----
  ctu_characteristics <-
    bind_rows(
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

  return(ctu_characteristics)

}
