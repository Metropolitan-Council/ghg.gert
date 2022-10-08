#' Calculate Demographic Forecast
#'
#' @return
#' @export
#'
#' @examples
calc_demographic_forecast <- function(tb = building_energy_data) {

  # COUNTY DEMOGRAPHIC FORECAST ----

  ## ---- estimate avg growth in single family floor area -----
  county_average_annual_growth_single_family_sqft <-
    tb$ztrax_building_sqft %>%
    filter(year_built > 1991) %>%
    filter(residential_type %in% c("single_family_residential")) %>%
    group_by(co_name) %>%
    arrange(co_name, year_built) %>%
    mutate(
      diff_year = year_built - lag(year_built),
      # Difference in time (just in case there are gaps)
      diff_growth = average_building_sqft - lag(average_building_sqft),
      # Difference in route between years
      rate_percent = (diff_growth / diff_year) / average_building_sqft
    ) %>% # growth rate in percent
    summarise(mean_growth_rate = mean(rate_percent, na.rm = TRUE),
              .groups = "keep")


  ## ----- estimate avg growth in single family floor area ----
  county_average_annual_growth_multifamily_sqft <-
    tb$ztrax_building_sqft %>%
    filter(year_built > 1991) %>%
    filter(residential_type %in% c("condominium")) %>%
    group_by(co_name) %>%
    arrange(co_name, year_built) %>%
    mutate(
      diff_year = year_built - lag(year_built),
      # Difference in time (just in case there are gaps)
      diff_growth = average_building_sqft - lag(average_building_sqft),
      # Difference in route between years
      rate_percent = (diff_growth / diff_year) / average_building_sqft
    ) %>% # growth rate in percent
    summarise(mean_growth_rate = mean(rate_percent, na.rm = TRUE),
              .groups = "keep")


  ## ----- estimate avg floor area for multifamily ----
  county_average_floor_area_multifamily_forecast <-
    county_average_floor_area_multifamily %>%
    left_join(county_average_annual_growth_multifamily_sqft, by = "co_name") %>%
    mutate(
      value =
        case_when(
          mean_growth_rate * (2040 - 2018) > 0.15 ~ value + (value *
                                                               0.15),
          mean_growth_rate * (2040 - 2018) < 0.15 ~ value + (value * mean_growth_rate)
        ),
      year = 2040
    )


  ## ----- get commercial/industrial workers forecast from 'Emissions' ----
  county_emp_forecast <-
    tb$emp_forecast_industry_county %>%
    filter(year == 2040) %>%
    group_by(co_name, year, indlabel) %>%
    mutate(var =
             case_when(
               (
                 indlabel %in% naics_codes$industrial ~ "industrial_emp_forecast_county"
               ),
               (
                 indlabel %in% naics_codes$commercial ~ "commercial_emp_forecast_county"
               )
             )) %>%
    group_by(co_name, year, var) %>%
    summarise(value = sum(emp, na.rm = T), .groups = "keep")


  ## ----- compile county forecast of demographic characteristics ----
  county_characteristics_forecast <-
    bind_rows(county_average_floor_area_multifamily_forecast,
              county_emp_forecast)

  # CTU DEMOGRAPHIC FORECAST -----

  ## ----get forecast population from 'Emissions' -----
  ctu_population_forecast <-
    tb$ctu_forecast %>%
    filter(year == 2040)


  ## ---- get industrial/commercial workers forecast from 'Emissions' -----
  ctu_emp_forecast <-
    tb$emp_forecast_industry_ctu %>%
    filter(year == 2040) %>%
    group_by(ctu_name, year, indlabel) %>%
    mutate(var =
             case_when(
               (
                 indlabel %in% naics_codes$industrial ~ "industrial_emp_forecast"
               ),
               (
                 indlabel %in% naics_codes$commercial ~ "commercial_emp_forecast"
               )
             )) %>%
    group_by(ctu_name, year, var) %>%
    summarise(value = sum(emp, na.rm = T), .groups = "keep")


  ## ---- get housing stock forecast from 'Emissions' ----
  housing_stock_ctu_forecast <- tb$forecast_lu_ctu %>%
    filter(year == 2040,
           ctu_name %in% unique(t_ctu_forecast$ctu_name),
           var %in% (c("SFD_Units", "MF_Units")))


  ## ---- estimate average growth of building area for single family ----
  ctu_average_floor_area_single_family_forecast <-
    ctu_average_floor_area_single_family %>%
    left_join(tb$ctu_county, by = "ctu_name") %>%
    left_join(county_average_annual_growth_single_family_sqft, by = "co_name") %>%
    select(-co_name) %>%
    group_by(ctu_name) %>%
    mutate(mean_growth_rate = mean(mean_growth_rate, na.rm = T)) %>%
    unique() %>%
    group_by(ctu_name) %>%
    mutate(
      value =
        case_when(
          mean_growth_rate * (2040 - 2018) > 0.15 ~ value + (value * 0.15),
          mean_growth_rate * (2040 - 2018) < 0.15 ~ value + (value * mean_growth_rate)
        ),
      year = 2040
    ) %>%
    unique() %>%
    select(ctu_name, year, var, value)


  ## -----multifamily floor area growth --------------------------------------------------------
  ctu_average_floor_area_multifamily_forecast <-
    ctu_average_floor_area_multifamily %>%
    left_join(tb$ctu_county, by = "ctu_name") %>%
    left_join(county_average_annual_growth_multifamily_sqft, by = "co_name") %>%
    select(-co_name) %>%
    group_by(ctu_name) %>%
    mutate(mean_growth_rate = mean(mean_growth_rate, na.rm = T)) %>%
    unique() %>%
    group_by(ctu_name) %>%
    mutate(
      value =
        case_when(
          mean_growth_rate * (2040 - 2018) > 0.15 ~ value + (value * 0.15),
          mean_growth_rate * (2040 - 2018) < 0.15 ~ value + (value * mean_growth_rate)
        ),
      year = 2040
    ) %>%
    unique() %>%
    select(ctu_name, year, var, value)


  ## ----get county forecast for avg multifamily floor area for when ctu equivalent is missing ----
  ctu_county_forecast <- county_characteristics_forecast %>%
    left_join(t_ctu_county, by = "co_name") %>%
    filter(var == "multifamily_average_floor_area_sqft_county") %>%
    group_by(ctu_name, year, var) %>%
    summarize(value = mean(value), .groups = "keep") %>%
    select(ctu_name, year, var, value)


  ## ----- compile forecast of ctu demographic characteristics -----
  ctu_characteristics_forecast <-
    bind_rows(
      ctu_population_forecast,
      housing_stock_ctu_forecast,
      ctu_average_floor_area_single_family_forecast,
      ctu_average_floor_area_multifamily_forecast,
      ctu_emp_forecast,
      ctu_county_forecast
    ) %>%
  as_tibble()
  #   distinct() %>%
  #   pivot_wider(names_from = var,
  #               values_from = value) %>%
  #   select(
  #     ctu_name,
  #     year,
  #     population,
  #     households,
  #     total_jobs = jobs,
  #     commercial_jobs = commercial_emp_forecast,
  #     industrial_jobs = industrial_emp_forecast,
  #     everything()
  #   ) %>%
  #   group_by(ctu_name, year) %>%
  #   bind_rows(ctu_characteristics %>%
  #               pivot_wider(names_from = var,
  #                           values_from = value))

  return(ctu_characteristics_forecast)

}
