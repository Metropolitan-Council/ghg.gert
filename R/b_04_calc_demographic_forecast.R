#' @title Calculate Demographic Forecast
#' @family buildings
#'
#' @description Calculates demographic forecast characteristics by CTU.
#'
#' @return [tibble::tibble()]
#'
#' @inheritParams run_scenario_transportation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ghg.sp)
#' calc_demographic_forecast(tb = building_energy_data)$ctu
#' }
calc_demographic_forecast <- function(tb = building_energy_data, .selected_ctu = "all") {
  cat("* calculating demographic forecast \n")
  tb <- filter_building_energy_data(data_list = tb, .selected_ctu = .selected_ctu)

  # COUNTY DEMOGRAPHIC FORECAST ----
  demographic_characteristics_forecast <- c()
  # -------------------------------------------------------------------------

  ## ---- estimate avg growth in single family floor area -----
  county_average_annual_growth_single_family_sqft <-
    tb$ztrax_building_sqft %>%
    dplyr::filter(
      year_built > 1991,
      residential_type %in% c("single_family_residential")
    ) %>%
    dplyr::group_by(co_name) %>%
    dplyr::arrange(co_name, year_built) %>%
    dplyr::mutate(
      diff_year = year_built - dplyr::lag(year_built),
      # Difference in time (just in case there are gaps)
      diff_growth = average_building_sqft - dplyr::lag(average_building_sqft),
      # Difference in route between years
      rate_percent = (diff_growth / diff_year) / average_building_sqft
    ) %>% # growth rate in percent
    dplyr::summarise(
      mean_growth_rate = mean(rate_percent, na.rm = TRUE),
      .groups = "keep"
    )


  # -------------------------------------------------------------------------

  ## ----- estimate avg growth in single family floor area ----
  county_average_annual_growth_multifamily_sqft <-
    tb$ztrax_building_sqft %>%
    dplyr::filter(
      year_built > 1991,
      residential_type %in% c("condominium")
    ) %>%
    dplyr::group_by(co_name) %>%
    dplyr::arrange(co_name, year_built) %>%
    dplyr::mutate(
      diff_year = year_built - dplyr::lag(year_built),
      # Difference in time (just in case there are gaps)
      diff_growth = average_building_sqft - dplyr::lag(average_building_sqft),
      # Difference in route between years
      rate_percent = (diff_growth / diff_year) / average_building_sqft
    ) %>% # growth rate in percent
    dplyr::summarise(
      mean_growth_rate = mean(rate_percent, na.rm = TRUE),
      .groups = "keep"
    )


  # -------------------------------------------------------------------------


  county_average_floor_area_multifamily <-
    tb$ztrax_sqft_summary_county %>%
    dplyr::filter(property_land_use == "Condominium") %>%
    dplyr::mutate(
      var = "multifamily_average_floor_area_sqft_county",
      year = 2018
    ) %>%
    dplyr::rename(value = mean_sqft) %>%
    dplyr::select(co_name, year, var, value)


  # -------------------------------------------------------------------------

  county_average_floor_area_single_family <-
    tb$ztrax_sqft_summary_county %>%
    dplyr::filter(property_land_use == "Single family residential") %>%
    dplyr::group_by(co_name) %>%
    dplyr::mutate(
      var = "single_family_average_floor_area_sqft_county",
      year = 2018
    ) %>%
    dplyr::rename(value = mean_sqft) %>%
    dplyr::select(co_name, year, var, value)


  # -------------------------------------------------------------------------

  county_average_floor_area_multifamily_forecast <-
    county_average_floor_area_multifamily %>%
    dplyr::left_join(county_average_annual_growth_multifamily_sqft, by = "co_name") %>%
    dplyr::mutate(
      value =
        dplyr::case_when(
          mean_growth_rate * (2040 - 2018) > 0.15 ~ value + (value * 0.15),
          mean_growth_rate * (2040 - 2018) < 0.15 ~ value + (value * mean_growth_rate)
        ),
      year = 2040
    )


  # -------------------------------------------------------------------------


  ## ----- get commercial/industrial workers forecast from 'Emissions' ----
  county_emp_forecast <-
    tb$emp_forecast_industry_county %>%
    dplyr::filter(year == 2040) %>%
    dplyr::group_by(co_name, year, indlabel) %>%
    dplyr::mutate(
      var =
        dplyr::case_when(
          (
            indlabel %in% naics_codes$industrial ~ "industrial_emp_forecast_county"
          ),
          (
            indlabel %in% naics_codes$commercial ~ "commercial_emp_forecast_county"
          )
        )
    ) %>%
    dplyr::group_by(co_name, year, var) %>%
    dplyr::summarise(value = sum(emp, na.rm = T), .groups = "keep")


  # -------------------------------------------------------------------------


  ## ----- compile county forecast of demographic characteristics ----
  county_characteristics_forecast <-
    dplyr::bind_rows(
      county_average_floor_area_multifamily_forecast,
      county_emp_forecast
    )


  # -------------------------------------------------------------------------

  # CTU DEMOGRAPHIC FORECAST -----

  ## ----get forecast population from 'Emissions' -----
  ctu_population_forecast <-
    tb$ctu_forecast %>%
    dplyr::filter(year == 2040)


  ## ---- get industrial/commercial workers forecast from 'Emissions' -----
  ctu_emp_forecast <-
    tb$emp_forecast_industry_ctu %>%
    dplyr::filter(year == 2040) %>%
    dplyr::group_by(ctu_name, year, indlabel) %>%
    dplyr::mutate(
      var =
        dplyr::case_when(
          (
            indlabel %in% naics_codes$industrial ~ "industrial_jobs"
          ),
          (
            indlabel %in% naics_codes$commercial ~ "commercial_jobs"
          )
        )
    ) %>%
    dplyr::group_by(ctu_name, year, var) %>%
    dplyr::summarise(value = sum(emp, na.rm = T), .groups = "keep")

  ## ---- get housing stock forecast from 'Emissions' ----
  housing_stock_ctu_forecast <- tb$forecast_lu_ctu %>%
    dplyr::filter(
      year == 2040,
      ctu_name %in% unique(tb$ctu_forecast$ctu_name),
      var %in% (c("SFD_Units", "MF_Units"))
    ) %>%
    dplyr::mutate(
      var =
        dplyr::case_when(
          (var == "MF_Units") ~ "multifamily_units",
          (var == "SFD_Units") ~ "single_family_units"
        )
    )

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

  ## ---- estimate average growth of building area for single family ----
  ctu_average_floor_area_single_family_forecast <-
    ctu_average_floor_area_single_family %>%
    dplyr::left_join(tb$ctu_county, by = "ctu_name") %>%
    dplyr::left_join(county_average_annual_growth_single_family_sqft, by = "co_name") %>%
    dplyr::select(-co_name) %>%
    dplyr::group_by(ctu_name) %>%
    dplyr::mutate(mean_growth_rate = mean(mean_growth_rate, na.rm = T)) %>%
    unique() %>%
    dplyr::group_by(ctu_name) %>%
    dplyr::mutate(
      value =
        dplyr::case_when(
          mean_growth_rate * (2040 - 2018) > 0.15 ~ value + (value * 0.15),
          mean_growth_rate * (2040 - 2018) < 0.15 ~ value + (value * mean_growth_rate)
        ),
      year = 2040
    ) %>%
    unique() %>%
    dplyr::select(ctu_name, year, var, value)


  ## -----multifamily floor area growth --------------------------------------------------------
  ctu_average_floor_area_multifamily_forecast <-
    ctu_average_floor_area_multifamily %>%
    dplyr::left_join(tb$ctu_county, by = "ctu_name") %>%
    dplyr::left_join(county_average_annual_growth_multifamily_sqft, by = "co_name") %>%
    dplyr::select(-co_name) %>%
    dplyr::group_by(ctu_name) %>%
    dplyr::mutate(mean_growth_rate = mean(mean_growth_rate, na.rm = T)) %>%
    unique() %>%
    dplyr::group_by(ctu_name) %>%
    dplyr::mutate(
      value =
        dplyr::case_when(
          mean_growth_rate * (2040 - 2018) > 0.15 ~ value + (value * 0.15),
          mean_growth_rate * (2040 - 2018) < 0.15 ~ value + (value * mean_growth_rate)
        ),
      year = 2040
    ) %>%
    unique() %>%
    dplyr::select(ctu_name, year, var, value)


  ## ----get county forecast for avg multifamily floor area for when ctu equivalent is missing ----
  ctu_county_forecast <- county_characteristics_forecast %>%
    dplyr::left_join(tb$ctu_county, by = "co_name") %>%
    dplyr::filter(var == "multifamily_average_floor_area_sqft_county" & is.na(ctu_name) == F) %>%
    dplyr::group_by(ctu_name, year, var) %>%
    dplyr::summarize(value = mean(value), .groups = "keep") %>%
    dplyr::select(ctu_name, year, var, value)


  ## ----- compile forecast of ctu demographic characteristics -----
  demographic_characteristics_forecast$ctu <-
    dplyr::bind_rows(
      ctu_population_forecast,
      housing_stock_ctu_forecast,
      ctu_average_floor_area_single_family_forecast,
      ctu_average_floor_area_multifamily_forecast,
      ctu_emp_forecast,
      ctu_county_forecast
    ) %>%
    tibble::as_tibble()

  return(demographic_characteristics_forecast)
}
