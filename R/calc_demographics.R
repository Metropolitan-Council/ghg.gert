#' @title Generate demographic summary tables at county, CTU level
#'
#' @param period character, one of `"baseline"`, `"forecast"`
#' @param tables list, necessary tables. Default is `ghg.sp::db_tables`.
#'
#' @family buildings
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr select filter mutate case_when group_by summarise bind_rows
#' @importFrom tidyr pivot_longer
calc_demographics <- function(period = "baseline",
                              tables = db_tables) {


  # baseline-----
  if (period == "baseline") {
    ## county -----


    # required tables
    ## t_ztrax_sqft_summary_county
    ## t_led_industry_county

    # obtain the average floor area in sqft of single family homes
    p_average_floor_area_single_family_county <-
      tables$t_ztrax_sqft_summary_county %>%
      dplyr::select(
        .data$co_name,
        .data$property_land_use,
        .data$mean_sqft
      ) %>%
      dplyr::filter(.data$property_land_use == "Single family residential") %>%
      # dplyr::group_by(.data$co_name)  %>%
      dplyr::mutate(
        metric = "single_family_average_floor_area_sqft_county",
        year = 2018
      ) %>%
      # dplyr::rename(value = .data$mean_sqft) %>%
      dplyr::select(co_name, year, metric, value = .data$mean_sqft)


    p_average_floor_area_multifamily_county <-
      tables$t_ztrax_sqft_summary_county %>%
      dplyr::select(
        .data$co_name,
        .data$property_land_use,
        .data$mean_sqft
      ) %>%
      # only homeowners, not renters?
      dplyr::filter(.data$property_land_use == "Condominium") %>%
      # dplyr::group_by(.data$co_name)  %>%
      dplyr::mutate(
        metric = "multifamily_average_floor_area_sqft_county",
        year = 2018
      ) %>%
      # dplyr::rename(value = mean_sqft) %>%
      dplyr::select(co_name, year, metric, value = mean_sqft)


    # number of workers (commercial vs residential)
    p_county_workers <-
      tables$t_led_industry_county %>%
      dplyr::filter(
        .data$year == 2018,
        .data$jw_indicator == "W"
      ) %>%
      dplyr::mutate(
        sector =
          case_when(
            (.data$industry %in% naics_codes$led_commercial) ~ "commercial_workers_county",
            (.data$industry %in% naics_codes$led_industrial) ~ "industrial_workers_county"
          )
      ) %>%
      dplyr::select(.data$co_name, .data$year, .data$sector, .data$count) %>%
      dplyr::group_by(.data$co_name, .data$year, .data$sector) %>%
      dplyr::summarise(value = sum(count), .groups = "keep") %>%
      dplyr::rename(metric = sector)


    demo_table_county <-
      dplyr::bind_rows(
        p_average_floor_area_single_family_county,
        p_average_floor_area_multifamily_county,
        p_county_workers
      )


    ## ctu ------
    # required tables
    ## t_ctu_population
    ## t_ctu_qcew_ctu
    ## t_housing_stock_ctu
    ## t_ztrax_sqft_summary_ctu


    # obtain population 2018 [# of people]
    # obtain households 2018 [# of households]
    p_ctu_population <-
      tables$t_ctu_population %>%
      dplyr::select(ctu_name, year, population, households) %>%
      dplyr::filter(year == 2018) %>%
      dplyr::group_by(ctu_name, year) %>%
      tidyr::pivot_longer(
        cols = c("population", "households"),
        names_to = "metric",
        values_to = "value"
      )

    # obtain jobs 2018 [# of employees]
    p_jobs <-
      tables$t_ctu_qcew_ctu %>%
      dplyr::filter(
        naicstitle == "Total, All Industries",
        year == 2018
      ) %>%
      dplyr::select(ctu_name, year, emp) %>%
      # dplyr::rename(value = emp) %>%
      dplyr::mutate(metric = "total_jobs") %>%
      dplyr::select(ctu_name, year, metric, value = emp)

    # obtain number of industrial workers 2018
    p_industrial_jobs <-
      tables$t_ctu_qcew_ctu %>%
      dplyr::filter(
        naicstitle %in% c(
          "Natural Resources and Mining",
          "Construction",
          "Trade, Transportation and Utilities"
        ),
        year == 2018
      ) %>%
      dplyr::select(ctu_name, year, naicstitle, emp) %>%
      dplyr::group_by(ctu_name, year) %>%
      dplyr::summarise(value = sum(emp, na.rm = TRUE), .groups = "keep") %>%
      dplyr::mutate(metric = "industrial_jobs") %>%
      dplyr::select(ctu_name, year, metric, value)

    # obtain number of commercial workers
    p_commercial_jobs <-
      tables$t_ctu_qcew_ctu %>%
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
      dplyr::mutate(metric = "commercial_jobs") %>%
      dplyr::select(ctu_name, year, metric, value)




    # housing stock - single family 2018 [# of housing units]
    # housing stock - multifamily 2018 [# of housing units]
    p_ctu_housing_stock <-
      tables$t_housing_stock_ctu %>%
      dplyr::mutate(
        single_family_units = single_family_detached + townhouse + manufactured_homes,
        multifamily_units = multifamily_in_5_or_more_units_bldng + duplex_triplex_or_quadplex
      ) %>%
      dplyr::filter(year == 2018) %>%
      dplyr::select(ctu_name, year, multifamily_units, single_family_units) %>%
      dplyr::group_by(ctu_name, year) %>%
      tidyr::pivot_longer(
        cols = c("single_family_units", "multifamily_units"),
        names_to = "metric",
        values_to = "value"
      )

    # average floor area - single family 2018 [sqft]
    p_average_floor_area_single_family_ctu <-
      tables$t_ztrax_sqft_summary_ctu %>%
      dplyr::select(ctu_name, property_land_use, mean_sqft) %>%
      dplyr::filter(property_land_use == "Single family residential") %>%
      dplyr::mutate(
        metric = "single_family_average_floor_area_sqft_ctu",
        year = 2018
      ) %>%
      # dplyr::rename(value = mean_sqft) %>%
      dplyr::select(ctu_name, year, metric, value = mean_sqft)

    # average floor area - multifamily 2018 [sqft]
    p_average_floor_area_multifamily_ctu <-
      tables$t_ztrax_sqft_summary_ctu %>%
      dplyr::select(ctu_name, property_land_use, mean_sqft) %>%
      dplyr::filter(property_land_use == "	Condominium") %>%
      dplyr::mutate(
        metric = "multifamily_average_floor_area_sqft_ctu",
        year = 2018
      ) %>%
      # dplyr::rename(value = mean_sqft) %>%
      dplyr::select(ctu_name, year, metric, value = mean_sqft)




    demo_table_ctu <-
      bind_rows(
        p_ctu_population,
        p_jobs,
        p_commercial_jobs,
        p_industrial_jobs,
        p_ctu_housing_stock,
        p_average_floor_area_single_family_ctu,
        p_average_floor_area_multifamily_ctu,
        # why rename??
        p_county_characteristics %>%
          rename("ctu_name" = "co_name")
        # mutate(ctu_name = params$ctu_name)
      )
  }

  # forecast -----


  return(demo_table)
}
