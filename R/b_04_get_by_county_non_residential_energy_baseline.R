#' @title Get by County Non Residential Energy
#'
#' @family buildings
#'
#' @description This function calculates and returns the non-residential energy baseline (in MWh/year)
#'    at the county level for a specified City or Township (CTU) or all CTUs.
#'    It processes building energy data, statewide energy information, county energy baselines, utility natural gas data, and
#'    demographic information to obtain commercial and industrial energy consumption per worker for each county. The function also uses EIA,
#'    MNDOC, and NREL data sources to calculate electricity consumption baselines for commercial and industrial sectors within each county.
#'    The output is a tibble containing the non-residential energy baseline for the selected county or counties.
#'
#' @export

get_by_county_non_residential_energy_baseline <-
  function(tb = building_energy_data, .selected_ctu = "all") {
    cli::cli_progress_message("* obtaining non residential energy data by county \n")

    tb <- filter_building_energy_data(data_list = tb, .selected_ctu = .selected_ctu)

    county_characteristics <- get_demographic_baseline()$county

    # NON-RESIDENTIAL ENERGY BASELINE ----
    # COUNTY ----

    # obtain customer class ratios, so that countywide electricity and natural gas use can be allocated.
    # should be a separate pacakge.

    # ---- electric utility servicewide percent sales by customer class -----
    servicewide_customer_class_ratio <-
      tb$eia_electricity_servicewide %>%
      dplyr::filter(customer_class_name %in% c(
        "Residential",
        "Commercial",
        "Industrial"
      )) %>%
      dplyr::mutate(
        mwh_per_year =
          dplyr::case_when(
            is.na(mwh_per_year) ~ 0,
            mwh_per_year > -1 ~ mwh_per_year
          )
      ) %>%
      dplyr::select(
        utility_name,
        customer_class_name,
        mwh_per_year
      ) %>%
      dplyr::group_by(utility_name) %>%
      tidyr::pivot_wider(
        names_from = customer_class_name,
        values_from = mwh_per_year
      ) %>%
      dplyr::mutate(
        Total = sum(Residential, Commercial, Industrial),
        servicewide_percent_residential = Residential / Total,
        servicewide_percent_commercial = Commercial / Total,
        servicewide_percent_industrial = Industrial / Total
      ) %>%
      dplyr::select(
        utility_name,
        servicewide_percent_residential,
        servicewide_percent_industrial,
        servicewide_percent_commercial
      )


    ## ---- MN Form 7610 electricity by county total and by utility -----
    temp_mndoc_electricity_county <-
      tb$mndoc_electricity_county %>%
      dplyr::left_join(.,
        tb$county %>%
          dplyr::select(co_name, mn_doc_co_code),
        by = "mn_doc_co_code"
      ) %>%
      dplyr::filter(year == 2018) %>%
      dplyr::filter(
        co_name %in% c(
          "Anoka",
          "Carver",
          "Dakota",
          "Hennepin",
          "Ramsey",
          "Scott",
          "Washington"
        )
      )

    mndoc_electricity_county_total <-
      temp_mndoc_electricity_county %>%
      dplyr::group_by(co_name) %>%
      dplyr::summarise(mwh = sum(mwh_mndoc_total))

    mndoc_electricity_county_utility <-
      temp_mndoc_electricity_county %>%
      dplyr::select(1, 5, 4)

    # remove temporary table
    temp_mndoc_electricity_county %>% remove()


    ## ---- obtain utility customer class ratio by land use designation -----
    customer_class_ratio_by_area <-
      tb$intersect_landuse_utility_service_area_county %>%
      dplyr::select(co_name, utility_name, type, acres) %>%
      dplyr::group_by(co_name, utility_name, type) %>%
      dplyr::summarise(acres = sum(acres), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = type,
        values_from = acres,
        values_fill = 0
      ) %>%
      dplyr::mutate(
        commercial = commercial,
        industrial = agriculture + industrial
      ) %>%
      dplyr::select(co_name, utility_name, commercial, industrial, residential) %>%
      dplyr::mutate(
        total = commercial + industrial + residential,
        commercial_percent_by_area = commercial / total,
        industrial_percent_by_area = industrial / total,
        residential_percent_by_area = residential / total
      ) %>%
      dplyr::filter(total > 50) %>%
      dplyr::select(
        co_name,
        utility_name,
        commercial_percent_by_area,
        industrial_percent_by_area,
        residential_percent_by_area
      )


    ## ----MNDOC countywide energy consumption customer class estimate----------------------------
    mndoc_customer_class_estimate <-
      dplyr::right_join(mndoc_electricity_county_utility,
        servicewide_customer_class_ratio,
        by = "utility_name"
      ) %>%
      dplyr::right_join(.,
        customer_class_ratio_by_area,
        by = c("utility_name", "co_name")
      ) %>%
      dplyr::mutate(
        percent_residential = (
          servicewide_percent_residential + residential_percent_by_area
        ) / 2,
        percent_commercial = (
          servicewide_percent_commercial + commercial_percent_by_area
        ) / 2,
        percent_industrial = (
          servicewide_percent_industrial + industrial_percent_by_area
        ) / 2
      ) %>%
      dplyr::mutate(
        residential_mwh = mwh_mndoc_total * percent_residential,
        commercial_mwh = mwh_mndoc_total * percent_commercial,
        industrial_mwh = mwh_mndoc_total * percent_industrial
      ) %>%
      dplyr::group_by(co_name) %>%
      dplyr::summarise(
        residential_mwh_county = sum(residential_mwh, na.rm = TRUE),
        commercial_mwh_county = sum(commercial_mwh, na.rm = TRUE),
        industrial_mwh_county = sum(industrial_mwh, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()


    ## ----- estimate countywide energy consumption by customer class ----
    county_electricity <-
      mndoc_customer_class_estimate %>%
      tidyr::pivot_longer(
        cols = c(
          "residential_mwh_county",
          "commercial_mwh_county",
          "industrial_mwh_county"
        ),
        names_to = "var"
      ) %>%
      dplyr::mutate(year = 2018) %>%
      dplyr::select(co_name, year, var, value)


    ## ----- estimate county MWh/worker (commercial and industrial) ----
    mwh_per_worker_county <-
      dplyr::bind_rows(
        county_electricity,
        county_characteristics %>%
          dplyr::filter(
            var %in% c("commercial_workers_county", "industrial_workers_county")
          )
      ) %>%
      tidyr::pivot_wider(names_from = "var", values_from = "value") %>%
      dplyr::mutate(
        commercial_mwh_per_worker_county = commercial_mwh_county / commercial_workers_county,
        industrial_mwh_per_worker_county = industrial_mwh_county / industrial_workers_county
      ) %>%
      dplyr::select(
        co_name,
        year,
        commercial_mwh_per_worker_county,
        industrial_mwh_per_worker_county
      ) %>%
      tidyr::pivot_longer(
        cols = c(
          "commercial_mwh_per_worker_county",
          "industrial_mwh_per_worker_county"
        ),
        names_to = "var"
      )

    ## ---- compile non-residential energy baseline assumption at the county scale ----
    county_nonresidential_baseline <-
      bind_rows(mwh_per_worker_county)

    return(county_nonresidential_baseline)
  }
