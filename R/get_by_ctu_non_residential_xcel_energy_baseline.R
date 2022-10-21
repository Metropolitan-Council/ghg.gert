#' @title Get Non Residential Xcel Energy by City/Township
#'
#' @return
#' @export
#'
#' @examples
get_by_ctu_non_residential_xcel_energy_baseline <-
  function(tb = building_energy_data) {

    ctu_characteristics <- get_demographic_baseline()$ctu

    statewide_nonresidential_energy <-
      get_statewide_non_residential_energy()

    commercial_mwh_per_worker_state <-
      (
        statewide_nonresidential_energy %>% filter(year == 2018, var == "commercial_mwh_per_worker_state")
        %>% select(value)
      )[, 1]
    industrial_mwh_per_worker_state <-
      (
        statewide_nonresidential_energy %>% filter(year == 2018, var == "industrial_mwh_per_worker_state")
        %>% select(value)
      )[, 1]

    ## ---- check if community is served by more than 90% Xcel Energy ----
    xcel_energy_percent <-
      tb$intersect_landuse_utility_service_area_ctu %>%
      dplyr::group_by(ctu_name, utility_name) %>%
      dplyr::summarise(acres = sum(acres), .groups = "keep") %>%
      dplyr::mutate(percent = acres / acres) %>%
      dplyr::filter(utility_name == "Xcel Energy")

    ## ---- variable return TRUE if Xcel Energy serves more than 90% ----
    is_served_by_mostly_xcel <-
      xcel_energy_percent %>%
      dplyr::rowwise() %>%
      dplyr::mutate(is_excel = dplyr::if_else(percent > 0.90, TRUE, FALSE))

    ## ---- get xcel energy mwh/year for the 'business' category ----
    commercial_industrial_electricity_mwh_xcel <-
      tb$utility_electricity_by_ctu %>%
      dplyr::filter(customer_class_name == "Business",
                    year == 2018)

    ## ---- get xcel energy intensity per customer class ----
    xcel_energy_electricity <-
      ctu_characteristics %>%
      dplyr::filter(var %in% c("commercial_jobs",
                               "industrial_jobs")) %>%
      dplyr::mutate(
        state_mwh_per_worker =
          dplyr::case_when(
            (var == "commercial_jobs") ~ commercial_mwh_per_worker_state,
            (var == "industrial_jobs") ~ industrial_mwh_per_worker_state
          ),
        var =
          dplyr::case_when(
            (var == "commercial_jobs") ~ "expected_commercial_mwh",
            (var == "industrial_jobs") ~ "expected_industrial_mwh"
          ),
        value = value * state_mwh_per_worker
      ) %>%
      dplyr::group_by(ctu_name, year) %>%
      dplyr::select(ctu_name, year, var, value) %>%
      tidyr::pivot_wider(names_from = var, values_from = value) %>%
      dplyr::left_join(
        commercial_industrial_electricity_mwh_xcel %>%
          select(ctu_name, year, mwh_per_year),
        by = c("ctu_name", "year")
      ) %>%
      dplyr::mutate(
        commercial_mwh_xcel = mwh_per_year * (
          expected_commercial_mwh / (expected_commercial_mwh + expected_industrial_mwh)
        ),
        industrial_mwh_xcel = mwh_per_year * (
          expected_industrial_mwh / (expected_commercial_mwh + expected_industrial_mwh)
        )
      ) %>%
      dplyr::select(ctu_name, year, commercial_mwh_xcel, industrial_mwh_xcel) %>%
      tidyr::pivot_longer(cols = c(commercial_mwh_xcel, industrial_mwh_xcel),
                          names_to = "var") %>%
      dplyr::filter(is.na(value) == FALSE)

    return(xcel_energy_electricity)

  }
