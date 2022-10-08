#' Title
#'
#' @return
#' @export
#'
#' @examples
get_ctu_non_residential_xcel_energy <-
  function(tb = building_energy_data) {
    ## ---- check if community is served by more than 90% Xcel Energy ----
    xcel_energy_percent <-
      tb$intersect_landuse_utility_service_area_ctu %>%
      group_by(ctu_name, utility_name) %>%
      summarise(acres = sum(acres), .groups = "keep") %>%
      mutate(percent = acres / acres) %>%
      filter(utility_name == "Xcel Energy")

    ## ---- variable return TRUE if Xcel Energy serves more than 90% ----
    is_served_by_mostly_xcel <-
      xcel_energy_percent %>%
      rowwise() %>%
      mutate(is_excel = if_else(percent > 0.90, TRUE, FALSE))

    ## ---- get xcel energy mwh/year for the 'business' category ----
    commercial_industrial_electricity_mwh_xcel <-
      tb$utility_electricity_by_ctu %>%
      filter(customer_class_name == "Business") %>%
      filter(year == 2018)

    ## ---- get xcel energy intensity per customer class ----
    xcel_energy_electricity <-
      ctu_characteristics %>%
      filter(var %in% c("commercial_jobs",
                        "industrial_jobs")) %>%
      mutate(
        state_mwh_per_worker =
          case_when(
            (var == "commercial_jobs") ~ commercial_mwh_per_worker_state[[1]],
            (var == "industrial_jobs") ~ industrial_mwh_per_worker_state[[1]]
          )
      ) %>%
      mutate(
        var =
          case_when(
            (var == "commercial_jobs") ~ "expected_commercial_mwh",
            (var == "industrial_jobs") ~ "expected_industrial_mwh"
          ),
        value = value * state_mwh_per_worker
      ) %>%
      group_by(ctu_name, year) %>%
      select(ctu_name, year, var, value) %>%
      pivot_wider(names_from = var, values_from = value) %>%
      left_join(
        commercial_industrial_electricity_mwh_xcel %>%
          select(ctu_name, year, mwh_per_year),
        by = c("ctu_name", "year")
      ) %>%
      mutate(
        commercial_mwh_xcel = mwh_per_year * (
          expected_commercial_mwh / (expected_commercial_mwh + expected_industrial_mwh)
        ),
        industrial_mwh_xcel = mwh_per_year * (
          expected_industrial_mwh / (expected_commercial_mwh + expected_industrial_mwh)
        )
      ) %>%
      select(ctu_name, year, commercial_mwh_xcel, industrial_mwh_xcel) %>%
      pivot_longer(cols = c(commercial_mwh_xcel, industrial_mwh_xcel),
                   names_to = "var") %>%
      filter(is.na(value) == FALSE)
  }
