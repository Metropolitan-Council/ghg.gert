#' @title Get Non Residential Energy Baseline
#'
#' @return
#' @export
#'
#' @examples
get_non_residential_energy_baseline <-
  function(tb = building_energy_data) {

    statewide_nonresidential_energy <-
      ghg.sp::get_statewide_non_residential_energy()

    county_nonresidential_baseline <-
      ghg.sp::get_by_county_non_residential_energy_baseline()

    xcel_energy_electricity <-
      ghg.sp::get_by_ctu_non_residential_xcel_energy_baseline()

    ctu_characteristics <- ghg.sp::get_demographic_baseline()$ctu

    ## ---- obtain mwh/year for commercial workers for the state ----
    commercial_mwh_per_worker_state <-
      statewide_nonresidential_energy %>%
      dplyr::filter(var == "commercial_mwh_per_worker_state") %>%
      dplyr::select(value)

    ## ---- obtain mwh/year for industrial workers for the state -----
    industrial_mwh_per_worker_state <-
      statewide_nonresidential_energy %>%
      dplyr::filter(var == "industrial_mwh_per_worker_state") %>%
      dplyr::select(value)

    ## ---- obtain commercial therms/worker for  the state ----
    commercial_therms_per_worker_state <-
      statewide_nonresidential_energy %>%
      dplyr::filter(var == "commercial_therms_per_worker_state") %>%
      dplyr::select(value)

    ## ---- obtain industrial therms/worker for the state ----
    industrial_therms_per_worker_state <-
      statewide_nonresidential_energy %>%
      dplyr::filter(var == "industrial_therms_per_worker_state") %>%
      dplyr::select(value)

    # NON-RESIDENTIAL ENERGY BASELINE ----
    # CTU ----


    ## ---- get commercial/industrial electricity baseline from NREL ----
    nrel_electricity_ctu <-
      tb$nrel_energy_consumption_ctu %>%
      dplyr::filter(sector %in% c("industrial", "commercial")) %>%
      dplyr::filter(source == "elec",
             year == "2018") %>%
      dplyr::select(ctu_name, year, sector, consumption_mmbtu) %>%
      dplyr::mutate(value = consumption_mmbtu * 0.29307107) %>%
      dplyr::mutate(var =
               dplyr::case_when(
                 (sector == "industrial") ~ "industrial_mwh_nrel",
                 (sector == "commercial") ~ "commercial_mwh_nrel",
               )) %>%
      dplyr::select(ctu_name, year, var, value)

    ## ---- get available utility natural gas data from 'Emissions' ----
    nonresidential_naturalgas_ctu <-
      tb$utility_natural_gas_by_ctu %>%
      dplyr::filter(year == 2018) %>%
      dplyr::filter(customer_class_name %in% c("Business",
                                        # "Industrial",
                                        # "Commercial",
                                        "Non-Residential")) %>%
      dplyr::group_by(ctu_name, year) %>%
      dplyr::summarise(
        mcf_per_year = sum(mcf_per_year, na.rm = T),
        number_of_customers = sum(number_of_customers, na.rm = T),
        therms_per_year = sum(therms_per_year, na.rm = T),
        utility_name = paste(utility_name, collapse = ", "),
        customer_class_name = paste(customer_class_name, collapse = ", "),
        .groups = "keep"
      )

    ## ----- dissagregate commercial and industrial natural gas utility data ----
    commercial_and_industrial_natural_gas_ctu <-
      ctu_characteristics %>%
      dplyr::filter(var %in% c("commercial_jobs",
                        "industrial_jobs")) %>%
      dplyr::mutate(
        state_therms_per_worker =
          dplyr::case_when(
            var == "commercial_jobs" ~ commercial_therms_per_worker_state[[1]],
            var == "industrial_jobs" ~ industrial_therms_per_worker_state[[1]]
          )
      ) %>%
      dplyr::mutate(
        var =
          dplyr::case_when(
            (var == "commercial_jobs") ~ "expected_commercial_therms",
            (var == "industrial_jobs") ~ "expected_industrial_therms"
          ),
        value = value * state_therms_per_worker
      ) %>%
      dplyr::group_by(ctu_name, year) %>%
      dplyr::select(ctu_name, year, var, value) %>%
      tidyr::pivot_wider(names_from = var, values_from = value) %>%
      dplyr::left_join(
        nonresidential_naturalgas_ctu %>%
          dplyr::select(ctu_name, year, therms_per_year),
        by = c("ctu_name", "year")
      ) %>%
      dplyr::mutate(
        commercial_therms = therms_per_year * (
          expected_commercial_therms / (expected_commercial_therms + expected_industrial_therms)
        ),
        industrial_therms = therms_per_year * (
          expected_industrial_therms / (expected_commercial_therms + expected_industrial_therms)
        )
      ) %>%
      dplyr::select(ctu_name, year, commercial_therms, industrial_therms) %>%
      tidyr::pivot_longer(cols = c(commercial_therms, industrial_therms),
                   names_to = "var") %>%
      dplyr::filter(is.na(value) == FALSE)


    ## ---- get NREL natural gas consumption data ----
    nrel_natural_gas_ctu <-
      tb$nrel_energy_consumption_ctu %>%
      dplyr::filter(sector %in% c("industrial", "commercial"),
                    source == "ng",
                    year == "2018") %>%
      dplyr::select(ctu_name, year, sector, consumption_mmbtu) %>%
      dplyr::mutate(
        value = consumption_mmbtu * 10,
        var =
          dplyr::case_when(
            (sector == "industrial") ~ "industrial_therms_nrel",
            (sector == "commercial") ~ "commercial_therms_nrel",
          )
      ) %>%
      dplyr::select(ctu_name, year, var, value) %>%
      unique()

    # -------------------------------------------------------------------------
    ctu_nonresidential_energy_baseline_1 <-
      dplyr::bind_rows(
        xcel_energy_electricity,
        nrel_electricity_ctu,
        commercial_and_industrial_natural_gas_ctu,
        nrel_natural_gas_ctu
      ) %>%
      tidyr::pivot_wider(names_from = var, values_from = value) %>%
      dplyr::transmute(
        commercial_mwh = ifelse(
          is.na(commercial_mwh_xcel) == FALSE,
          commercial_mwh_xcel,
          commercial_mwh_nrel
        ),
        industrial_mwh = ifelse(
          is.na(industrial_mwh_xcel) == FALSE,
          industrial_mwh_xcel,
          industrial_mwh_nrel
        ),
        commercial_therms = ifelse(
          is.na(commercial_therms) == FALSE,
          commercial_therms,
          commercial_therms_nrel
        ),
        industrial_therms = ifelse(
          is.na(industrial_therms) == FALSE,
          industrial_therms,
          industrial_therms_nrel
        )
      ) %>%
      tidyr::pivot_longer(
        cols = c(
          "commercial_therms",
          "industrial_therms",
          "commercial_mwh",
          "industrial_mwh"
        ),
        names_to = "var"
      )

    ## -------------------------------------------------------------------------------------------
    ctu_nonresidential_energy_per_worker <-
      dplyr::bind_rows(ctu_nonresidential_energy_baseline_1,
                ctu_characteristics %>%
                  dplyr::filter(var %in% c("commercial_jobs", "industrial_jobs"))) %>%
      tidyr::pivot_wider(names_from = "var", values_from = "value") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        commercial_therm_per_worker = commercial_therms / commercial_jobs,
        industrial_therm_per_worker = industrial_therms / industrial_jobs,
        commercial_mwh_per_worker = commercial_mwh / commercial_jobs,
        industrial_mwh_per_worker = industrial_mwh / industrial_jobs
      ) %>%
      dplyr::select(
        ctu_name,
        year,
        commercial_therm_per_worker,
        industrial_therm_per_worker,
        commercial_mwh_per_worker,
        industrial_mwh_per_worker
      ) %>%
      tidyr::pivot_longer(
        cols = c(
          "commercial_therm_per_worker",
          "industrial_therm_per_worker",
          "commercial_mwh_per_worker",
          "industrial_mwh_per_worker"
        ),
        names_to = "var"
      )

    ## -------------------------------------------------------------------------------------------
    ctu_nonresidential_energy_baseline <-
      bind_rows(ctu_nonresidential_energy_baseline_1,
                ctu_nonresidential_energy_per_worker) %>%
      tibble::as_tibble()

    return(ctu_nonresidential_energy_baseline)

  }
