get_non_residential_energy_baseline <- function() {
  # NON-RESIDENTIAL ENERGY BASELINE ----
  # STATE ----

  ## ----- obtain electricity consumption by customer class for the state -----
  electricity_consumption_by_customer_class_state <-
   tb$eia_energy_consumption_state %>%
    filter(type == "elec") %>%
    filter(year == 2018) %>%
    filter(
      var %in% c(
        "electricity_industrial_consumption_mwh",
        "electricity_commercial_consumption_mwh",
        "electricity_residential_consumption_mwh"
      )
    ) %>%
    select(state_name, year, var, value) %>%
    mutate(
      var =
        case_when(
          (var == "electricity_residential_consumption_mwh") ~ "electricity_residential_consumption_mwh_state",
          (var == "electricity_commercial_consumption_mwh") ~ "electricity_commercial_consumption_mwh_state",
          (var == "electricity_industrial_consumption_mwh") ~ "electricity_industrial_consumption_mwh_state",
        )
    )


  ## ---- obtain the number of employees (industrial/commercial) for the state ----
  employees_by_type_state <-
   tb$state_qcew %>%
    filter(year == 2018) %>%
    select(state_name, year, naicstitle, emp) %>%
    mutate(type =
             case_when(
               (
                 naicstitle %in% c(
                   "Natural Resources and Mining",
                   "Construction",
                   "Manufacturing"
                 )
               ) ~ "industrial_employees_state",
               (
                 naicstitle %in% c(
                   "Trade, Transportation and Utilities",
                   "Information",
                   "Financial Activities",
                   "Professional and Business Services",
                   "Education and Health Services",
                   "Leisure and Hospitality",
                   "Other Services",
                   "Public Administration"
                 )
               ) ~ "commercial_employees_state"
             )) %>%
    group_by(state_name, year, type) %>%
    summarise(value = sum(emp), .groups = "keep") %>%
    rename(var = type)


  ## ---- estimate energy intensity of worker at the state scale -------
  mwh_per_worker_state <-
    bind_rows(electricity_consumption_by_customer_class_state,
              employees_by_type_state) %>%
    pivot_wider(values_from = "value", names_from = "var") %>%
    mutate(
      commercial_mwh_per_worker_state = (
        electricity_commercial_consumption_mwh_state / commercial_employees_state
      ),
      industrial_mwh_per_worker_state = (
        electricity_industrial_consumption_mwh_state / industrial_employees_state
      )
    ) %>%
    select(
      state_name,
      year,
      commercial_mwh_per_worker_state,
      industrial_mwh_per_worker_state
    ) %>%
    pivot_longer(
      cols = c(
        "commercial_mwh_per_worker_state",
        "industrial_mwh_per_worker_state"
      ),
      names_to = "var"
    )


  ## ---- obtain natural gas consumption by customer class for the state scale ------
  natural_gas_consumption_by_customer_class_state <-
   tb$eia_energy_consumption_state %>%
    filter(type == "ng") %>%
    filter(year == 2018) %>%
    filter(
      var %in% c(
        "natural_gas_industrial_consumption_mmcf",
        "natural_gas_commercial_consumption_mmcf"
      )
    ) %>%
    select(state_name, year, var, value)


  ## ---- obtain therms/worker (commercial/industrial) at the state scale ------
  therms_per_worker_state <-
    bind_rows(natural_gas_consumption_by_customer_class_state,
              employees_by_type_state) %>%
    pivot_wider(values_from = "value", names_from = "var") %>%
    mutate(
      commercial_therms_per_worker_state = (((natural_gas_commercial_consumption_mmcf * 1e+6) * 0.01
      ) / commercial_employees_state),
      industrial_therms_per_worker_state = (((natural_gas_industrial_consumption_mmcf * 1e+6) * 0.01
      ) / industrial_employees_state)
    ) %>%
    select(
      state_name,
      year,
      commercial_therms_per_worker_state,
      industrial_therms_per_worker_state
    ) %>%
    pivot_longer(
      cols = c(
        "commercial_therms_per_worker_state",
        "industrial_therms_per_worker_state"
      ),
      names_to = "var"
    )


  ## ----- compile statewide non residential variables ----
  state_nonresidential_energy <-
    bind_rows(
      electricity_consumption_by_customer_class_state,
      employees_by_type_state,
      mwh_per_worker_state,
      therms_per_worker_state
    )

  ## ---- obtain mwh/year for commercial workers for the state ----
  commercial_mwh_per_worker_state <-
    state_nonresidential_energy %>%
    filter(var == "commercial_mwh_per_worker_state") %>%
    select(value)

  ## ---- obtain mwh/year for industrial workers for the state -----
  industrial_mwh_per_worker_state <-
    state_nonresidential_energy %>%
    filter(var == "industrial_mwh_per_worker_state") %>%
    select(value)

  ## ---- obtain commercial therms/worker for  the state ----
  commercial_therms_per_worker_state <-
    state_nonresidential_energy %>%
    filter(var == "commercial_therms_per_worker_state") %>%
    select(value)

  ## ---- obtain industrial therms/worker for the state ----
  industrial_therms_per_worker_state <-
    state_nonresidential_energy %>%
    filter(var == "industrial_therms_per_worker_state") %>%
    select(value)


  # NON-RESIDENTIAL ENERGY BASELINE ----
  # COUNTY ----

  # obtain customer class ratios, so that countywide electricity and natural gas use can be allocated.
  # should be a separate pacakge.

  # ---- electric utility servicewide percent sales by customer class -----
  servicewide_customer_class_ratio <-
   tb$eia_electricity_servicewide %>%
    dplyr::filter(customer_class_name %in% c("Residential",
                                             "Commercial",
                                             "Industrial")) %>%
    dplyr::mutate(mwh_per_year =
                    case_when(is.na(mwh_per_year) ~ 0,
                              mwh_per_year > -1 ~ mwh_per_year)) %>%
    dplyr::select(utility_name,
                  customer_class_name,
                  mwh_per_year) %>%
    group_by(utility_name) %>%
    pivot_wider(names_from = customer_class_name,
                values_from = mwh_per_year) %>%
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
                     by = "mn_doc_co_code") %>%
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
    dplyr::mutate(commercial = commercial,
                  industrial = agriculture + industrial) %>%
    dplyr::select(co_name, utility_name, commercial, industrial, residential) %>%
    dplyr::mutate(
      total = commercial + industrial + residential,
      commercial_percent_by_area = commercial / total,
      industrial_percent_by_area = industrial / total,
      residential_percent_by_area = residential / total
    ) %>%
    dplyr::filter(total > 50) %>%
    select(
      co_name,
      utility_name,
      commercial_percent_by_area,
      industrial_percent_by_area,
      residential_percent_by_area
    )


  ## ----MNDOC countywide energy consumption customer class estimate----------------------------
  mndoc_customer_class_estimate <-
    dplyr::right_join(p_mndoc_electricity_county_utility,
                      servicewide_customer_class_ratio,
                      by = "utility_name") %>%
    right_join(.,
               customer_class_ratio_by_area,
               by = c("utility_name", "co_name")) %>%
    mutate(
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
    pivot_longer(
      cols = c(
        "residential_mwh_county",
        "commercial_mwh_county",
        "industrial_mwh_county"
      ),
      names_to = "var"
    ) %>%
    mutate(year = 2018) %>%
    select(co_name, year, var, value)


  ## ----- estimate county MWh/worker (commercial and industrial) ----
  mwh_per_worker_county <-
    bind_rows(county_electricity,
              county_characteristics %>%
                filter(
                  var %in% c("commercial_workers_county", "industrial_workers_county")
                )) %>%
    pivot_wider(names_from = "var", values_from = "value") %>%
    mutate(
      commercial_mwh_per_worker_county = commercial_mwh_county / commercial_workers_county,
      industrial_mwh_per_worker_county = industrial_mwh_county / industrial_workers_county
    ) %>%
    select(
      co_name,
      year,
      commercial_mwh_per_worker_county,
      industrial_mwh_per_worker_county
    ) %>%
    pivot_longer(
      cols = c(
        "commercial_mwh_per_worker_county",
        "industrial_mwh_per_worker_county"
      ),
      names_to = "var"
    )


  ## ---- compile non-residential energy baseline assumption at the county scale ----
  county_nonresidential_baseline <-
    bind_rows(p_mwh_per_worker_county)


  # NON-RESIDENTIAL ENERGY BASELINE ----
  # CTU ----

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


  ## ---- get commercial/industrial electricity baseline from NREL ----
  nrel_electricity_ctu <-
   tb$nrel_energy_consumption_ctu %>%
    filter(sector %in% c("industrial", "commercial")) %>%
    filter(source == "elec",
           year == "2018") %>%
    select(ctu_name, year, sector, consumption_mmbtu) %>%
    mutate(value = consumption_mmbtu * 0.29307107) %>%
    mutate(var =
             case_when(
               (sector == "industrial") ~ "industrial_mwh_nrel",
               (sector == "commercial") ~ "commercial_mwh_nrel",
             )) %>%
    select(ctu_name, year, var, value)


  ## ---- get available utility natural gas data from 'Emissions' ----
  nonresidential_naturalgas_ctu <-
   tb$utility_natural_gas_by_ctu %>%
    filter(year == 2018) %>%
    filter(customer_class_name %in% c("Business",
                                      # "Industrial",
                                      # "Commercial",
                                      "Non-Residential")) %>%
    group_by(ctu_name, year) %>%
    summarise(
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
    filter(var %in% c("commercial_jobs",
                      "industrial_jobs")) %>%
    mutate(
      state_therms_per_worker =
        case_when(
          var == "commercial_jobs" ~ commercial_therms_per_worker_state[[1]],
          var == "industrial_jobs" ~ industrial_therms_per_worker_state[[1]]
        )
    ) %>%
    mutate(
      var =
        case_when(
          (var == "commercial_jobs") ~ "expected_commercial_therms",
          (var == "industrial_jobs") ~ "expected_industrial_therms"
        ),
      value = value * state_therms_per_worker
    ) %>%
    group_by(ctu_name, year) %>%
    select(ctu_name, year, var, value) %>%
    pivot_wider(names_from = var, values_from = value) %>%
    left_join(
      nonresidential_naturalgas_ctu %>%
        select(ctu_name, year, therms_per_year),
      by = c("ctu_name", "year")
    ) %>%
    mutate(
      commercial_therms = therms_per_year * (
        expected_commercial_therms / (expected_commercial_therms + expected_industrial_therms)
      ),
      industrial_therms = therms_per_year * (
        expected_industrial_therms / (expected_commercial_therms + expected_industrial_therms)
      )
    ) %>%
    select(ctu_name, year, commercial_therms, industrial_therms) %>%
    pivot_longer(cols = c(commercial_therms, industrial_therms),
                 names_to = "var") %>%
    filter(is.na(value) == FALSE)


  ## ---- get NREL natural gas consumption data ----
  nrel_natural_gas_ctu <-
   tb$nrel_energy_consumption_ctu %>%
    filter(sector %in% c("industrial", "commercial")) %>%
    filter(source == "ng",
           year == "2018") %>%
    select(ctu_name, year, sector, consumption_mmbtu) %>%
    mutate(value = consumption_mmbtu * 10) %>%
    mutate(var =
             case_when(
               (sector == "industrial") ~ "industrial_therms_nrel",
               (sector == "commercial") ~ "commercial_therms_nrel",
             )) %>%
    select(ctu_name, year, var, value) %>%
    unique()


  # ------

  ctu_nonresidential_energy_baseline_1 <-
    bind_rows(
      xcel_energy_electricity,
      nrel_electricity_ctu,
      commercial_and_industrial_natural_gas_ctu,
      nrel_natural_gas_ctu
    ) %>%
    pivot_wider(names_from = var, values_from = value) %>%
    transmute(
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
    pivot_longer(
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
    bind_rows(ctu_nonresidential_energy_baseline_1,
              ctu_characteristics %>%
                filter(var %in% c("commercial_jobs", "industrial_jobs"))) %>%
    pivot_wider(names_from = "var", values_from = "value") %>%
    rowwise() %>%
    mutate(
      commercial_therm_per_worker = commercial_therms / commercial_jobs,
      industrial_therm_per_worker = industrial_therms / industrial_jobs,
      commercial_mwh_per_worker = commercial_mwh / commercial_jobs,
      industrial_mwh_per_worker = industrial_mwh / industrial_jobs
    ) %>%
    select(
      ctu_name,
      year,
      commercial_therm_per_worker,
      industrial_therm_per_worker,
      commercial_mwh_per_worker,
      industrial_mwh_per_worker
    ) %>%
    pivot_longer(
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
              ctu_nonresidential_energy_per_worker)


}
