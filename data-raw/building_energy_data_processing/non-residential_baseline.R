# import tables
## -------------------------------------------------------------------------------------------
t_eia_electricity_servicewide <- import_from_emissions("metro_energy.vw_eia_electricity_servicewide")
t_mndoc_electricity_county <- import_from_emissions("metro_energy.vw_mndoc_electricity_county")
t_intersect_landuse_utility_service_area_county <- import_from_emissions("metro_energy.vw_intersect_landuse_utility_service_area_county")
t_eia_energy_consumption_state <- import_from_emissions("state_energy.eia_energy_consumption_state")
t_intersect_landuse_utility_service_area_ctu <- import_from_emissions("metro_energy.vw_intersect_landuse_utility_service_area_ctu")
t_state_qcew <- import_from_emissions("state_demographic.vw_state_qcew")
t_county <- import_from_emissions("state_demographic.county")
t_utility_electricity_by_ctu <- import_from_emissions("metro_energy.vw_utility_electricity_by_ctu")
t_nrel_energy_consumption_ctu <- import_from_emissions("metro_energy.vw_nrel_energy_consumption_ctu")
t_utility_natural_gas_by_ctu <- import_from_emissions("metro_energy.vw_utility_natural_gas_by_ctu")

# non-residential baseline

## ----electric serviewide customer ratio of utilities----------------------------------------
p_servicewide_customer_class_ratio <-
  t_eia_electricity_servicewide %>%
  dplyr::filter(customer_class_name %in% c(
    "Residential",
    "Commercial",
    "Industrial"
  )) %>%
  dplyr::mutate(
    mwh_per_year =
      case_when(
        is.na(mwh_per_year) ~ 0,
        mwh_per_year > -1 ~ mwh_per_year
      )
  ) %>%
  dplyr::select(
    utility_name,
    customer_class_name,
    mwh_per_year
  ) %>%
  group_by(utility_name) %>%
  pivot_wider(
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
    utility_name, servicewide_percent_residential,
    servicewide_percent_industrial, servicewide_percent_commercial
  )


## ----MNDOC electricity by county total and by utility---------------------------------------
p_temp_mndoc_electricity_county <-
  t_mndoc_electricity_county %>%
  dplyr::left_join(.,
    t_county %>%
      dplyr::select(co_name, mn_doc_co_code),
    by = "mn_doc_co_code"
  ) %>%
  dplyr::filter(year == 2018) %>%
  dplyr::filter(co_name %in% c(
    "Anoka",
    "Carver",
    "Dakota",
    "Hennepin",
    "Ramsey",
    "Scott",
    "Washington"
  ))

p_mndoc_electricity_county_total <-
  p_temp_mndoc_electricity_county %>%
  dplyr::group_by(co_name) %>%
  dplyr::summarise(mwh = sum(mwh_mndoc_total))

p_mndoc_electricity_county_utility <-
  p_temp_mndoc_electricity_county %>%
  dplyr::select(1, 5, 4)

# remove temporary table
p_temp_mndoc_electricity_county %>% remove()


## ----customer class ratio by land use designation-------------------------------------------
# estimate the ratio of MWh/year that is consumed by county based on the service wide utility territory MWH sales by customer class
# combine the estimated ratio with the land use that intersects with the utility service territory and get an average of the two ratios
p_customer_class_ratio_by_area <-
  t_intersect_landuse_utility_service_area_county %>%
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
  select(co_name, utility_name, commercial_percent_by_area, industrial_percent_by_area, residential_percent_by_area)


## ----MNDOC countywide energy consumption customer class estimate----------------------------
p_mndoc_customer_class_estimate <-
  dplyr::right_join(p_mndoc_electricity_county_utility,
    p_servicewide_customer_class_ratio,
    by = "utility_name"
  ) %>%
  right_join(.,
    p_customer_class_ratio_by_area,
    by = c("utility_name", "co_name")
  ) %>%
  mutate(
    percent_residential = (servicewide_percent_residential + residential_percent_by_area) / 2,
    percent_commercial = (servicewide_percent_commercial + commercial_percent_by_area) / 2,
    percent_industrial = (servicewide_percent_industrial + industrial_percent_by_area) / 2
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


## -------------------------------------------------------------------------------------------
p_county_electricity <-
  p_mndoc_customer_class_estimate %>%
  pivot_longer(
    cols = c(
      "residential_mwh_county",
      "commercial_mwh_county",
      "industrial_mwh_county"
    ),
    names_to = "metric"
  ) %>%
  mutate(year = 2018) %>%
  select(co_name, year, metric, value)
# filter(co_name == p_county)


## -------------------------------------------------------------------------------------------
p_mwh_per_worker_county <-
  bind_rows(
    p_county_electricity,
    p_county_characteristics %>%
      filter(metric %in% c(
        "commercial_workers_county", "industrial_workers_county"
      ))
  ) %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
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
    cols = c("commercial_mwh_per_worker_county", "industrial_mwh_per_worker_county"),
    names_to = "metric"
  )


## -------------------------------------------------------------------------------------------
p_county_nonresidential_baseline <-
  bind_rows(
    p_mwh_per_worker_county
  )


## -------------------------------------------------------------------------------------------
p_electricity_consumption_by_customer_class_state <-
  t_eia_energy_consumption_state %>%
  filter(type == "elec") %>%
  filter(year == 2018) %>%
  filter(
    metric %in% c(
      "electricity_industrial_consumption_mwh",
      "electricity_commercial_consumption_mwh",
      "electricity_residential_consumption_mwh"
    )
  ) %>%
  select(state_name, year, metric, value) %>%
  mutate(
    metric =
      case_when(
        (metric == "electricity_residential_consumption_mwh") ~ "electricity_residential_consumption_mwh_state",
        (metric == "electricity_commercial_consumption_mwh") ~ "electricity_commercial_consumption_mwh_state",
        (metric == "electricity_industrial_consumption_mwh") ~ "electricity_industrial_consumption_mwh_state",
      )
  )

p_employees_by_type_state <-
  t_state_qcew %>%
  filter(year == 2018) %>%
  select(state_name, year, naicstitle, emp) %>%
  mutate(
    type =
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
      )
  ) %>%
  group_by(state_name, year, type) %>%
  summarise(value = sum(emp)) %>%
  rename(metric = type)


## -------------------------------------------------------------------------------------------
p_mwh_per_woker_state <-
  bind_rows(
    p_electricity_consumption_by_customer_class_state,
    p_employees_by_type_state
  ) %>%
  pivot_wider(values_from = "value", names_from = "metric") %>%
  mutate(
    commercial_mwh_per_worker_state = (electricity_commercial_consumption_mwh_state / commercial_employees_state),
    industrial_mwh_per_worker_state = (electricity_industrial_consumption_mwh_state / industrial_employees_state)
  ) %>%
  select(state_name, year, commercial_mwh_per_worker_state, industrial_mwh_per_worker_state) %>%
  pivot_longer(
    cols = c(
      "commercial_mwh_per_worker_state",
      "industrial_mwh_per_worker_state"
    ),
    names_to = "metric"
  )


## -------------------------------------------------------------------------------------------
p_natural_gas_consumption_by_customer_class_state <-
  t_eia_energy_consumption_state %>%
  filter(type == "ng") %>%
  filter(year == 2018) %>%
  filter(
    metric %in% c(
      "natural_gas_industrial_consumption_mmcf",
      "natural_gas_commercial_consumption_mmcf"
    )
  ) %>%
  select(state_name, year, metric, value)


## -------------------------------------------------------------------------------------------
p_therms_per_woker_state <-
  bind_rows(
    p_natural_gas_consumption_by_customer_class_state,
    p_employees_by_type_state
  ) %>%
  pivot_wider(values_from = "value", names_from = "metric") %>%
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
    names_to = "metric"
  )


## -------------------------------------------------------------------------------------------
p_state_nonresidential_energy <-
  bind_rows(
    p_electricity_consumption_by_customer_class_state,
    p_employees_by_type_state,
    p_mwh_per_woker_state,
    p_therms_per_woker_state
  )

## ----xcel energy | commercial and industrial electricity | mwh/year | ctu-------------------
# check if community is serve by more than 90% Xcel Energy
p_xcel_energy_percent <-
  t_intersect_landuse_utility_service_area_ctu %>%
  group_by(ctu_name, utility_name) %>%
  summarise(acres = sum(acres)) %>%
  mutate(percent = acres / acres) %>%
  filter(utility_name == "Xcel Energy")

# variable return TRUE if Xcel Energy serves more than 90%
p_is_served_by_mostly_xcel <-
  p_xcel_energy_percent %>%
  rowwise() %>%
  mutate(is_excel = if_else(percent > 0.90, TRUE, FALSE))

# get xcel energy mwh/year for the 'business' category
p_commercial_industrial_electricity_mwh_xcel <-
  t_utility_electricity_by_ctu %>%
  filter(customer_class_name == "Business") %>%
  filter(year == 2018)

# obtain mwh/year for commercial workers for the state
p_commercial_mwh_per_worker_state <-
  p_state_nonresidential_energy %>%
  filter(metric == "commercial_mwh_per_worker_state") %>%
  select(value)

# obtain mwh/year for industrial workers for the state
p_industrial_mwh_per_worker_state <-
  p_state_nonresidential_energy %>%
  filter(metric == "industrial_mwh_per_worker_state") %>%
  select(value)

# estimate the MWh per customer class using Xcel Energy data, which aggregates
# industrial and commercial. For this I use the number of industrial vs commercial workers
# and the energy use intensity per worker for the customer classes for the state of MN.
p_xcel_energy_electricity <-
  p_ctu_characteristics %>%
  filter(metric %in% c(
    "commercial_jobs",
    "industrial_jobs"
  )) %>%
  mutate(
    state_mwh_per_worker =
      case_when(
        (metric == "commercial_jobs") ~ p_commercial_mwh_per_worker_state[[1]],
        (metric == "industrial_jobs") ~ p_industrial_mwh_per_worker_state[[1]]
      )
  ) %>%
  left_join(p_commercial_industrial_electricity_mwh_xcel) %>%
  mutate(
    value = value * state_mwh_per_worker,
    ratio = value / sum(value),
    mwh = ratio * mwh_per_year
  ) %>%
  select(ctu_name, year, metric, mwh) %>%
  mutate(metric = case_when(
    (metric == "commercial_jobs") ~ "commercial_mwh",
    (metric == "industrial_jobs") ~ "industrial_mwh"
  )) %>%
  rename(value = mwh)


## ----nrel | commercial and industrial electricity | mwh/year | ctu--------------------------
p_nrel_electricity_ctu <-
  t_nrel_energy_consumption_ctu %>%
  filter(sector %in% c("industrial", "commercial")) %>%
  filter(
    source == "elec",
    year == "2018"
  ) %>%
  select(ctu_name, year, sector, consumption_mmbtu) %>%
  mutate(value = consumption_mmbtu * 0.29307107) %>%
  mutate(
    metric =
      case_when(
        (sector == "industrial") ~ "industrial_mwh",
        (sector == "commercial") ~ "commercial_mwh",
      )
  ) %>%
  select(ctu_name, year, metric, value)


## -------------------------------------------------------------------------------------------
p_nonresidential_naturalgas_ctu <-
  t_utility_natural_gas_by_ctu %>%
  filter(year == 2018) %>%
  filter(customer_class_name %in% c(
    "Business",
    "Industrial",
    "Commercial",
    "Non-Residential"
  )) %>%
  group_by(ctu_name, year) %>%
  summarize(
    mcf_per_year = sum(mcf_per_year, na.rm = T),
    number_of_customers = sum(number_of_customers, na.rm = T),
    therms_per_year = sum(therms_per_year, na.rm = T),
    utility_name = paste(utility_name, collapse = ", "),
    customer_class_name = paste(customer_class_name, collapse = ", ")
  )

p_commercial_therms_per_worker_state <-
  p_state_nonresidential_energy %>%
  filter(metric == "commercial_therms_per_worker_state") %>%
  select(value)

p_industrial_therms_per_worker_state <-
  p_state_nonresidential_energy %>%
  filter(metric == "industrial_therms_per_worker_state") %>%
  select(value)

p_commercial_and_industrial_natural_gas_ctu <-
  p_ctu_characteristics %>%
  filter(metric %in% c(
    "commercial_jobs",
    "industrial_jobs"
  )) %>%
  mutate(
    state_therms_per_worker =
      case_when(
        metric == "commercial_jobs" ~ p_commercial_therms_per_worker_state[[1]],
        metric == "industrial_jobs" ~ p_industrial_therms_per_worker_state[[1]]
      )
  ) %>%
  left_join(p_nonresidential_naturalgas_ctu) %>%
  group_by(ctu_name, year, metric) %>%
  mutate(
    value1 = value * state_therms_per_worker,
    ratio = value1 / sum(value1, na.rm = T),
    therms = ratio * therms_per_year
  ) %>%
  select(ctu_name, year, metric, therms) %>%
  mutate(metric = case_when(
    (metric == "commercial_jobs") ~ "commercial_therms",
    (metric == "industrial_jobs") ~ "industrial_therms"
  )) %>%
  rename(value = therms)


## ----nrel | commercial and industrial natural gas | therms | ctu----------------------------
p_nrel_natural_gas_ctu <-
  t_nrel_energy_consumption_ctu %>%
  filter(sector %in% c("industrial", "commercial")) %>%
  filter(
    source == "ng",
    year == "2018"
  ) %>%
  select(ctu_name, year, sector, consumption_mmbtu) %>%
  mutate(value = consumption_mmbtu * 10) %>%
  mutate(
    metric =
      case_when(
        (sector == "industrial") ~ "industrial_therms",
        (sector == "commercial") ~ "commercial_therms",
      )
  ) %>%
  select(ctu_name, year, metric, value) %>%
  unique()


# ------

p_ctu_nonresidential_energy_baseline_1 <-
  purrr::map_dfr(
    unique(p_ctu_characteristics$ctu_name),
    function(ctu_na) {
      # browser()

      # print(ctu_na)
      p_xcel_energy_electricity %<>% filter(ctu_name == ctu_na)
      p_nrel_electricity_ctu %<>% filter(ctu_name == ctu_na)

      p_nrel_natural_gas_ctu %<>% filter(ctu_name == ctu_na)
      p_nonresidential_naturalgas_ctu %<>% filter(ctu_name == ctu_na)
      p_commercial_and_industrial_natural_gas_ctu %<>% filter(ctu_name == ctu_na)
      p_is_served_by_mostly_xcel %<>% filter(ctu_name == ctu_na)

      is_xcel <- if (nrow(p_is_served_by_mostly_xcel) == 0) {
        FALSE
      } else {
        TRUE
      }

      # browser()
      bind_rows( # electricity
        if (is_xcel == TRUE) {
          p_xcel_energy_electricity
        } else if (nrow(p_nrel_electricity_ctu) > 0) {
          p_nrel_electricity_ctu
        } else {
          # browser()
          message(paste0("ELEC missing ", ctu_na))
          p_xcel_energy_electricity
        },
        # natural gas
        if (nrow(p_nrel_natural_gas_ctu) > 0) {
          p_nrel_natural_gas_ctu
        } else if (nrow(p_nonresidential_naturalgas_ctu) > 0) { # &&
          # p_nonresidential_naturalgas_ctu$customer_class_name == "Business") {
          p_commercial_and_industrial_natural_gas_ctu
        } else {
          message(paste0("NG missing ", ctu_na))
          p_commercial_and_industrial_natural_gas_ctu
        }
      )
    }
  )

## -------------------------------------------------------------------------------------------
p_ctu_nonresidential_energy_per_worker <-
  bind_rows(
    p_ctu_nonresidential_energy_baseline_1,
    p_ctu_characteristics %>%
      filter(metric %in% c("commercial_jobs", "industrial_jobs"))
  ) %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
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
  pivot_longer(cols = c(
    "commercial_therm_per_worker",
    "industrial_therm_per_worker",
    "commercial_mwh_per_worker",
    "industrial_mwh_per_worker"
  ), names_to = "metric")


## -------------------------------------------------------------------------------------------
p_ctu_nonresidential_energy_baseline <-
  bind_rows(
    p_ctu_nonresidential_energy_baseline_1,
    p_ctu_nonresidential_energy_per_worker
  )
