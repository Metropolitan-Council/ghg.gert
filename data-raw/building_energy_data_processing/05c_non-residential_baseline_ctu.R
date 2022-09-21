# NON-RESIDENTIAL ENERGY BASELINE ----
# CTU ----

## ---- check if community is served by more than 90% Xcel Energy ----
p_xcel_energy_percent <-
  t_intersect_landuse_utility_service_area_ctu %>%
  group_by(ctu_name, utility_name) %>%
  summarise(acres = sum(acres), .groups = "keep") %>%
  mutate(percent = acres / acres) %>%
  filter(utility_name == "Xcel Energy")

## ---- variable return TRUE if Xcel Energy serves more than 90% ----
p_is_served_by_mostly_xcel <-
  p_xcel_energy_percent %>%
  rowwise() %>%
  mutate(is_excel = if_else(percent > 0.90, TRUE, FALSE))

## ---- get xcel energy mwh/year for the 'business' category ----
p_commercial_industrial_electricity_mwh_xcel <-
  t_utility_electricity_by_ctu %>%
  filter(customer_class_name == "Business") %>%
  filter(year == 2018)

## ---- get xcel energy intensity per customer class ----
p_xcel_energy_electricity <-
  p_ctu_characteristics %>%
  filter(var %in% c(
    "commercial_jobs",
    "industrial_jobs"
  )) %>%
  mutate(
    state_mwh_per_worker =
      case_when(
        (var == "commercial_jobs") ~ p_commercial_mwh_per_worker_state[[1]],
        (var == "industrial_jobs") ~ p_industrial_mwh_per_worker_state[[1]]
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
    p_commercial_industrial_electricity_mwh_xcel %>%
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
  pivot_longer(
    cols = c(commercial_mwh_xcel, industrial_mwh_xcel),
    names_to = "var"
  ) %>%
  filter(is.na(value) == FALSE)


## ---- get commercial/industrial electricity baseline from NREL ----
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
    var =
      case_when(
        (sector == "industrial") ~ "industrial_mwh_nrel",
        (sector == "commercial") ~ "commercial_mwh_nrel",
      )
  ) %>%
  select(ctu_name, year, var, value)


## ---- get available utility natural gas data from 'Emissions' ----
p_nonresidential_naturalgas_ctu <-
  t_utility_natural_gas_by_ctu %>%
  filter(year == 2018) %>%
  filter(customer_class_name %in% c(
    "Business",
    # "Industrial",
    # "Commercial",
    "Non-Residential"
  )) %>%
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
p_commercial_and_industrial_natural_gas_ctu <-
  p_ctu_characteristics %>%
  filter(var %in% c(
    "commercial_jobs",
    "industrial_jobs"
  )) %>%
  mutate(
    state_therms_per_worker =
      case_when(
        var == "commercial_jobs" ~ p_commercial_therms_per_worker_state[[1]],
        var == "industrial_jobs" ~ p_industrial_therms_per_worker_state[[1]]
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
    p_nonresidential_naturalgas_ctu %>%
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
  pivot_longer(
    cols = c(commercial_therms, industrial_therms),
    names_to = "var"
  ) %>%
  filter(is.na(value) == FALSE)


## ---- get NREL natural gas consumption data ----
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
    var =
      case_when(
        (sector == "industrial") ~ "industrial_therms_nrel",
        (sector == "commercial") ~ "commercial_therms_nrel",
      )
  ) %>%
  select(ctu_name, year, var, value) %>%
  unique()


# ------

p_ctu_nonresidential_energy_baseline_1 <-
  bind_rows(
    p_xcel_energy_electricity,
    p_nrel_electricity_ctu,
    p_commercial_and_industrial_natural_gas_ctu,
    p_nrel_natural_gas_ctu
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
p_ctu_nonresidential_energy_per_worker <-
  bind_rows(
    p_ctu_nonresidential_energy_baseline_1,
    p_ctu_characteristics %>%
      filter(var %in% c("commercial_jobs", "industrial_jobs"))
  ) %>%
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
p_ctu_nonresidential_energy_baseline <-
  bind_rows(
    p_ctu_nonresidential_energy_baseline_1,
    p_ctu_nonresidential_energy_per_worker
  )
