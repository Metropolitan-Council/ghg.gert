#' Get Non Residential Energy Baseline
#'
#' @return
#' @export
#'
#' @examples
get_statewide_non_residential_energy <- function(tb = building_energy_data) {
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

  return(state_nonresidential_energy)

}
