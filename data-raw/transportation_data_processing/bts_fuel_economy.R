# download from https://www.bts.gov/content/average-fuel-efficiency-us-light-duty-vehicles
bts_fuel_economy <- readxl::read_xlsx("data-raw/transportation_data_processing/table_04_23_042425.xlsx",
  sheet = 2,
  skip = 1,
  col_types = "text"
) %>%
  filter(...1 == "Average U.S. light duty vehicle fuel efficiency (mpg) (calendar year)") %>%
  pivot_longer(
    cols = 2:ncol(.),
    names_to = "veh_year",
    values_to = "mpg"
  ) %>%
  mutate(value = as.numeric(mpg)) %>%
  filter(veh_year %in% c(2015, 2018, 2020)) %>%
  mutate(
    aeo_mode = "LDV",
    mode = "PLDV",
    var = "SIMPG",
    metadata = "U.S. Department of Transportation, Federal Highway Administration, Highway Statistics (Washington, DC: Annual Issues), table VM-1, available at http://www.fhwa.dot.gov/policyinformation/statistics.cfm as of Apr. 1, 2025."
  ) %>%
  select(aeo_mode,
    mode,
    year = veh_year,
    var,
    value,
    metadata
  )
