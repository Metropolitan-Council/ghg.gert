# NON-RESIDENTIAL ENERGY BASELINE ----
# COUNTY ----

# obtain customer class ratios, so that countywide electricity and natural gas use can be allocated.
# should be a separate pacakge.

# ---- electric utility servicewide percent sales by customer class -----
p_servicewide_customer_class_ratio <-
  t_eia_electricity_servicewide %>%
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
p_temp_mndoc_electricity_county <-
  t_mndoc_electricity_county %>%
  dplyr::left_join(.,
                   t_county %>%
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

p_mndoc_electricity_county_total <-
  p_temp_mndoc_electricity_county %>%
  dplyr::group_by(co_name) %>%
  dplyr::summarise(mwh = sum(mwh_mndoc_total))

p_mndoc_electricity_county_utility <-
  p_temp_mndoc_electricity_county %>%
  dplyr::select(1, 5, 4)

# remove temporary table
p_temp_mndoc_electricity_county %>% remove()


## ---- obtain utility customer class ratio by land use designation -----
p_customer_class_ratio_by_area <-
  t_intersect_landuse_utility_service_area_county %>%
  dplyr::select(co_name, utility_name, type, acres) %>%
  dplyr::group_by(co_name, utility_name, type) %>%
  dplyr::summarise(acres = sum(acres), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = type,
                     values_from = acres,
                     values_fill = 0) %>%
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
p_mndoc_customer_class_estimate <-
  dplyr::right_join(p_mndoc_electricity_county_utility,
                    p_servicewide_customer_class_ratio,
                    by = "utility_name") %>%
  right_join(.,
             p_customer_class_ratio_by_area,
             by = c("utility_name", "co_name")) %>%
  mutate(
    percent_residential = (
      servicewide_percent_residential + residential_percent_by_area
    ) / 2,
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


## ----- estimate countywide energy consumption by customer class ----
p_county_electricity <-
  p_mndoc_customer_class_estimate %>%
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
p_mwh_per_worker_county <-
  bind_rows(p_county_electricity,
            p_county_characteristics %>%
              filter(
                var %in% c("commercial_workers_county", "industrial_workers_county")
              )) %>%
  pivot_wider(names_from = "var", values_from = "value") %>%
  mutate(
    commercial_mwh_per_worker_county = commercial_mwh_county / commercial_workers_county,
    industrial_mwh_per_worker_county = industrial_mwh_county / industrial_workers_county
  ) %>%
  select(co_name,
         year,
         commercial_mwh_per_worker_county,
         industrial_mwh_per_worker_county) %>%
  pivot_longer(
    cols = c(
      "commercial_mwh_per_worker_county",
      "industrial_mwh_per_worker_county"
    ),
    names_to = "var"
  )


## ---- compile non-residential energy baseline assumption at the county scale ----
p_county_nonresidential_baseline <-
  bind_rows(p_mwh_per_worker_county)
