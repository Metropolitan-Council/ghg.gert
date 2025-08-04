#
load("data/demographic_data.rda")
load("data/building_energy_data.rda")

jobs <- demographic_data %>%
  filter(sp_categories == "jobs") %>%
  group_by(geog_name, geog_id, sp_categories, geog_level) %>%
  mutate(
    baseline_adjustment = value_change_from_base[inventory_year == 2022][1],
    value_change_from_2022 = value_change_from_base - baseline_adjustment,
    geog_name = gsub("\\s*Twp\\.", "", geog_name)
  ) %>%
  ungroup() %>%
  select(-baseline_adjustment)

# get community designations
cprg_ctu <- readRDS("C:/Users/LimeriSA/Documents/Projects/ghg-cprg/_meta/data/cprg_ctu.RDS") %>%
  select(-geometry)


## Electricity
electricity_mwh_per_job_ctu_2022 <- building_energy_data$electricity_business_ctu %>%
  filter(inventory_year == 2022 && !is.na(geog_name)) %>%
  ungroup() %>%
  left_join(cprg_ctu,
            by = join_by(geog_name == ctu_name,
                         geog_level == ctu_class)
  ) %>%
  left_join(jobs,
            by = join_by(geog_name,
                         geog_level,
                         inventory_year)
  ) %>%
  select(-value_change_from_base,
         -value_change_from_2022) %>%
  mutate(
    mwh_per_job_2022 = mwh / value
  )

imagineCommDesgn_mwh_per_job_2022 <- electricity_mwh_per_job_ctu_2022 %>%
  group_by(imagine_designation) %>%
  summarise(
    total_mwh = sum(mwh, na.rm = TRUE),
    total_jobs = sum(value, na.rm = TRUE)
  ) %>%
  mutate(
    mwh_per_job = total_mwh / total_jobs
  )


regional_mwh_per_job_2022 <- electricity_mwh_per_job_ctu_2022 %>%
  summarise(
    total_mwh = sum(mwh, na.rm = TRUE),
    total_jobs = sum(value, na.rm = TRUE)
  ) %>%
  mutate(
    mwh_per_job = total_mwh / total_jobs
  )



## Natural Gas
natural_gas_mcf_per_job_ctu_2022 <- building_energy_data$natural_gas_business_ctu %>%
  filter(inventory_year == 2022 && !is.na(geog_name)) %>%
  ungroup() %>%
  left_join(cprg_ctu,
            by = join_by(geog_name == ctu_name,
                         geog_level == ctu_class)
  ) %>%
  left_join(jobs,
            by = join_by(geog_name,
                         geog_level,
                         inventory_year)
  ) %>%
  select(-value_change_from_base,
         -value_change_from_2022) %>%
  mutate(
    mcf_per_job_2022 = mcf / value
  )


imagineCommDesgn_mcf_per_job_2022 <- natural_gas_mcf_per_job_ctu_2022 %>%
  group_by(imagine_designation) %>%
  summarise(
    total_mcf = sum(mcf, na.rm = TRUE),
    total_jobs = sum(value, na.rm = TRUE)
  ) %>%
  mutate(
    mcf_per_job = total_mcf / total_jobs
  )

regional_mcf_per_job_2022 <- natural_gas_mcf_per_job_ctu_2022 %>%
  summarise(
    total_mcf = sum(mcf, na.rm = TRUE),
    total_jobs = sum(value, na.rm = TRUE)
  ) %>%
  mutate(
    mcf_per_job = total_mcf / total_jobs
  )
