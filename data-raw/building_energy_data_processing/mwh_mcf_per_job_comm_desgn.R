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
cprg_ctu <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/refs/heads/main/_meta/data/cprg_ctu.RDS") %>%
  sf::st_drop_geometry() %>%
  filter(county_name %in% c(
    "Anoka",
    "Carver",
    "Dakota",
    "Hennepin",
    "Ramsey",
    "Scott",
    "Washington"
  )) %>%
  select(-statefp, -state_abb, -geoid_wis, -cprg_area) %>%
  mutate(ctu_name = if_else(ctu_class == "TOWNSHIP",
    paste(ctu_name, "Twp."),
    ctu_name
  ))


## Electricity
electricity_mwh_per_job_ctu_2022 <- building_energy_data$electricity_business_ctu %>%
  filter(inventory_year == 2022 & !is.na(geog_name)) %>%
  ungroup() %>%
  select(-imagine_designation) %>%
  left_join(cprg_ctu,
    by = join_by(
      geog_name == ctu_name
    )
  ) %>%
  left_join(jobs,
    by = join_by(
      gnis == geog_id,
      inventory_year
    )
  ) %>%
  select(
    -value_change_from_base,
    -value_change_from_2022
  ) %>%
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
  ) %>%
  select(-total_mwh, -total_jobs)


regional_mwh_per_job_2022 <- electricity_mwh_per_job_ctu_2022 %>%
  summarise(
    total_mwh = sum(mwh, na.rm = TRUE),
    total_jobs = sum(value, na.rm = TRUE)
  ) %>%
  mutate(
    mwh_per_job = total_mwh / total_jobs,
    imagine_designation = "Regional"
  )

imagine_mwh_per_job_2022 <- bind_rows(
  imagineCommDesgn_mwh_per_job_2022,
  regional_mwh_per_job_2022 %>%
    select(
      imagine_designation,
      mwh_per_job
    )
)

## Natural Gas
natural_gas_mcf_per_job_ctu_2022 <- building_energy_data$natural_gas_business_ctu %>%
  filter(inventory_year == 2022 & !is.na(geog_name)) %>%
  ungroup() %>%
  select(-imagine_designation) %>%
  left_join(cprg_ctu,
    by = join_by(
      geog_name == ctu_name
    )
  ) %>%
  left_join(jobs,
    by = join_by(
      gnis == geog_id,
      inventory_year
    )
  ) %>%
  select(
    -value_change_from_base,
    -value_change_from_2022
  ) %>%
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
  ) %>%
  select(-total_mcf, -total_jobs)

regional_mcf_per_job_2022 <- natural_gas_mcf_per_job_ctu_2022 %>%
  summarise(
    total_mcf = sum(mcf, na.rm = TRUE),
    total_jobs = sum(value, na.rm = TRUE)
  ) %>%
  mutate(
    mcf_per_job = total_mcf / total_jobs,
    imagine_designation = "Regional"
  )

imagine_mcf_per_job_2022 <- bind_rows(
  imagineCommDesgn_mcf_per_job_2022,
  regional_mcf_per_job_2022 %>%
    select(
      imagine_designation,
      mcf_per_job
    )
)

imagine_mwh_mcf_per_job <- imagine_mcf_per_job_2022 %>%
  left_join(imagine_mwh_per_job_2022,
    by = join_by(imagine_designation)
  )


usethis::use_data(imagine_mwh_mcf_per_job, overwrite = TRUE)
