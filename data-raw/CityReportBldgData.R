sfarea_bldg_05 <- run_scenario_building(.single_family_floor_area_growth_pct = 05)
sfarea_bldg_10 <- run_scenario_building(.single_family_floor_area_growth_pct = .1)
sfarea_bldg_15 <- run_scenario_building(.single_family_floor_area_growth_pct = .15)

telework_fxn <- function(.pct) {
  .pct$passenger_all %>%
    filter(ctu == "Minneapolis") %>%
    group_by(ctu, scenario, year) %>%
    summarise(direct = sum(dir_ghg, na.rm = T),
              embodied = sum(ghg_embodied, na.rm = T))
}

telework_ctu <- telework_fxn(sfarea_bldg_100) %>% mutate(pct = 1) %>%
  bind_rows(telework_fxn(sfarea_bldg_80) %>% mutate(pct = .8)) %>%
  bind_rows(telework_fxn(sfarea_bldg_60) %>% mutate(pct = .6)) %>%
  bind_rows(telework_fxn(sfarea_bldg_40) %>% mutate(pct = .4)) %>%
  bind_rows(telework_fxn(sfarea_bldg_20) %>% mutate(pct = .2)) %>%
  pivot_longer(names_to = "type", values_to = "emissions", -c(ctu, scenario, year, pct))


a <- run_scenario_building()
