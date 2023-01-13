telework_trans_100 <- run_scenario_transportation(.telework_pct = 1)
telework_trans_80 <- run_scenario_transportation(.telework_pct = .8)
telework_trans_60 <- run_scenario_transportation(.telework_pct = .6)
telework_trans_40 <- run_scenario_transportation(.telework_pct = .4)
telework_trans_20 <- run_scenario_transportation(.telework_pct = .2)

telework_fxn <- function(.pct) {
  .pct$passenger_all %>%
    filter(ctu == "Minneapolis") %>%
    group_by(ctu, scenario, year) %>%
    summarise(direct = sum(dir_ghg, na.rm = T),
              embodied = sum(ghg_embodied, na.rm = T))
}

telework_ctu <- telework_fxn(telework_trans_100) %>% mutate(pct = 1) %>%
  bind_rows(telework_fxn(telework_trans_80) %>% mutate(pct = .8)) %>%
  bind_rows(telework_fxn(telework_trans_60) %>% mutate(pct = .6)) %>%
  bind_rows(telework_fxn(telework_trans_40) %>% mutate(pct = .4)) %>%
  bind_rows(telework_fxn(telework_trans_20) %>% mutate(pct = .2)) %>%
  pivot_longer(names_to = "type", values_to = "emissions", -c(ctu, scenario, year, pct))


