pkgload::load_all()
# telework-----
telework_trans_100 <- run_scenario_transportation(.telework_pct = 1, .selected_ctu = "Minneapolis", .scenario = "t100")
telework_trans_80 <- run_scenario_transportation(.telework_pct = .8, .selected_ctu = "Minneapolis", .scenario = "t80")
telework_trans_60 <- run_scenario_transportation(.telework_pct = .6, .selected_ctu = "Minneapolis", .scenario = "t60")
telework_trans_40 <- run_scenario_transportation(.telework_pct = .4, .selected_ctu = "Minneapolis", .scenario = "t40")
telework_trans_20 <- run_scenario_transportation(.telework_pct = .2, .selected_ctu = "Minneapolis", .scenario = "t20")
telework_trans_0 <- run_scenario_transportation(.telework_pct = 0, .selected_ctu = "Minneapolis", .scenario = "t0")

telework_fxn <- function(.pct) {
  .pct$passenger_all %>%
    filter(ctu == "Minneapolis") %>%
    group_by(ctu, scenario, year, mode) %>%
    summarise(
      direct = sum(dir_ghg, na.rm = T),
      embodied = sum(ghg_embodied, na.rm = T)
    )
}

telework_ctu <- telework_fxn(telework_trans_100) %>%
  mutate(param = 1) %>%
  bind_rows(telework_fxn(telework_trans_80) %>% mutate(param = .8)) %>%
  bind_rows(telework_fxn(telework_trans_60) %>% mutate(param = .6)) %>%
  bind_rows(telework_fxn(telework_trans_40) %>% mutate(param = .4)) %>%
  bind_rows(telework_fxn(telework_trans_20) %>% mutate(param = .2)) %>%
  bind_rows(telework_fxn(telework_trans_0) %>% mutate(param = 0)) %>%
  pivot_longer(names_to = "type", values_to = "emissions", -c(ctu, scenario, year, param, mode))



# electrification-----
bev_trans_100 <- run_scenario_transportation(.bev_pct_sales = 1, .selected_ctu = "Minneapolis", .scenario = "bev100")
bev_trans_80 <- run_scenario_transportation(.bev_pct_sales = .8, .selected_ctu = "Minneapolis", .scenario = "bev80")
bev_trans_60 <- run_scenario_transportation(.bev_pct_sales = .6, .selected_ctu = "Minneapolis", .scenario = "bev600")
bev_trans_40 <- run_scenario_transportation(.bev_pct_sales = .4, .selected_ctu = "Minneapolis", .scenario = "bev40")
bev_trans_20 <- run_scenario_transportation(.bev_pct_sales = .2, .selected_ctu = "Minneapolis", .scenario = "bev20")
bev_trans_0 <- run_scenario_transportation(.bev_pct_sales = 0, .selected_ctu = "Minneapolis", .scenario = "bev0")

bev_fxn <- function(.pct) {
  .pct$passenger_all %>%
    filter(ctu == "Minneapolis") %>%
    group_by(ctu, scenario, year, mode) %>%
    summarise(
      direct = sum(dir_ghg, na.rm = T),
      embodied = sum(ghg_embodied, na.rm = T)
    )
}

bev_ctu <- bev_fxn(bev_trans_100) %>%
  mutate(param = 1) %>%
  bind_rows(bev_fxn(bev_trans_80) %>% mutate(param = .8)) %>%
  bind_rows(bev_fxn(bev_trans_60) %>% mutate(param = .6)) %>%
  bind_rows(bev_fxn(bev_trans_40) %>% mutate(param = .4)) %>%
  bind_rows(bev_fxn(bev_trans_20) %>% mutate(param = .2)) %>%
  bind_rows(bev_fxn(bev_trans_0) %>% mutate(param = .0)) %>%
  pivot_longer(names_to = "type", values_to = "emissions", -c(ctu, scenario, year, param, mode))

# public transit----
transitservice_trans_100 <- run_scenario_transportation(.transit_service_pct = 1, .selected_ctu = "Minneapolis", .scenario = "ts100")
transitservice_trans_80 <- run_scenario_transportation(.transit_service_pct = .8, .selected_ctu = "Minneapolis", .scenario = "ts80")
transitservice_trans_60 <- run_scenario_transportation(.transit_service_pct = .6, .selected_ctu = "Minneapolis", .scenario = "ts60")
transitservice_trans_40 <- run_scenario_transportation(.transit_service_pct = .4, .selected_ctu = "Minneapolis", .scenario = "ts40")
transitservice_trans_20 <- run_scenario_transportation(.transit_service_pct = .2, .selected_ctu = "Minneapolis", .scenario = "ts40")
transitservice_trans_0 <- run_scenario_transportation(.transit_service_pct = 0, .selected_ctu = "Minneapolis", .scenario = "ts0")

transitservice_fxn <- function(.pct) {
  .pct$passenger_all %>%
    filter(ctu == "Minneapolis") %>%
    group_by(ctu, scenario, year, mode) %>%
    summarise(
      direct = sum(dir_ghg, na.rm = T),
      embodied = sum(ghg_embodied, na.rm = T)
    )
}

transitservice_ctu <- telework_fxn(transitservice_trans_100) %>%
  mutate(param = 1) %>%
  bind_rows(transitservice_fxn(transitservice_trans_80) %>% mutate(param = .8)) %>%
  bind_rows(transitservice_fxn(transitservice_trans_60) %>% mutate(param = .6)) %>%
  bind_rows(transitservice_fxn(transitservice_trans_40) %>% mutate(param = .4)) %>%
  bind_rows(transitservice_fxn(transitservice_trans_20) %>% mutate(param = .2)) %>%
  bind_rows(transitservice_fxn(transitservice_trans_0) %>% mutate(param = .0)) %>%
  pivot_longer(names_to = "type", values_to = "emissions", -c(ctu, scenario, year, param, mode))

# road pricing ----
roadprice_trans_100 <- run_scenario_transportation(.cong_price = 1, .selected_ctu = "Minneapolis", .scenario = "rp100")
roadprice_trans_80 <- run_scenario_transportation(.cong_price = .8, .selected_ctu = "Minneapolis", .scenario = "rp80")
roadprice_trans_60 <- run_scenario_transportation(.cong_price = .6, .selected_ctu = "Minneapolis", .scenario = "rp60")
roadprice_trans_40 <- run_scenario_transportation(.cong_price = .4, .selected_ctu = "Minneapolis", .scenario = "rp400")
roadprice_trans_20 <- run_scenario_transportation(.cong_price = .2, .selected_ctu = "Minneapolis", .scenario = "rp20")
roadprice_trans_0 <- run_scenario_transportation(.cong_price = 0, .selected_ctu = "Minneapolis", .scenario = "rp0")

roadprice_fxn <- function(.pct) {
  .pct$passenger_all %>%
    filter(ctu == "Minneapolis") %>%
    group_by(ctu, scenario, year, mode) %>%
    summarise(
      direct = sum(dir_ghg, na.rm = T),
      embodied = sum(ghg_embodied, na.rm = T)
    )
}

roadprice_ctu <- telework_fxn(roadprice_trans_100) %>%
  mutate(param = 1) %>%
  bind_rows(roadprice_fxn(roadprice_trans_80) %>% mutate(param = .8)) %>%
  bind_rows(roadprice_fxn(roadprice_trans_60) %>% mutate(param = .6)) %>%
  bind_rows(roadprice_fxn(roadprice_trans_40) %>% mutate(param = .4)) %>%
  bind_rows(roadprice_fxn(roadprice_trans_20) %>% mutate(param = .2)) %>%
  bind_rows(roadprice_fxn(roadprice_trans_0) %>% mutate(param = .0)) %>%
  pivot_longer(names_to = "type", values_to = "emissions", -c(ctu, scenario, year, param, mode))


# parking policy ----
parking_trans_100 <- run_scenario_transportation(.parking_price = 50, .selected_ctu = "Minneapolis", .scenario = "pk100")
parking_trans_80 <- run_scenario_transportation(.parking_price = 40, .selected_ctu = "Minneapolis", .scenario = "pk80")
parking_trans_60 <- run_scenario_transportation(.parking_price = 30, .selected_ctu = "Minneapolis", .scenario = "pk60")
parking_trans_40 <- run_scenario_transportation(.parking_price = 20, .selected_ctu = "Minneapolis", .scenario = "pk40")
parking_trans_20 <- run_scenario_transportation(.parking_price = 10, .selected_ctu = "Minneapolis", .scenario = "pk20")
parking_trans_0 <- run_scenario_transportation(.parking_price = 0, .selected_ctu = "Minneapolis", .scenario = "pk0")

parking_fxn <- function(.pct) {
  .pct$passenger_all %>%
    filter(ctu == "Minneapolis") %>%
    group_by(ctu, scenario, year, mode) %>%
    summarise(
      direct = sum(dir_ghg, na.rm = T),
      embodied = sum(ghg_embodied, na.rm = T)
    )
}

parking_ctu <- parking_fxn(parking_trans_100) %>%
  mutate(param = 1) %>%
  bind_rows(parking_fxn(parking_trans_80) %>% mutate(param = .8)) %>%
  bind_rows(parking_fxn(parking_trans_60) %>% mutate(param = .6)) %>%
  bind_rows(parking_fxn(parking_trans_40) %>% mutate(param = .4)) %>%
  bind_rows(parking_fxn(parking_trans_20) %>% mutate(param = .2)) %>%
  bind_rows(parking_fxn(parking_trans_0) %>% mutate(param = .0)) %>%
  pivot_longer(names_to = "type", values_to = "emissions", -c(ctu, scenario, year, param, mode))



#  pop density -----
density_neg100 <- run_scenario_transportation(
  .pop_dens_pct_change = -1,
  .selected_ctu = "Minneapolis",
  .scenario = "densn100"
)
density_neg50 <- run_scenario_transportation(
  .pop_dens_pct_change = -.5,
  .selected_ctu = "Minneapolis",
  .scenario = "densn50"
)
density_0 <- run_scenario_transportation(
  .pop_dens_pct_change = 0,
  .selected_ctu = "Minneapolis",
  .scenario = "dens0"
)
density_50 <- run_scenario_transportation(
  .pop_dens_pct_change = .5,
  .selected_ctu = "Minneapolis",
  .scenario = "dens50"
)
density_100 <- run_scenario_transportation(
  .pop_dens_pct_change = 1,
  .selected_ctu = "Minneapolis",
  .scenario = "dens100"
)


density_fxn <- function(.pct) {
  .pct$passenger_all %>%
    filter(ctu == "Minneapolis") %>%
    group_by(ctu, scenario, year, mode) %>%
    summarise(
      direct = sum(dir_ghg, na.rm = T),
      embodied = sum(ghg_embodied, na.rm = T)
    )
}

density_ctu <- density_fxn(density_neg100) %>%
  mutate(param = -1) %>%
  bind_rows(density_fxn(density_neg50) %>% mutate(param = -.5)) %>%
  bind_rows(density_fxn(density_0) %>% mutate(param = 0)) %>%
  bind_rows(density_fxn(density_50) %>% mutate(param = .5)) %>%
  bind_rows(density_fxn(density_100) %>% mutate(param = 1)) %>%
  pivot_longer(names_to = "type", values_to = "emissions", -c(ctu, scenario, year, param, mode))



# employment density  -----
emp_density_neg100 <- run_scenario_transportation(
  .emp_dens_pct_change = -1,
  .selected_ctu = "Minneapolis",
  .scenario = "empdensn100"
)
emp_density_neg50 <- run_scenario_transportation(
  .emp_dens_pct_change = -.5,
  .selected_ctu = "Minneapolis",
  .scenario = "empdensn50"
)
emp_density_0 <- run_scenario_transportation(
  .emp_dens_pct_change = 0,
  .selected_ctu = "Minneapolis",
  .scenario = "empdens0"
)
emp_density_50 <- run_scenario_transportation(
  .emp_dens_pct_change = .5,
  .selected_ctu = "Minneapolis",
  .scenario = "empdens50"
)
emp_density_100 <- run_scenario_transportation(
  .emp_dens_pct_change = 1,
  .selected_ctu = "Minneapolis",
  .scenario = "empdens100"
)


emp_density_fxn <- function(.pct) {
  .pct$passenger_all %>%
    filter(ctu == "Minneapolis") %>%
    group_by(ctu, scenario, year, mode) %>%
    summarise(
      direct = sum(dir_ghg, na.rm = T),
      embodied = sum(ghg_embodied, na.rm = T)
    )
}

emp_density_ctu <- emp_density_fxn(emp_density_neg100) %>%
  mutate(param = -1) %>%
  bind_rows(emp_density_fxn(emp_density_neg50) %>% mutate(param = -.5)) %>%
  bind_rows(emp_density_fxn(emp_density_0) %>% mutate(param = 0)) %>%
  bind_rows(emp_density_fxn(emp_density_50) %>% mutate(param = .5)) %>%
  bind_rows(emp_density_fxn(emp_density_100) %>% mutate(param = 1)) %>%
  pivot_longer(names_to = "type", values_to = "emissions", -c(ctu, scenario, year, param, mode))


# bau-----

bau <- run_scenario_transportation(.selected_ctu = "Minneapolis", .scenario = "BAU")

bau_ctu <- bau$passenger_all %>%
  group_by(ctu, scenario, year, mode) %>%
  summarise(
    direct = sum(dir_ghg, na.rm = T),
    embodied = sum(ghg_embodied, na.rm = T)
  ) %>%
  mutate(param = "BAU") %>%
  pivot_longer(
    names_to = "type",
    values_to = "emissions",
    -c(ctu, scenario, year, param, mode)
  )




# save data -----
save(bev_ctu, telework_ctu, transitservice_ctu, roadprice_ctu, parking_ctu, density_ctu, emp_density_ctu, bau_ctu,
  file = "data-raw/transportation_report_data.rda"
)

beepr::beep()



# telework_trans_100 <- run_scenario_transportation(.telework_pct = 1)
# telework_trans_0 <- run_scenario_transportation(.telework_pct = 0)
#
# # direct ghg emissions don't change even if percent of teleworking varies
# (telework_trans_100$passenger_all) %>%
#   filter(ctu == "Minneapolis",
#          year %in% c("2015", "2040")) %>%
#   mutate(telework = "100%") %>%
#   group_by(year, telework) %>%
#   summarise(sum(dir_ghg, na.rm = T)) %>%
#   full_join(
#     (telework_trans_0$passenger_all) %>%
#       filter(ctu == "Minneapolis",
#              year %in% c("2015", "2040")) %>%
#       mutate(telework = "0%") %>%
#       group_by(year, telework) %>%
#       summarise(sum(dir_ghg, na.rm = T))
#   )
#
# # looks like this is somehow realted to the fact that vmt does not change for pldv with telework
# (telework_trans_100$passenger$PLDV$vmt) %>%
#   filter(ctu == "Minneapolis",
#          year %in% c("2015", "2040")) %>%
#   mutate(telework = "100%") %>%
#   group_by(year, telework) %>%
#   summarise(sum(vmt)) %>%
#   full_join(
#     (telework_trans_0$passenger$PLDV$vmt) %>%
#       filter(ctu == "Minneapolis",
#              year %in% c("2015", "2040")) %>%
#       mutate(telework = "0%") %>%
#       group_by(year, telework) %>%
#       summarise(sum(vmt))
#   )
