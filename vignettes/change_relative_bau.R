# Calculate changes relative to BAU for each scenario
# VMT and GHG change
# Calculate the change by year - use BAU table as a basis
del_land <- bau_summary %>% mutate(across(all_of(YRS), ~ (unlist(mit_land_summary %>% select(cur_column())) - .x)))
del_land$scenario <- "LAND"
del_ev <- bau_summary %>% mutate(across(all_of(YRS), ~ (unlist(mit_ev_summary %>% select(cur_column())) - .x)))
del_ev$scenario <- "EV"
del_drs <- bau_summary %>% mutate(across(all_of(YRS), ~ (unlist(mit_drs_summary %>% filter(mode != "DRS") %>% select(cur_column())) - .x)))
del_drs_only <- mit_drs_summary %>%
  filter(mode == "DRS") %>%
  select(everything())
del_drs <- rbind(del_drs, del_drs_only)
del_drs$scenario <- "DRS"
del_av <- bau_summary %>% mutate(across(all_of(YRS), ~ (unlist(mit_av_summary %>% filter(mode != "AV") %>% select(cur_column())) - .x)))
del_av_only <- mit_av_summary %>%
  filter(mode == "AV") %>%
  select(everything())
del_av <- rbind(del_av, del_av_only)
del_av$scenario <- "AV"
del_transit <- bau_summary %>% mutate(across(all_of(YRS), ~ (unlist(mit_transit_summary %>% select(cur_column())) - .x)))
del_transit$scenario <- "TRANS"
del_tele <- bau_summary %>% mutate(across(all_of(YRS), ~ (unlist(mit_tele_summary %>% select(cur_column())) - .x)))
del_tele$scenario <- "TELE"
del_price <- bau_summary %>% mutate(across(all_of(YRS), ~ (unlist(mit_price_summary %>% select(cur_column())) - .x)))
del_price$scenario <- "PRICE"
del_all <- rbind(del_land, del_ev, del_drs, del_av, del_transit, del_tele, del_price)
# Summarize across metrics and transportation sector (passenger/freight)
sum_del_all <- del_all %>%
  group_by(type, output) %>%
  summarise(across(YRS, sum))

# Mode split change by VMT for passenger modes - in 10,000 of annual miles
temp_del_all <- del_all %>%
  filter(output == "VMT") %>%
  mutate(across(all_of(YRS), ~ .x * 10^4))
del_mode_split <- temp_del_all %>%
  group_by(mode) %>%
  summarise(across(YRS, sum))

# Remaining vmt/emissions/fuels/costs after all treatments are applied - excluding DRS and AV
remain_all <- bau_summary %>% mutate(across(all_of(YRS), ~ (unlist(.x + del_land %>% select(cur_column()) + del_ev %>% select(cur_column()) + del_drs %>% filter(mode != "DRS") %>% select(cur_column()) + del_av %>% filter(mode != "AV") %>% select(cur_column()) + del_transit %>% select(cur_column()) + del_tele %>% select(cur_column()) + del_price %>% select(cur_column())))))
# Add DRS and AV results from mitigation treatments
remain_all <- rbind(remain_all, del_drs_only, del_av_only)
# Summarize across metrics and transportation sector (passenger/freight) - including DRS and AV
sum_remain_all <- remain_all %>%
  group_by(type, output) %>%
  summarise(across(YRS, mean))
