library(ghg.sp)
library(tidyverse)
library(councilR)


transitservice_60 <- run_scenario_transportation(
  .transit_service_pct = 0.6,
  .transit_avo_pct = 0.6 * 1.6,
  .scenario = "ts60",
  .selected_ctu = "Richfield")

transitservice_40 <- run_scenario_transportation(
  .transit_service_pct = 0.4,
  .transit_avo_pct = 0.4 * 1.6,
  .scenario = "ts40",
  .selected_ctu = "Richfield")

transitservice_0 <- run_scenario_transportation(
  .transit_service_pct = 0,
  .scenario = "ts0",
  .transit_avo_pct = 0,
  .selected_ctu = "Richfield")


transit <-bind_rows( transitservice_60$passenger_all,
                     transitservice_0$passenger_all,
                     transitservice_40$passenger_all) %>%
  filter(year == "2040") %>%
  group_by(ctu, scenario, year) %>% # mode, sector
  summarise(emissions = sum(dir_ghg, na.rm = T)) %>%
  pivot_wider(names_from = scenario, values_from = emissions)


check_avo_pct <- function(avo_pct){

  suppressMessages(
    run_scenario_transportation(
      .transit_service_pct = 0.6,
      .transit_avo_pct = 0.6 * avo_pct,
      .scenario = avo_pct,
      .selected_ctu = "Richfield") %>%
      magrittr::extract2("passenger_all")) %>%
    filter(year == "2040") %>%
    group_by(ctu, scenario, year) %>% # mode, sector
    summarise(emissions = sum(dir_ghg, na.rm = T),
              .groups = "keep")
}

library(furrr)
plan(multisession)

transit_avo_checks <- furrr::future_map_dfr(seq(0, 2, by = 0.01),
                                            check_avo_pct)



ggplot(transit_avo_checks,
       aes(x = scenario * 0.6,
           y = emissions)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = transit$ts0) +
  labs(x = "% increase in transit AVO",
       y = "Emissions",
       title = "Richfield, 60% increase in transit service",
       subtitle = "Direct passenger emissions. Horizontal line shows BAU",
       caption = Sys.Date()) +
  councilR::theme_council(use_showtext = TRUE)
