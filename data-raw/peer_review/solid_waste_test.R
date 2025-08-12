rm(list=ls())
library(ghg.ccap)
library(tidyverse)

.ctu <- "all"
.ctu <- "Eagan"

test <- run_module_waste(
  .selected_ctu = .ctu,
  .waste_reduction_pct = 0.05,
  .waste_reduction_end = 2035,
  .source_diversion_start = 2030,
  .source_diversion_end = 2040,
  .diverted_to_landfill_pct = 0.05
  # .source_diversion_start = 2030,
  # .source_diversion_end = 2040,
  # .diverted_to_landfill_pct = 0.05
)







p1 <- rbind(test$activity$inv,
            test$activity$future) %>%
  # create a wedge diagram of activities separated by fill color based on source
  # use ggplot2
  ggplot() +
  geom_area(
    aes(
      x = inventory_year,
      y = value_activity,
      fill = source,
      group = source
    ),
    position = "stack",
    alpha = 0.8, show.legend = FALSE
  ) +
  geom_line(aes(x=inventory_year, y=bau_activity), color="black", size=1, linetype="11") +
  lims(y=c(0,1e5)) +
  labs(title="BAU waste scenario",
       y="Waste generated (metric tons)",
       x=NULL) +

  rbind(
    tb_inv,
    tb_proj_01
  ) %>%
  left_join(tb_bau %>% group_by(inventory_year) %>% summarize(bau_activity = head(bau_activity,1))) %>%
  # create a wedge diagram of activities separated by fill color based on source
  # use ggplot2
  ggplot() +
  geom_area(
    aes(
      x = inventory_year,
      y = value_activity,
      fill = source,
      group = source
    ),
    position = "stack",
    alpha = 0.8, show.legend = FALSE
  ) +
  geom_line(aes(x=inventory_year, y=bau_activity), color="black", size=1, linetype="11") +
  lims(y=c(0,1e5)) +
  labs(
    title = paste0("Waste reduced by ", .waste_reduction_pct*100, "%"),
    y = NULL,
    x = NULL
  ) +
  theme(axis.text.y = element_blank()) +

  rbind(
    tb_inv,
    tb_proj_02
  ) %>%
  left_join(tb_bau %>% group_by(inventory_year) %>% summarize(bau_activity = head(bau_activity,1))) %>%
  # create a wedge diagram of activities separated by fill color based on source
  # use ggplot2
  ggplot() +
  geom_area(
    aes(
      x = inventory_year,
      y = value_activity,
      fill = source,
      group = source
    ),
    position = "stack",
    alpha = 0.8
  ) +
  geom_line(aes(x=inventory_year, y=bau_activity), color="black", size=1, linetype="11") +
  lims(y=c(0,1e5)) +
  labs(
    title = paste0("Waste reduced by ", .waste_reduction_pct*100, "%, with source diversions"),
    y = NULL,
    x = NULL
  ) +
  theme(axis.text.y = element_blank())

