rm(list = ls())
library(ghg.gert)
library(tidyverse)

.ctu <- "all"
.ctu <- "Eagan"

test <- run_module_waste(
  .selected_ctu = .ctu
  # .waste_reduction_pct = 0.05,
  # .waste_reduction_end = 2035,
  # .source_diversion_start = 2030,
  # .source_diversion_end = 2040,
  # .diverted_to_landfill_pct = 0.05
  # .source_diversion_start = 2030,
  # .source_diversion_end = 2040,
  # .diverted_to_landfill_pct = 0.05
)

test
