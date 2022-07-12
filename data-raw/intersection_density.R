## intersection density

library(dplyr)
library(tidyr)
library(ghg.sp)

inters <- read.csv("data-raw/intersect_cts.csv") %>%
  mutate(inter_type = paste0(inter_type, "-way")) %>%
  tidyr::pivot_wider(
    id_cols = "ctu_name",
    names_from = "inter_type",
    values_from = "count"
  ) %>%
  rowwise() %>%
  mutate(total_inter = sum(across(2:7), na.rm = T))
