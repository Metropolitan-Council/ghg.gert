# sources

library(tidyverse)

# create template -----
# unique_sources <- pass_transpo %>%
#   select(mode, var) %>%
#   mutate(source = "model name/lit reference") %>%
#   unique()
#
# write.csv(unique_sources, "data-raw/indices/unique_sources.csv")

# returned from Jason -----
uni_sources <- read.csv("~/Documents/MetC_Locals/CD/ghg.sp/data-raw/indices/unique_sources.csv")



uni_sources %>%
  group_by(source_short) %>%
  count()



# "MA3T adapted to MSP by changing state-level populations in MA3T to CTU-level populations. Assume 'central city' in MA3T is one of three core cities in MSP, 'suburban' is other urban CTU, and 'rural' is rural CTU. "
