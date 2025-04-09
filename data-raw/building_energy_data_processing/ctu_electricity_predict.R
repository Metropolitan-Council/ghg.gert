#### import random forest models from inventory repo to project mwh demand forward

urbansim <- readRDS("data-raw/meta/urbansim_allyrs.RDS")

busi_rf <- readr::read_rds(
"https://github.com/Metropolitan-Council/ghg-cprg/raw/182-develop-city-utility-demand-model/_energy/data/ctu_business_elec_random_forest.RDS"
)

res_rf <- readr::read_rds(
  "https://github.com/Metropolitan-Council/ghg-cprg/raw/182-develop-city-utility-demand-model/_energy/data/ctu_residential_elec_random_forest.RDS"
)
