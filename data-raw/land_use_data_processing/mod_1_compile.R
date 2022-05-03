# Compiler

# RUN ANALYSIS GHG INVENTORY 

getwd()
setwd("./mod_1/run/")

source(knitr::purl("../0_get_data_tables/0_get_data_tables.Rmd"))
source(knitr::purl("../1_preprocess_data/1_preprocess_data.Rmd"))
source(knitr::purl("../2_land_cover_land_use/2_general_values.Rmd"))
source(knitr::purl("../2_land_cover_land_use/3_land_composition.Rmd"))
source(knitr::purl("../2_land_cover_land_use/4_land_by_development_type.Rmd"))
source(knitr::purl("../2_land_cover_land_use/5_scenario_land_use.Rmd"))
source(knitr::purl("../2_land_cover_land_use/6_summed_land_use.Rmd"))
source(knitr::purl("../2_land_cover_land_use/7_land_cover_percentages.Rmd"))
source(knitr::purl("../2_land_cover_land_use/8_land_cover_by_land_use.Rmd"))
source(knitr::purl("../3_strategies/9_tree_planting.Rmd"))
source(knitr::purl("../3_strategies/10_parking_lot.Rmd"))
source(knitr::purl("../3_strategies/11_conservation_tillage.Rmd"))
