# Data import -----
# Passenger data inputs for all CTUs
pass_transpo <- read_csv("inputs/pass_transpo_dat.csv")
pass_transpo <- arrange(pass_transpo, ctu)
# Freight data inputs for all CTUS
freight_transpo <- read_csv("inputs/freight_transpo_dat.csv")
# Cost inputs (not available for all modes)
cost_factors <- read_csv("inputs/cost_factor_dat.csv")
# GHG factor inputs
ghg_factors <- read_csv("inputs/ghg_factor_dat.csv")
# Annual Energy Outlook inputs for specification of alternative macroeconomic future sensitivity
aeo_factors <- read_csv("inputs/aeo_factor_dat.csv")

# Remove columns that aren't used due to forecast horizon (only if not using full set of available years)
pass_transpo <- subset(pass_transpo, select = -c(`2045`, `2050`))
freight_transpo <- subset(freight_transpo, select = -c(`2045`, `2050`))
cost_factors <- subset(cost_factors, select = -c(`2045`, `2050`))
ghg_factors <- subset(ghg_factors, select = -c(`2045`, `2050`))
aeo_factors <- subset(aeo_factors, select = -c(`2045`, `2050`))


