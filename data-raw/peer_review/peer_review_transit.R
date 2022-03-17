library(ghg.sp)
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(wesanderson)
library(councilR)

ggplot2::theme_set(
councilR::theme_council(use_showtext = T,
                        use_manual_font_sizes = T))


st_paul_pass <- transportation_data$passenger %>%
  filter(ctu %in% c("St. Paul",
                    "All"),
         !year %in% c("2045",
                      "2050"))

st_paul_freight <- transportation_data$freight %>%
  filter(ctu %in% c("St. Paul",
                    "All"),
         !year %in% c("2045",
                      "2050"))


bau_summary <- run_scenario(pass_tb = st_paul_pass,
                            freight_tb = st_paul_freight,
                            .scenario = "BAU",
                            .electric_scenario = "ER",
                            .aeo_scenario = "REF")

# debug(calc_vmt_forecast)

# browser()
mitigation_trans <-  run_scenario(
  pass_tb = st_paul_pass,
  freight_tb = st_paul_freight,
  .scenario = "strategy_improve_transit",
  .electric_scenario = "ER",
  .aeo_scenario = "REF",
  .transit_avo_pct = 0.20,
  .transit_rider_pct = 0.10
  # .cong_price = 0.10,
  # .gas_tax = 0.05,
  # .av_pct = 0.10,
  # .av_fuel_type = "BEV"
  # .drs_pct = 0.03,
  # .drs_fuel_type = "BEV"
) %>%
  suppressMessages()


mitigation_lu <-  run_scenario(
  pass_tb = st_paul_pass,
  freight_tb = st_paul_freight,
  .scenario = "strategy_land_use",
  .electric_scenario = "ER",
  .aeo_scenario = "REF",
  .pop_dens_pct_change = 0.05,
  .emp_dens_pct_change = 0.05,
  .land_use_diversity_pct_change = 0.05,
  .intersection_design_pct_change = 0.05,
  .job_access_pct_change = 0.05,
  .transit_dist_pct_change = -0.05,
  .comb_5d_impact_pct_change = 0.25
) %>%
  suppressMessages()


mitigation_lu_transit <-  run_scenario(
  pass_tb = st_paul_pass,
  freight_tb = st_paul_freight,
  .scenario = "strategy_land_use_and_transit",
  .electric_scenario = "ER",
  .aeo_scenario = "REF",
  .pop_dens_pct_change = 0.05,
  .emp_dens_pct_change = 0.05,
  .land_use_diversity_pct_change = 0.05,
  .intersection_design_pct_change = 0.05,
  .job_access_pct_change = 0.05,
  .transit_dist_pct_change = -0.05,
  .comb_5d_impact_pct_change = 0.25,
  .transit_avo_pct = 0.20,
  .transit_rider_pct = .10
)  %>%
  suppressMessages()

# Plots -----

all_scen_passenger_vmt <- purrr::map_dfr(
  list(bau_summary,
       mitigation_trans,
       mitigation_lu,
       mitigation_lu_transit
  ),
  function(x){
    x$passenger_all %>%
      filter(mode %in% c(
        "AV",
        "PLDV",
        "DRS"
        # "BU", "BRT",
        # "RU", "RI",
        # "AT"
      )) %>%
      group_by(year, scenario) %>%
      summarize(vmt = sum(vmt, na.rm = T), .groups = "keep")
  }
)

ggplot(all_scen_passenger_vmt,
       aes(x = year,
           y = vmt,
           color = scenario,
           group = scenario)) +
  geom_point() +
  geom_line(alpha = 0.5,
            size = 1) +
  labs(title = "PLDV, AV miles traveled")



all_scen_passenger_dir_ghg <- purrr::map_dfr(
  list(bau_summary,
       mitigation_trans,
       mitigation_lu,
       mitigation_lu_transit
  ),
  function(x){
    x$passenger_all %>%
      filter(mode %in% c(
        "AV",
        "PLDV",
        "DRS"
        # "BU", "BRT",
        # "RU", "RI",
        # "AT"
      )) %>%
      group_by(year, scenario) %>%
      summarize(dir_ghg = sum(dir_ghg, na.rm = T), .groups = "keep")
  }
)


ggplot(all_scen_passenger_dir_ghg,
       aes(x = year,
           y = dir_ghg,
           color = scenario,
           group = scenario)) +
  geom_point() +
  geom_line(alpha = 0.5,
            size = 1) +
  labs(title = "PLDV, AV direct emissions")

