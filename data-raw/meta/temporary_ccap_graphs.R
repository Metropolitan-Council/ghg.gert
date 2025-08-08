#### ccap graphics ####
library(dplyr)
library(ggplot2)

### residential building projection and reduction measures

res_bau_miss <- run_scenario_building() %>%
  filter(scenario == "bau",
         is.na(electricity_emissions))

res_bau <- run_scenario_building(.selected_ctu = "Minneapolis") %>%
  dplyr::filter(scenario == "bau") %>%
  group_by(inventory_year, scenario) %>%
  summarize(residential_mwh = sum(residential_mwh),
            residential_mcf = sum(residential_mcf),
            electricity_emissions = sum(electricity_emissions),
            natural_gas_emissions = sum(natural_gas_emissions))

res_50 <- run_scenario_building(.selected_ctu = "Minneapolis",
                                .scenario = "half",
                                .leed_start_year = 2028,
                                .new_sf_homes_leed_gold_pct = 0.5,
                                .new_mf_homes_leed_gold_pct = 0.5,
                                .retrofit_start_year = 2028,
                                .retrofit_end_year = 2050,
                                .existing_sf_retrofit_pct = 0.5,
                                .existing_mf_retrofit_pct = 0.5,
                                # electrification
                                .heatpump_start_year = 2028,
                                .heatpump_end_year = 2050,
                                .sf_heat_pump_pct = 0.5,
                                .mf_heat_pump_pct = 0.5,
                                .app_elec_start_year = 2028,
                                .app_elec_end_year = 2050,
                                .sf_app_elec_pct = 0.5,
                                .mf_app_elec_pct = 0.5) %>%
  dplyr::filter(scenario == "half") %>%
  group_by(inventory_year, scenario) %>%
  summarize(residential_mwh = sum(residential_mwh),
            residential_mcf = sum(residential_mcf),
            electricity_emissions = sum(electricity_emissions),
            natural_gas_emissions = sum(natural_gas_emissions))

res_100 <- run_scenario_building(.selected_ctu = "Minneapolis",
                                 .scenario = "full",
                                .leed_start_year = 2028,
                                .new_sf_homes_leed_gold_pct = 1,
                                .new_mf_homes_leed_gold_pct = 1,
                                .retrofit_start_year = 2028,
                                .retrofit_end_year = 2050,
                                .existing_sf_retrofit_pct = 1,
                                .existing_mf_retrofit_pct = 1,
                                # electrification
                                .heatpump_start_year = 2028,
                                .heatpump_end_year = 2050,
                                .sf_heat_pump_pct = 1,
                                .mf_heat_pump_pct = 1,
                                .app_elec_start_year = 2028,
                                .app_elec_end_year = 2050,
                                .sf_app_elec_pct = 1,
                                .mf_app_elec_pct = 1) %>%
  dplyr::filter(scenario == "full") %>%
  group_by(inventory_year, scenario) %>%
  summarize(residential_mwh = sum(residential_mwh),
            residential_mcf = sum(residential_mcf),
            electricity_emissions = sum(electricity_emissions),
            natural_gas_emissions = sum(natural_gas_emissions))

plot_data <- bind_rows(
  res_bau,
  res_50,
  res_100
) %>%
  mutate(total_emissions = electricity_emissions + natural_gas_emissions)

# Get 2005 BAU total emissions
bau_2005 <- plot_data %>%
  filter(inventory_year == 2005, scenario == "bau") %>%
  pull(total_emissions)

half_2005 <- 0.5 * bau_2005

# Create plotting categories (duplicate 2028 for continuity)
plot_data <- plot_data %>%
  mutate(
    scenario_plot = case_when(
      inventory_year <= 2028 ~ "pre2028_black",
      inventory_year >= 2028 & scenario == "bau" ~ "bau_post2028",
      inventory_year >= 2028 & scenario == "half" ~ "half_post2028",
      inventory_year >= 2028 & scenario == "full" ~ "full_post2028"
    )
  )

plot_data <- bind_rows(
  plot_data,
  plot_data %>% filter(inventory_year == 2028) %>%
    mutate(scenario_plot = case_when(
      scenario == "bau" ~ "bau_post2028",
      scenario == "half" ~ "half_post2028",
      scenario == "full" ~ "full_post2028"
    ))
)

# Prepare special points
asterisks <- tibble::tribble(
  ~inventory_year, ~total_emissions, ~label,
  2030, half_2005, "*",       # 50% reduction
  max(plot_data$inventory_year), 0, "*"   # Net-zero for "full"
)

plot_data <- plot_data %>%
  mutate(scenario_plot = factor(scenario_plot, levels = c(
  "pre2028_black",
  "bau_post2028",
  "half_post2028",
  "full_post2028"
)))

# Plot
ggplot(plot_data, aes(x = inventory_year, y = total_emissions,
                      color = scenario_plot, linetype = scenario_plot,
                      group = interaction(scenario, scenario_plot))) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.8) +
  geom_line(linewidth = 1) +
  geom_text(
    data = asterisks,
    aes(x = inventory_year, y = total_emissions, label = label),
    inherit.aes = FALSE,
    size = 15
  ) +
  scale_color_manual(
    values = c(
      "pre2028_black" = "black",
      "bau_post2028" = "black",
      "half_post2028" = "red",
      "full_post2028" = "blue"
    ),
    labels = c(
      "pre2028_black" = "Inventory",
      "bau_post2028" = "Business as usual",
      "half_post2028" = "Current strategies",
      "full_post2028" = "Net-zero"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "pre2028_black" = "solid",
      "bau_post2028" = "dotdash",
      "half_post2028" = "solid",
      "full_post2028" = "solid"
    ),
    labels = c(
      "pre2028_black" = "Inventory",
      "bau_post2028" = "Business as usual",
      "half_post2028" = "Current strategies",
      "full_post2028" = "Net-zero"
    )
  ) +
  labs(
    x = "Year",
    y = "",
    title = "Residential Emissions (MT CO2e)",
    color = "Scenario",
    linetype = "Scenario"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 22, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )

ggplot(plot_data, aes(x = inventory_year, y = residential_mwh ,
                      color = scenario_plot, linetype = scenario_plot,
                      group = interaction(scenario, scenario_plot))) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.8) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "pre2028_black" = "black",
      "bau_post2028" = "black",
      "half_post2028" = "red",
      "full_post2028" = "blue"
    ),
    labels = c(
      "pre2028_black" = "Inventory",
      "bau_post2028" = "Business as usual",
      "half_post2028" = "Current strategies",
      "full_post2028" = "Net-zero"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "pre2028_black" = "solid",
      "bau_post2028" = "dotdash",
      "half_post2028" = "solid",
      "full_post2028" = "solid"
    ),
    labels = c(
      "pre2028_black" = "Inventory",
      "bau_post2028" = "Business as usual",
      "half_post2028" = "Current strategies",
      "full_post2028" = "Net-zero"
    )
  ) +
  labs(
    x = "Year",
    y = "",
    title = "Residential Electricity Demand (Megawatt Hours)",
    color = "Scenario",
    linetype = "Scenario"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 22, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )




##### import ghg_inventory from ghg_cprg
library(dplyr)
library(readr)

ghg_ctu <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/205-ctu-ghg-compiler/_meta/data/ctu_emissions.RDS")

### need to resolve the below in ghg-cprg repo
ghg_ctu %>%
  filter(is.na(value_emissions)) %>%
  dplyr::distinct(geog_name, sector, category, source)

ghg_county <- readr::read_rds("https://github.com/Metropolitan-Council/ghg-cprg/raw/205-ctu-ghg-compiler/_meta/data/cprg_county_emissions.RDS")

### conform and bind

ghg_inventory_seven <-
  ghg_county %>%
    dplyr::mutate(
      ctu_class = "COUNTY",
      category = as.character(category),
      geog_name = paste(county_name, "County")
    ) %>%
    dplyr::rename(
      population = county_total_population,
      fips_id = geoid
    ) %>%
    dplyr::select(-c(data_source, county_name, factor_source, population_data_source))
) %>% # patch category issue (fix in inventory repo later)
  mutate(
    category = dplyr::case_when(
      sector_alt == "Commercial" ~ "Commercial building fuel",
      sector_alt == "Industrial" & !grepl("processes", category) ~ "Industrial stationary combustion",
      TRUE ~ category
    ),
    sector_alt = dplyr::if_else(sector_alt == "Commercial",
                                "Building fuel",
                                sector_alt
    )
  ) %>%
  # collapse county commercial/industrial building fuel to business
  mutate(sector = if_else(sector %in% c(
    "Commercial",
    "Industrial"
  ) &
    sector_alt != "Industrial",
  "Business",
  sector
  )) %>%
  # reorder sectors
  mutate(
    sector = factor(sector,
                    levels = c(
                      "Transportation",
                      "Residential",
                      "Business",
                      "Industrial",
                      "Waste",
                      "Agriculture",
                      "Natural Systems"
                    )
    ),
    sector_alt = factor(sector_alt,
                        levels = c(
                          "Transportation",
                          "Electricity",
                          "Building fuel",
                          "Industrial",
                          "Waste",
                          "Agriculture",
                          "Natural Systems"
                        )
    ),
    category = factor(category,
                      levels = c(
                        "Passenger vehicles", "Buses", "Trucks", "Off-road",
                        "Business electricity", "Residential electricity",
                        "Business building fuel", "Residential building fuel",
                        "Industrial electricity", "Industrial processes", "Industrial stationary combustion", "Industrial building fuel",
                        "Commercial electricity", "Commercial building fuel",
                        "Refinery processes",
                        "Wastewater", "Solid waste",
                        "Cropland", "Livestock", "Sequestration",
                        "Freshwater"
                      ),
                      ordered = TRUE
    )
  ) %>%
  ### hot fix, needs permanent fix in cprg repo
  mutate(value_emissions = if_else(is.na(value_emissions),
                                   0,
                                   value_emissions
  )) %>%
  # remove refinery emissions
  filter(!(emissions_per_capita > 50 & sector == "Industrial"))
