#### Run residential buildings BAU

library(purrr)
library(dplyr)
library(scales)
library(ggplot2)

ctu_index <- geog_index %>%
  filter(geog_level != "COUNTY",
         !geog_name %in% c("Fort Snelling",
                           "Landfall"))

# regional density_output
density_output <- run_scenario_land_use()

# bau
bau_results <- purrr::map_dfr(
  ctu_index$geog_name,
  ~ run_scenario_building(
    .baseline_year = 2022,
    .selected_ctu = .x,  # iterates across al ctus
    .density_output = density_output,
  ),
  .id = "ctu"
) %>%
  group_by(inventory_year, scenario) %>%
  summarize(mwh = sum(residential_mwh),
            mcf = sum(residential_mcf),
            electricity_emissions = sum(electricity_emissions),
            natgas_emissions = sum(natural_gas_emissions)) %>%
  ungroup() %>%
  filter(scenario == "bau")

# readr::write_csv(bau_results,
#           paste0(wd,"residential_bau.csv"))

net_zero_results <- purrr::map_dfr(
  ctu_index$geog_name,
  ~ run_scenario_building(
    .baseline_year = 2022,
    .scenario = "net_zero",
    .selected_ctu = .x,  # iterates across al ctus
    .density_output = density_output,
    .leed_start_year = 2026,
    .new_sf_homes_leed_gold_pct = 1.0,
    .new_mf_homes_leed_gold_pct = 1.0,
    .retrofit_start_year = 2026,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 1.0,
    .existing_mf_retrofit_pct = 1.0,
    # electrification
    .heatpump_start_year = 2026,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 1.0,
    .mf_heat_pump_pct = 1.0
  ),
  .id = "ctu"
) %>%
  group_by(inventory_year, scenario) %>%
  summarize(mwh = sum(residential_mwh),
            mcf = sum(residential_mcf),
            electricity_emissions = sum(electricity_emissions),
            natgas_emissions = sum(natural_gas_emissions)) %>%
  ungroup() %>%
  filter(scenario == "net_zero")

### Accelerated policy pathways

ppp_results <- purrr::map_dfr(
  ctu_index$geog_name,
  ~ run_scenario_building(
    .baseline_year = 2022,
    .scenario = "ppp",
    .selected_ctu = .x,  # iterates across al ctus
    .density_output = density_output,
    .leed_start_year = 2026,
    .new_sf_homes_leed_gold_pct = 0.5,
    .new_mf_homes_leed_gold_pct = 0.5,
    .retrofit_start_year = 2026,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0.5,
    .existing_mf_retrofit_pct = 0.5,
    # electrification
    .heatpump_start_year = 2026,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0.5,
    .mf_heat_pump_pct = 0.5
  ),
  .id = "ctu"
) %>%
  group_by(inventory_year, scenario) %>%
  summarize(mwh = sum(residential_mwh),
            mcf = sum(residential_mcf),
            electricity_emissions = sum(electricity_emissions),
            natgas_emissions = sum(natural_gas_emissions)) %>%
  ungroup() %>%
  filter(scenario == "ppp")


total_emissions <- bind_rows(
  bau_results %>%
    mutate(total_emissions = electricity_emissions + natgas_emissions),
  net_zero_results %>%
    mutate(total_emissions = electricity_emissions + natgas_emissions),
  ppp_results %>%
    mutate(total_emissions = electricity_emissions + natgas_emissions)
) %>%
  mutate(scenario = factor(scenario,
                           levels = c("bau","ppp","net_zero")))




# prepare the data for different segments and fills

#  base data (2005-2025, identical across scenarios)
base_data <- total_emissions %>%
  filter(inventory_year <= 2025, scenario == "bau") %>%  # Use any scenario since they're identical
  mutate(segment = "base")


#  diverging scenarios (2026+)
diverging_data <- total_emissions %>%
  filter(inventory_year >= 2025) %>%
  mutate(segment = "diverging")

# Prepare data for ribbon fills
# Net zero data for ribbon
net_zero_data <- diverging_data %>% filter(scenario == "net_zero")

# PPP data - need to merge with net_zero for the lower bound
ppp_data <- diverging_data %>%
  filter(scenario == "ppp") %>%
  select(inventory_year, total_emissions) %>%
  rename(ppp_emissions = total_emissions)

net_zero_for_ppp <- diverging_data %>%
  filter(scenario == "net_zero") %>%
  select(inventory_year, total_emissions) %>%
  rename(net_zero_emissions = total_emissions)

ppp_ribbon_data <- ppp_data %>%
  left_join(net_zero_for_ppp, by = "inventory_year")

# Create the plot
emissions_gg <- ggplot() +
  # Base fill (2005-2025, gray)
  geom_ribbon(data = base_data,
              aes(x = inventory_year, ymin = 0, ymax = total_emissions),
              fill = "gray80", alpha = 0.7) +

  # Net zero fill (maroon)
  geom_ribbon(data = net_zero_data,
              aes(x = inventory_year, ymin = 0, ymax = total_emissions),
              fill = "maroon", alpha = 0.3) +

  # PPP fill (from net_zero to ppp)
  geom_ribbon(data = ppp_ribbon_data,
              aes(x = inventory_year, ymin = net_zero_emissions, ymax = ppp_emissions),
              fill = "rosybrown1", alpha = 0.5) +

  # Base line (2005-2025)
  geom_line(data = base_data,
            aes(x = inventory_year, y = total_emissions),
            color = "black", size = 1) +

  # Diverging scenario lines
  geom_line(data = diverging_data %>% filter(scenario == "bau"),
            aes(x = inventory_year, y = total_emissions, color = "Business as usual"),
            linetype = "dashed", size = 1) +

  geom_line(data = diverging_data %>% filter(scenario == "ppp"),
            aes(x = inventory_year, y = total_emissions, color = "Accelerated policy pathways"),
            size = 1) +

  geom_line(data = diverging_data %>% filter(scenario == "net_zero"),
            aes(x = inventory_year, y = total_emissions, color = "Net zero"),
            size = 1) +

  geom_segment(aes(x = 2025, xend = 2025, y = 0, yend = base_data %>% filter(inventory_year == 2025) %>% pull(total_emissions)),
               color = "black", linetype = "solid", size = 0.8) +
  # annotate("text", x = 2025, y = max(your_data$total_emissions) * 0.9,
  #          label = "Historical | Projected", angle = 90, hjust = 1, size = 3.5) +

  # Manual color scale with correct order
  scale_color_manual(
    values = c(
      "Business as usual" = "black",
      "Net zero" = "maroon",
      "Accelerated policy pathways" = "rosybrown3"
    ),
    breaks = c("Business as usual", "Accelerated policy pathways", "Net zero")  # Force legend order
  ) +

  # Manual legend guide to show line types
  guides(
    color = guide_legend(
      title = "Scenarios",
      override.aes = list(
        linetype = c("dashed", "solid", "solid"),
        color = c("black", "rosybrown3", "maroon")
      )
    )
  ) +
  labs(
    x = "Year",
    y = "",
    title = "Residential Building Emissions \n(Millions of CO2-equivalency)"
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-6)) +  # convert to millions
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 18),
    axis.text =  element_text(size = 14),
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.2, "cm")
  ) +
  xlim(2005, 2050)

print(emissions_gg)

ggplot2::ggsave(plot = emissions_gg,
       filename = paste0(wd,"/residential_decarbonization.png"),  # add your file path here
       width = 12,
       height = 6,
       units = "in",
       dpi = 300,
       bg = "white")


### mwh graph ####

# Prepare ribbons
bau_data <- diverging_data %>%
  filter(scenario == "bau") %>%
  select(inventory_year, mwh)
ppp_data <- diverging_data %>%
  filter(scenario == "ppp") %>%
  select(inventory_year, mwh) %>%
  rename(ppp_mwh = mwh)
net_zero_data <- diverging_data %>%
  filter(scenario == "net_zero") %>%
  select(inventory_year, mwh) %>%
  rename(net_zero_mwh = mwh)

# Merge for ribbons
ribbon_ppp <- left_join(bau_data, ppp_data, by = "inventory_year")
ribbon_net_zero <- left_join(ppp_data, net_zero_data, by = "inventory_year")

# Create plot
p_mwh <- ggplot() +
  # Base fill
  geom_ribbon(data = base_data, aes(x = inventory_year, ymin = 0, ymax = mwh),
              fill = "gray80", alpha = 0.7) +

  geom_ribbon(data = bau_data, aes(x = inventory_year, ymin = 0, ymax = mwh),
              fill = "gray70", alpha = 0.7) +

  # PPP ribbon (BAU → PPP)
  geom_ribbon(data = ribbon_ppp, aes(x = inventory_year, ymin = mwh, ymax = ppp_mwh),
              fill = "rosybrown1", alpha = 0.5) +

  # Net Zero ribbon (PPP → Net Zero)
  geom_ribbon(data = ribbon_net_zero, aes(x = inventory_year, ymin = ppp_mwh, ymax = net_zero_mwh),
              fill = "maroon", alpha = 0.3) +

  # base line
  geom_line(data = base_data, aes(x = inventory_year, y = mwh), color = "black", size = 1) +

  # lines
  geom_line(data = diverging_data %>% filter(scenario == "bau"),
            aes(x = inventory_year, y = mwh, color = "Business as usual"),
            linetype = "dashed", size = 1) +
  geom_line(data = diverging_data %>% filter(scenario == "ppp"),
            aes(x = inventory_year, y = mwh, color = "Accelerated policy pathways"),
            size = 1) +
  geom_line(data = diverging_data %>% filter(scenario == "net_zero"),
            aes(x = inventory_year, y = mwh, color = "Net zero"),
            size = 1) +

  scale_color_manual(
    values = c(
      "Business as usual" = "black",
      "Accelerated policy pathways" = "rosybrown3",
      "Net zero" = "maroon"
    ),
    breaks = c("Business as usual", "Accelerated policy pathways", "Net zero")
  ) +
  guides(
    color = guide_legend(
      title = "Scenarios",
      override.aes = list(
        linetype = c("dashed", "solid", "solid"),
        color = c("black", "rosybrown3", "maroon"),
        size = c(1.5, 1.5, 1.5)
      )
    )
  ) +
  labs(
    x = "Year",
    y = "",
    title = "Residential Building Energy (GWh)"
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-3)) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 18),
    axis.text =  element_text(size = 14),
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.2, "cm")
  ) +
  xlim(2005, 2050)

print(p_mwh)


### tabular data


net_zero_efficiency <- purrr::map_dfr(
  ctu_index$geog_name,
  ~ run_scenario_building(
    .baseline_year = 2022,
    .scenario = "net_zero",
    .selected_ctu = .x,  # iterates across al ctus
    .density_output = density_output,
    .leed_start_year = 2026,
    .new_sf_homes_leed_gold_pct = 1.0,
    .new_mf_homes_leed_gold_pct = 1.0,
    .retrofit_start_year = 2026,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 1.0,
    .existing_mf_retrofit_pct = 1.0,
    # electrification
    .heatpump_start_year = 2026,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0
  ),
  .id = "ctu"
) %>%
  group_by(inventory_year, scenario) %>%
  summarize(mwh = sum(residential_mwh),
            mcf = sum(residential_mcf),
            electricity_emissions = sum(electricity_emissions),
            natgas_emissions = sum(natural_gas_emissions)) %>%
  ungroup() %>%
  filter(scenario == "net_zero")

ppp_efficiency <- purrr::map_dfr(
  ctu_index$geog_name,
  ~ run_scenario_building(
    .baseline_year = 2022,
    .scenario = "ppp",
    .selected_ctu = .x,  # iterates across al ctus
    .density_output = density_output,
    .leed_start_year = 2026,
    .new_sf_homes_leed_gold_pct = 0.5,
    .new_mf_homes_leed_gold_pct = 0.5,
    .retrofit_start_year = 2026,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0.5,
    .existing_mf_retrofit_pct = 0.5,
    # electrification
    .heatpump_start_year = 2026,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0,
    .mf_heat_pump_pct = 0
  ),
  .id = "ctu"
) %>%
  group_by(inventory_year, scenario) %>%
  summarize(mwh = sum(residential_mwh),
            mcf = sum(residential_mcf),
            electricity_emissions = sum(electricity_emissions),
            natgas_emissions = sum(natural_gas_emissions)) %>%
  ungroup() %>%
  filter(scenario == "ppp")


net_zero_elec <- purrr::map_dfr(
  ctu_index$geog_name,
  ~ run_scenario_building(
    .baseline_year = 2022,
    .scenario = "net_zero",
    .selected_ctu = .x,  # iterates across al ctus
    .density_output = density_output,
    .leed_start_year = 2026,
    .new_sf_homes_leed_gold_pct = 0,
    .new_mf_homes_leed_gold_pct = 0,
    .retrofit_start_year = 2026,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0,
    .existing_mf_retrofit_pct = 0,
    # electrification
    .heatpump_start_year = 2026,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 1.0,
    .mf_heat_pump_pct = 1.0
  ),
  .id = "ctu"
) %>%
  group_by(inventory_year, scenario) %>%
  summarize(mwh = sum(residential_mwh),
            mcf = sum(residential_mcf),
            electricity_emissions = sum(electricity_emissions),
            natgas_emissions = sum(natural_gas_emissions)) %>%
  ungroup() %>%
  filter(scenario == "net_zero")

ppp_elec <- purrr::map_dfr(
  ctu_index$geog_name,
  ~ run_scenario_building(
    .baseline_year = 2022,
    .scenario = "ppp",
    .selected_ctu = .x,  # iterates across al ctus
    .density_output = density_output,
    .leed_start_year = 2026,
    .new_sf_homes_leed_gold_pct = 0,
    .new_mf_homes_leed_gold_pct = 0,
    .retrofit_start_year = 2026,
    .retrofit_end_year = 2050,
    .existing_sf_retrofit_pct = 0,
    .existing_mf_retrofit_pct = 0,
    # electrification
    .heatpump_start_year = 2026,
    .heatpump_end_year = 2050,
    .sf_heat_pump_pct = 0.5,
    .mf_heat_pump_pct = 0.5
  ),
  .id = "ctu"
) %>%
  group_by(inventory_year, scenario) %>%
  summarize(mwh = sum(residential_mwh),
            mcf = sum(residential_mcf),
            electricity_emissions = sum(electricity_emissions),
            natgas_emissions = sum(natural_gas_emissions)) %>%
  ungroup() %>%
  filter(scenario == "ppp")

strategy_tab <- bau_results %>%
            mutate(bau_emissions = electricity_emissions + natgas_emissions) %>%
            filter(inventory_year %in% c(2030, 2050)) %>%
            select(inventory_year, bau_emissions) %>%
  left_join(net_zero_efficiency  %>%
              mutate(nz_eff_emissions = electricity_emissions + natgas_emissions) %>%
              filter(inventory_year %in% c(2030, 2050)) %>%
              select(inventory_year, nz_eff_emissions)) %>%
  left_join(ppp_efficiency  %>%
              mutate(ppp_eff_emissions = electricity_emissions + natgas_emissions) %>%
              filter(inventory_year %in% c(2030, 2050)) %>%
              select(inventory_year, ppp_eff_emissions))%>%
  left_join(net_zero_elec  %>%
              mutate(nz_elec_emissions = electricity_emissions + natgas_emissions) %>%
              filter(inventory_year %in% c(2030, 2050)) %>%
              select(inventory_year, nz_elec_emissions)) %>%
  left_join(ppp_elec  %>%
              mutate(ppp_elec_emissions = electricity_emissions + natgas_emissions) %>%
              filter(inventory_year %in% c(2030, 2050)) %>%
              select(inventory_year, ppp_elec_emissions))

strategy_tab_diff <- strategy_tab %>%
  mutate(
    nz_eff_diff   = bau_emissions - nz_eff_emissions,
    ppp_eff_diff  = bau_emissions - ppp_eff_emissions,
    nz_elec_diff  = bau_emissions - nz_elec_emissions,
    ppp_elec_diff = bau_emissions - ppp_elec_emissions
  )
