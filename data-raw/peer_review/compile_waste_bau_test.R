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


## Need to get regional scale emissions from solid waste, add each city
all_ctus <- waste_data$inventory %>%
  filter(geog_level != "COUNTY") %>%
  group_by(geog_id) %>%
  summarize(geog_name = first(geog_name)) %>%
  relocate(geog_name, .before = geog_id) %>%
  arrange(geog_name) %>%
  deframe()


sw_list <- list()

i <- 1
for (i in seq_along(all_ctus)) {
  .ctu_id <- all_ctus[i]
  .ctu <- names(.ctu_id)

  print(paste0("now running ", .ctu, " (", i, " of ", length(all_ctus), ")"))

  test <- run_module_waste(
    .selected_ctu = .ctu
  )

  df <- rbind(
    test$emissions$inv,
    test$emissions$future
  ) %>%
    group_by(inventory_year, geog_id, geog_name, geog_level) %>%
    summarize(
      value_emissions = sum(value_emissions),
      units_emissions = first(units_emissions),
      sector = first(sector),
      category = first(category),
      data_source = first(data_source),
      factor_source = first(factor_source),
      .groups = "keep"
    ) %>%
    ungroup()


  # add to list
  sw_list[[i]] <- df
}


# combine all data frames in the list
solid_waste_emissions <- bind_rows(sw_list) %>%
  dplyr::select(c(
    inventory_year, geog_id, geog_name, geog_level,
    value_emissions, units_emissions, sector, category
  ))

solid_waste_emissions_regional <- solid_waste_emissions %>%
  group_by(inventory_year) %>%
  summarize(
    value_emissions = sum(value_emissions),
    units_emissions = first(units_emissions),
    sector = first(sector),
    category = first(category)
  ) %>%
  ungroup()


# solid_waste_emissions_regional %>%
#   ggplot() +
#   geom_line(aes(x = inventory_year, y = value_emissions)) +
#   labs(
#     title = "Wastewater emissions",
#     y = "Emissions (metric tons CO2e)",
#     x = NULL
#   )


# wastewater emissions all cities -----------------------------------------

wastewater_emissions <- rbind(
  wastewater_data$inventory,
  wastewater_data$projections
) %>%
  filter(geog_level != "COUNTY") %>%
  arrange(geog_name, inventory_year) %>%
  rename(category = source) %>%
  mutate(sector = "Waste") %>%
  dplyr::select(c(
    inventory_year, geog_id, geog_name, geog_level,
    value_emissions, units_emissions, sector, category
  )) %>%
  mutate(
    units_emissions = "Metric tons CO2e"
  )


wastewater_emissions_regional <- wastewater_emissions %>%
  group_by(inventory_year) %>%
  summarize(
    value_emissions = sum(value_emissions),
    units_emissions = first(units_emissions),
    sector = first(sector),
    category = first(category)
  ) %>%
  ungroup()


# wastewater_emissions_regional %>%
#   ggplot() +
#   geom_line(aes(x = inventory_year, y = value_emissions)) +
#   labs(
#     title = "Wastewater emissions",
#     y = "Emissions (metric tons CO2e)",
#     x = NULL
#   )


# Add natural systems (oh god ooh god) ------------------------------------


ns_list <- list()
# i <- names(all_ctus) %in% "Roseville" %>% which()
i <- 1
for (i in seq_along(all_ctus)) {
  .ctu_id <- all_ctus[i]
  .ctu <- names(.ctu_id)

  print(paste0("now running ", .ctu, " (", i, " of ", length(all_ctus), ")"))

  test <- run_scenario_natural_systems(
    .selected_ctu = .ctu
  ) %>% rename(geog_level = ctu_class)

  test %>%
    filter(!is.na(seq_mtco2e_sqkm)) %>%
    group_by(inventory_year) %>%
    summarize(value_emssions = sum(value_emissions))


  df <- test %>%
    filter(inventory_year >= 2005) %>%
    filter(!is.na(seq_mtco2e_sqkm)) %>%
    group_by(inventory_year, geog_id, geog_name, geog_level) %>%
    summarize(
      value_emissions = sum(value_emissions),
      units_emissions = "Metric tons CO2e",
      sector = "Natural systems",
      category = "Natural systems",
      .groups = "keep"
    )

  # add to list
  ns_list[[i]] <- df
}


natural_systems_sequestration <- bind_rows(ns_list) %>%
  dplyr::select(c(
    inventory_year, geog_id, geog_name, geog_level,
    value_emissions, units_emissions, sector, category
  ))


natural_systems_sequestration_regional <- natural_systems_sequestration %>%
  group_by(inventory_year) %>%
  summarize(
    value_emissions = sum(value_emissions),
    units_emissions = first(units_emissions),
    sector = first(sector),
    category = first(category)
  ) %>%
  ungroup()


bau_region <- rbind(
  natural_systems_sequestration_regional,
  wastewater_emissions_regional,
  solid_waste_emissions_regional
)


bau_ctu <- rbind(
  natural_systems_sequestration,
  wastewater_emissions,
  solid_waste_emissions
)


# write.csv(bau_region,
#           file = "data-raw/peer_review/tmp/bau_region_ww_sw_ns.csv",
#           row.names = FALSE)
# write.csv(bau_ctu,
#           file = "data-raw/peer_review/tmp/bau_ctu_ww_sw_ns.csv",
#           row.names = FALSE)
bau_region %>%
  pull(category) %>%
  unique()

p1 <- bau_region %>%
  ggplot() +
  theme_minimal() +
  geom_ribbon(
    data = . %>% filter(category == "Solid waste"),
    aes(x = inventory_year, ymin = 0, ymax = value_emissions, fill = "#ae017e"),
    alpha = 0.5
  ) +
  geom_ribbon(
    data = . %>% filter(category %in% c("Wastewater", "Solid waste")) %>%
      pivot_wider(names_from = category, values_from = value_emissions) %>%
      mutate(
        total = `Wastewater` + `Solid waste`,
        min = `Solid waste`
      ),
    aes(x = inventory_year, ymin = min, ymax = total, fill = "#f768a1"),
    alpha = 0.5
  ) +
  geom_ribbon(
    data = bau_region %>% filter(category == "Natural systems"),
    aes(x = inventory_year, ymin = value_emissions, ymax = 0, fill = "#7fbc41"),
    alpha = 0.5
  ) +
  geom_hline(yintercept = 0, color = "gray30", linetype = "dashed") +
  geom_vline(xintercept = 2023, color = "gray30", linetype = "dashed") +
  scale_fill_identity(
    name = "Category",
    breaks = c("#f768a1", "#ae017e", "#7fbc41"),
    labels = c("Wastewater", "Solid waste", "Natural systems"),
    guide = "legend"
  ) +
  labs(
    title = "Regional BAU emissions",
    y = "Emissions (metric tons CO2e)",
    x = NULL
  )

# now let's make a stacked bar graph showing regional emissions for each category
# for the years 2005, 2022 and 2050
p2 <- bau_region %>%
  filter(inventory_year %in% c(2005, 2022, 2050)) %>%
  # recode category to save wastewater before solid waste
  mutate(category = factor(category, levels = c("Wastewater", "Solid waste", "Natural systems"))) %>%
  ggplot() +
  theme_minimal() +
  geom_bar(aes(x = as.factor(inventory_year), y = value_emissions, fill = category),
    stat = "identity",
    position = "stack",
    alpha = 0.5
  ) +
  geom_hline(yintercept = 0, color = "gray30", linetype = "dashed") +
  scale_fill_manual(
    name = "Category",
    values = c(
      "Wastewater" = "#f768a1",
      "Solid waste" = "#ae017e",
      "Natural systems" = "#7fbc41"
    )
  ) +
  labs(
    title = NULL,
    y = NULL,
    x = NULL
  )


p_final <- p1 +
  theme(
    legend.position = "none"
  ) +

  p2 +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  plot_layout(widths = c(5, 1))

p_final
# ggsave("data-raw/peer_review/tmp/bau_region_ww_sw_ns.png",
#        plot = p_final,
#        width = 8,
#        height = 4,
#        units = "in",
#        dpi = 300)


bau_region %>%
  filter(category != "Natural systems") %>%
  filter(inventory_year %in% c(2005, 2022, 2050)) %>%
  # calculate sum of wastewater and solid was for each year
  group_by(inventory_year) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  # compared to baseline, how much did emissions change in 2022
  mutate(
    change_from_2005 = value_emissions - value_emissions[inventory_year == 2005],
    pct_change = change_from_2005 / value_emissions[inventory_year == 2005] * 100
  )


waste_data$inventory %>%
  filter(geog_level == "COUNTY" &
    inventory_year == 2022) %>%
  filter(geog_name == "Hennepin County")
