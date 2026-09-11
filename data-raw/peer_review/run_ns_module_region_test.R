rm(list = ls())
library(ghg.gert)
library(tidyverse)

.ctu <- "Regional"


# inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_nature/data/"
# inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/236-incorporate-restorable-wetlands-into-natural-systems-projections/_nature/data/"
#
# natural_systems_data <- c()
#
# natural_systems_data$land_cover_carbon <- readr::read_rds(paste0(inpath, "land_cover_carbon.rds"))


# read natural_systems_data.rda file in ghg.gert/data
natural_systems_data <- ghg.gert::natural_systems_data

mod_bau <- ghg.gert::run_scenario_natural_systems(
  .selected_ctu = "Regional",
  tb_inv = natural_systems_data$inventory,
  tb_future = natural_systems_data$projections,
  tb_seq = natural_systems_data$land_cover_carbon,
  .enviro_factors = ghg.gert::enviro_factors,

  # # user inputs here!
  .wetland_restore_start = 2025,
  .wetland_restore_end = 2030,
  .wetland_restore_fromGrass_perc = 100, # percent of grassland to convert to wetland
  .wetland_restore_fromBare_perc = 100, # percent of bare land to convert to wetland
  .wetland_restore_fromCrop_perc = 100, # percent of cropland to convert to wetland
  .wetland_restore_fromTree_perc = 5, # percent of forest to convert to wetland

  .forest_restore_start = 2030,
  .forest_restore_end = 2045,
  .forest_restore_fromGrass_perc = 100, # percent of grassland to convert to forest
  .forest_restore_fromBare_perc = 100, # percent of bare land to convert to forest
  .forest_restore_fromCrop_perc = 100, # percent of cropland to convert to forest

  .community_tree_start = 2035,
  .community_tree_end = 2040,
  .community_tree_perc = 65, # percent of developed area to convert to community tree cover

  .pocket_prairie_start = 2040,
  .pocket_prairie_end = 2050,
  .pocket_prairie_perc = 100, # percent of urban grassland area to convert to grassland
)


mod_bau

mod_bau %>%
  # filter(!is.na(value_emissions)) %>%
  ggplot(
    aes(x = inventory_year, y = area, fill = land_cover_type)
  ) +
  # stacked area chart
  geom_area(alpha = 0.6, color = NA, position = "stack") +
  geom_line(
    alpha = 0.9, linewidth = 0.5,
    aes(color = land_cover_type), position = "stack", show.legend = F
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )


ghg.gert::run_scenario_natural_systems(
  .selected_ctu = "Saint Bonifacius",
  tb_inv = natural_systems_data$inventory,
  tb_future = natural_systems_data$projections,
  tb_seq = natural_systems_data$land_cover_carbon,
  .enviro_factors = ghg.gert::enviro_factors,

  # # # user inputs here!
  # .wetland_restore_start = 2025,
  # .wetland_restore_end = 2030,
  # .wetland_restore_fromGrass_perc = 100, # percent of grassland to convert to wetland
  # .wetland_restore_fromBare_perc = 100,  # percent of bare land to convert to wetland
  # .wetland_restore_fromCrop_perc = 100,  # percent of cropland to convert to wetland
  # .wetland_restore_fromTree_perc = 5,  # percent of forest to convert to wetland
  #
  # .forest_restore_start = 2030,
  # .forest_restore_end = 2045,
  # .forest_restore_fromGrass_perc = 100, # percent of grassland to convert to forest
  # .forest_restore_fromBare_perc = 100,  # percent of bare land to convert to forest
  # .forest_restore_fromCrop_perc = 100,  # percent of cropland to convert to forest

  # .community_tree_start = 2035,
  # .community_tree_end = 2040,
  # .community_tree_perc = 65, # percent of developed area to convert to community tree cover

  .pocket_prairie_start = 2040,
  .pocket_prairie_end = 2050,
  .pocket_prairie_perc = 52, # percent of urban grassland area to convert to grassland
) %>%
  # filter(!is.na(value_emissions)) %>%
  ggplot(
    aes(x = inventory_year, y = area, fill = land_cover_type)
  ) +
  # stacked area chart
  geom_area(alpha = 0.6, color = NA, position = "stack") +
  geom_line(
    alpha = 0.9, linewidth = 0.5,
    aes(color = land_cover_type), position = "stack", show.legend = F
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )


mod_bau

mod_bau %>%
  # filter(!is.na(value_emissions)) %>%
  ggplot(
    aes(x = inventory_year, y = area, fill = land_cover_type)
  ) +
  # stacked area chart
  geom_area(alpha = 0.6, color = NA, position = "stack") +
  geom_line(
    alpha = 0.9, linewidth = 0.5,
    aes(color = land_cover_type), position = "stack", show.legend = F
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )


# Old code below ----------------------------------------------------------

lc_county <- readr::read_rds(paste0(inpath, "nlcd_county_landcover_allyrs.rds")) %>%
  mutate(
    geog_name = paste(county_name, "County"),
    geog_id = county_id,
    ctu_class = "COUNTY"
  ) %>%
  ungroup() %>%
  group_by(geog_name, ctu_class, geog_id, inventory_year, land_cover_type) %>%
  dplyr::summarize(area = sum(area), .groups = "keep") %>%
  ungroup()


inventory_start_year <- 2005
inventory_end_year <- 2022
future_years <- 2023:2050


natural_systems_data$county$inventory <- lc_county %>%
  filter(inventory_year %in% seq(inventory_start_year, inventory_end_year, by = 1)) %>%
  # dplyr::select(geog_name, ctu_class, geog_id, inventory_year, land_cover_type, area) %>%
  pivot_wider(names_from = land_cover_type, values_from = area) %>%
  rowwise() %>%
  mutate(TOTAL = rowSums(across(c(
    Bare, Developed_Low, Developed_Med, Developed_High,
    Urban_Grassland, Urban_Tree,
    Cropland, Grassland, Tree, Water,
    Wetland
  )), na.rm = T)) %>%
  ungroup() %>%
  # replace NAs with 0
  mutate(across(everything(), ~ tidyr::replace_na(., 0)))


county_projections_2022 <- natural_systems_data$county$inventory %>%
  filter(inventory_year == inventory_end_year) %>%
  select(-c(inventory_year, TOTAL)) %>%
  pivot_longer(cols = -c(geog_name, geog_id, ctu_class), names_to = "land_cover_type", values_to = "area")


county_projections_null <- county_projections_2022 %>%
  tidyr::crossing(inventory_year = future_years) %>%
  pivot_wider(names_from = "land_cover_type", values_from = "area") %>%
  rowwise() %>%
  mutate(TOTAL = sum(dplyr::c_across(c(
    Bare, Developed_Low, Developed_Med, Developed_High,
    Urban_Grassland, Urban_Tree,
    Cropland, Grassland, Tree, Water,
    Wetland
  )), na.rm = T)) %>%
  ungroup() %>%
  # replace NAs with 0
  mutate(across(everything(), ~ tidyr::replace_na(., 0)))


# Regional inventory by summing county inventories
natural_systems_data$regional$inventory <- natural_systems_data$county$inventory %>%
  group_by(inventory_year) %>%
  summarize(
    Bare = sum(Bare),
    Developed_Low = sum(Developed_Low),
    Developed_Med = sum(Developed_Med),
    Developed_High = sum(Developed_High),
    Urban_Grassland = sum(Urban_Grassland),
    Urban_Tree = sum(Urban_Tree),
    Cropland = sum(Cropland),
    Grassland = sum(Grassland),
    Tree = sum(Tree),
    Water = sum(Water),
    Wetland = sum(Wetland),
    TOTAL = sum(TOTAL)
  ) %>%
  mutate(
    geog_name = "Regional",
    ctu_class = "REGION",
    geog_id = "00000000"
  ) %>%
  dplyr::select(
    geog_name, ctu_class, geog_id, inventory_year,
    Bare, Cropland, Developed_High, Developed_Low, Developed_Med,
    Grassland, Tree, Urban_Grassland, Urban_Tree, Water, Wetland, TOTAL
  )


regional_projections_2022 <- natural_systems_data$regional$inventory %>%
  filter(inventory_year == inventory_end_year) %>%
  select(-c(inventory_year, TOTAL)) %>%
  pivot_longer(cols = -c(geog_name, geog_id, ctu_class), names_to = "land_cover_type", values_to = "area")


natural_systems_data$regional$null_projections <- regional_projections_2022 %>%
  tidyr::crossing(inventory_year = future_years) %>%
  pivot_wider(names_from = "land_cover_type", values_from = "area") %>%
  rowwise() %>%
  mutate(TOTAL = sum(dplyr::c_across(c(
    Bare, Developed_Low, Developed_Med, Developed_High,
    Urban_Grassland, Urban_Tree,
    Cropland, Grassland, Tree, Water,
    Wetland
  )), na.rm = T)) %>%
  ungroup() %>%
  # replace NAs with 0
  mutate(across(everything(), ~ tidyr::replace_na(., 0)))


mod_bau <- ghg.gert::run_scenario_natural_systems(
  .selected_ctu = "Regional",
  tb_inv = natural_systems_data$regional$inventory,
  tb_future = natural_systems_data$regional$null_projections,
  tb_seq = natural_systems_data$land_cover_carbon,
  .enviro_factors = ghg.gert::enviro_factors,

  # user inputs here!
  # .urban_tree_start = 2025,
  # .urban_tree_time = 25,
  # .urban_tree_area_perc = 10,
  # .l2l_start = input$lawnsToLegumes_start_yr,
  # .l2l_time = input$lawnsToLegumes_comp_time,
  # .l2l_area_perc = input$lawnsToLegumes_area_pct,
  # .restoration_start = input$cropRestoration_start_yr,
  # .restoration_time = as.numeric(input$cropRestoration_comp_time),
  # .restoration_area_perc = input$cropRestoration_area_pct
)


#+ Let's max everything out and call it net-zero
#+ Don't worry about potentially wet histosols at this stage
#+ Change to 100% planting trees in developed areas
#+ 5% of croplands to trees
#+ 100% of barren to trees
#+ 10% of grassland to trees

target_urbanTree_2050 <- 100
target_cropland_2050 <- 5
target_bare_2050 <- 100
target_grassland_2050 <- 10
# for wetlands, we need to figure out on a 9 county-scale what we HAVE
# vs. what could be
target_wetland_2050 <- 33


mod_ns <- ghg.gert::run_scenario_natural_systems(
  .selected_ctu = "Regional",
  tb_inv = natural_systems_data$regional$inventory,
  tb_future = natural_systems_data$regional$null_projections,
  tb_seq = natural_systems_data$land_cover_carbon,
  .enviro_factors = ghg.gert::enviro_factors,

  # user inputs here!
  .urban_tree_start = 2025,
  .urban_tree_time = 25,
  .urban_tree_area_perc = target_urbanTree_2050,
  # .l2l_start = input$lawnsToLegumes_start_yr,
  # .l2l_time = input$lawnsToLegumes_comp_time,
  # .l2l_area_perc = input$lawnsToLegumes_area_pct,
  .grassland_area_perc = target_grassland_2050,
  .cropland_area_perc = target_cropland_2050,
  .bare_area_perc = target_bare_2050,
  .wetland_area_perc = target_wetland_2050,
  .restoration_start = 2025,
  .restoration_time = 25,
)


#
# plot_wedge_total <- ggplot() +
#   # Base fill (2005-2025, gray)
#   geom_ribbon(data = mod_ns %>%
#                 filter(inventory_year <= 2025 & !is.na(value_emissions)) %>%
#                 group_by(inventory_year) %>%
#                 summarize(total_emissions = sum(value_emissions, na.rm = T)),
#               aes(x = inventory_year, ymin = 0, ymax = total_emissions),
#               fill = "gray80", alpha = 0.7) +
#
#   # # Net zero fill (maroon)
#   # geom_ribbon(data = net_zero_data,
#   #             aes(x = inventory_year, ymin = 0, ymax = total_emissions),
#   #             fill = "maroon", alpha = 0.3) +
#
#   # PPP fill (from net_zero to ppp)
#   geom_ribbon(data = mod_ns %>%
#                 filter(inventory_year >= 2025 & !is.na(value_emissions)) %>%
#                 group_by(inventory_year) %>%
#                 summarize(total_emissions = sum(value_emissions, na.rm = T)),
#               aes(x = inventory_year, ymin = 0, ymax = total_emissions),
#               fill = "lightgreen", alpha = 0.5) +
#
#   # Base line (2005-2025)
#   geom_line(data = mod_ns %>%
#               filter(inventory_year <= 2025 & !is.na(value_emissions)) %>%
#               group_by(inventory_year) %>%
#               summarize(total_emissions = sum(value_emissions, na.rm = T)),
#             aes(x = inventory_year, y = total_emissions),
#             color = "black", linewidth = 1) +
#
#   # Diverging scenario lines
#   geom_line(data = mod_bau %>%
#               filter(inventory_year >= 2025 & !is.na(value_emissions)) %>%
#               group_by(inventory_year) %>%
#               summarize(total_emissions = sum(value_emissions, na.rm = T)),
#             aes(x = inventory_year, y = total_emissions, color = "Business as usual"),
#             linetype = "dashed", linewidth = 1) +
#
#   geom_line(data = mod_ns %>%
#               filter(inventory_year >= 2025 & !is.na(value_emissions)) %>%
#               group_by(inventory_year) %>%
#               summarize(total_emissions = sum(value_emissions, na.rm = T)),
#             aes(x = inventory_year, y = total_emissions, color = "Accelerated policy pathways"),
#             linewidth = 1) +
#
#   geom_point(
#     data = data.frame(emissions_year = 2050, value_emissions = res_target),
#     aes(x = emissions_year, y = value_emissions),
#     shape = "*",    # asterisk
#     size = 12,     # make larger or smaller
#     stroke = 1.5, # line thickness of the asterisk
#     color = "black"
#   ) +
#
#   geom_segment(aes(x = 2025, xend = 2025, y = 0,
#                    yend = mod_ns %>%
#                      filter(inventory_year >= 2025 & !is.na(value_emissions)) %>%
#                      group_by(inventory_year) %>%
#                      summarize(total_emissions = sum(value_emissions, na.rm = T)) %>%
#                      filter(inventory_year == 2025) %>% pull(total_emissions)),
#                color = "black", linetype = "solid", size = 0.8) +
#   # annotate("text", x = 2025, y = max(your_data$total_emissions) * 0.9,
#   #          label = "Historical | Projected", angle = 90, hjust = 1, size = 3.5) +
#
#   # Manual color scale with correct order
#   scale_color_manual(
#     values = c(
#       "Business as usual" = "black",
#       # "Net zero" = "maroon",
#       "Accelerated policy pathways" = "lightgreen"
#     ),
#     breaks = c("Business as usual", "Accelerated policy pathways", "Net zero")  # Force legend order
#   ) +
#
#   # Manual legend guide to show line types
#   guides(
#     color = guide_legend(
#       title = "Scenarios",
#       override.aes = list(
#         linetype = c("dashed", "solid"),
#         color = c("black", "lightgreen")
#       )
#     )
#   ) +
#   labs(
#     x = "Year",
#     y = "",
#     title = "Sequestration by Natural Systems \n(Millions of CO2-equivalency)"
#   ) +
#   scale_y_continuous(labels = label_number(scale = 1e-6)) +  # convert to millions
#   theme_minimal() +
#   theme(
#     panel.grid.minor = element_blank(),
#     legend.position = "bottom",
#     plot.title = element_text(size = 18),
#     axis.text =  element_text(size = 14),
#     legend.text = element_text(size = 18),
#     legend.key.width = unit(1.2, "cm")
#   ) +
#   xlim(2005, 2050)
#
# print(plot_wedge_total)


plot_emissions <- function(bau, scen1, target) {
  # Aggregate helper
  agg <- function(df) {
    df %>%
      filter(!is.na(value_emissions)) %>%
      group_by(inventory_year) %>%
      summarize(total_emissions = sum(value_emissions, na.rm = TRUE), .groups = "drop")
  }

  scen1_agg <- agg(scen1)
  bau_agg <- agg(bau)

  ggplot() +
    # Base fill (2005–2025, gray)
    geom_ribbon(
      data = scen1_agg %>% filter(inventory_year <= 2025),
      aes(x = inventory_year, ymin = 0, ymax = total_emissions),
      fill = "gray80", alpha = 0.7
    ) +

    # Scenario fill (lightgreen, 2025+)
    geom_ribbon(
      data = scen1_agg %>% filter(inventory_year >= 2025),
      aes(x = inventory_year, ymin = 0, ymax = total_emissions),
      fill = "lightgreen", alpha = 0.5
    ) +

    # Base line (2005–2025)
    geom_line(
      data = scen1_agg %>% filter(inventory_year <= 2025),
      aes(x = inventory_year, y = total_emissions),
      color = "black", linewidth = 1
    ) +

    # Diverging scenario lines
    geom_line(
      data = bau_agg %>% filter(inventory_year >= 2025),
      aes(x = inventory_year, y = total_emissions, color = "Business as usual"),
      linetype = "dashed", linewidth = 1
    ) +
    geom_line(
      data = scen1_agg %>% filter(inventory_year >= 2025),
      aes(x = inventory_year, y = total_emissions, color = "Potential policy pathways"),
      linewidth = 1
    ) +

    # 2050 target point
    geom_point(
      data = data.frame(emissions_year = 2050, value_emissions = target),
      aes(x = emissions_year, y = value_emissions),
      shape = "*", size = 12, stroke = 1.5, color = "black"
    ) +

    # Divider at 2025
    geom_segment(
      aes(
        x = 2025, xend = 2025, y = 0,
        yend = scen1_agg %>% filter(inventory_year == 2025) %>% pull(total_emissions)
      ),
      color = "black", linetype = "solid", size = 0.8
    ) +

    # Manual color scale
    scale_color_manual(
      values = c(
        "Business as usual" = "black",
        "Potential policy pathways" = "lightgreen"
      ),
      breaks = c("Business as usual", "Potential policy pathways", "Net zero")
    ) +
    guides(
      color = guide_legend(
        title = "Scenarios",
        override.aes = list(
          linetype = c("dashed", "solid"),
          color = c("black", "lightgreen")
        )
      )
    ) +
    labs(
      x = "Year",
      y = "",
      title = "Sequestration by Natural Systems \n(Millions of CO2-equivalency)"
    ) +
    scale_y_continuous(labels = label_number(scale = 1e-6)) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 18),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 18),
      legend.key.width = unit(1.2, "cm")
    ) +
    xlim(2005, 2050)
}


p1 <- plot_emissions(
  bau = mod_bau,
  scen1 = ghg.gert::run_scenario_natural_systems(
    .selected_ctu = "Regional",
    tb_inv = natural_systems_data$regional$inventory,
    tb_future = natural_systems_data$regional$null_projections,
    tb_seq = natural_systems_data$land_cover_carbon,
    .enviro_factors = ghg.gert::enviro_factors,

    # # user inputs here!
    # .urban_tree_start = 2025,
    # .urban_tree_time = 25,
    # .urban_tree_area_perc = target_urbanTree_2050,
    # .l2l_start = input$lawnsToLegumes_start_yr,
    # .l2l_time = input$lawnsToLegumes_comp_time,
    # .l2l_area_perc = input$lawnsToLegumes_area_pct,
    .grassland_area_perc = target_grassland_2050,
    .cropland_area_perc = target_cropland_2050,
    .bare_area_perc = target_bare_2050,
    .restoration_start = 2025,
    .restoration_time = 25,
  ),
  target = 0
)


p2 <- plot_emissions(
  bau = mod_bau,
  scen1 = ghg.gert::run_scenario_natural_systems(
    .selected_ctu = "Regional",
    tb_inv = natural_systems_data$regional$inventory,
    tb_future = natural_systems_data$regional$null_projections,
    tb_seq = natural_systems_data$land_cover_carbon,
    .enviro_factors = ghg.gert::enviro_factors,

    # user inputs here!
    .urban_tree_start = 2025,
    .urban_tree_time = 25,
    .urban_tree_area_perc = target_urbanTree_2050,
    # .l2l_start = input$lawnsToLegumes_start_yr,
    # .l2l_time = input$lawnsToLegumes_comp_time,
    # .l2l_area_perc = input$lawnsToLegumes_area_pct,
    # .grassland_area_perc = target_grassland_2050,
    # .cropland_area_perc = target_cropland_2050,
    # .bare_area_perc = target_bare_2050,
    #
    # .restoration_start = 2025,
    # .restoration_time = 25,
  ),
  target = 0
)


p3 <- plot_emissions(
  bau = mod_bau,
  scen1 = ghg.gert::run_scenario_natural_systems(
    .selected_ctu = "Regional",
    tb_inv = natural_systems_data$regional$inventory,
    tb_future = natural_systems_data$regional$null_projections,
    tb_seq = natural_systems_data$land_cover_carbon,
    .enviro_factors = ghg.gert::enviro_factors,

    # user inputs here!
    .urban_tree_start = 2025,
    .urban_tree_time = 25,
    .urban_tree_area_perc = target_urbanTree_2050,
    # .l2l_start = input$lawnsToLegumes_start_yr,
    # .l2l_time = input$lawnsToLegumes_comp_time,
    # .l2l_area_perc = input$lawnsToLegumes_area_pct,
    .grassland_area_perc = target_grassland_2050,
    .cropland_area_perc = target_cropland_2050,
    .bare_area_perc = target_bare_2050,
    .restoration_start = 2025,
    .restoration_time = 25,
  ),
  target = 0
)


p1 + p2 + p3


target_seq_for_netZero <- ghg.gert::run_scenario_natural_systems(
  .selected_ctu = "Regional",
  tb_inv = natural_systems_data$regional$inventory,
  tb_future = natural_systems_data$regional$null_projections,
  tb_seq = natural_systems_data$land_cover_carbon,
  .enviro_factors = ghg.gert::enviro_factors,

  # user inputs here!
  .urban_tree_start = 2025,
  .urban_tree_time = 25,
  .urban_tree_area_perc = target_urbanTree_2050,
  # .l2l_start = input$lawnsToLegumes_start_yr,
  # .l2l_time = input$lawnsToLegumes_comp_time,
  # .l2l_area_perc = input$lawnsToLegumes_area_pct,
  .grassland_area_perc = min(100, c(2 * target_grassland_2050)),
  .cropland_area_perc = min(100, c(2 * target_cropland_2050)),
  .bare_area_perc = min(100, c(2 * target_bare_2050)),
  .restoration_start = 2025,
  .restoration_time = 25,
) %>%
  filter(inventory_year == 2050) %>%
  summarize(total_emissions = sum(value_emissions, na.rm = T)) %>%
  pull(total_emissions)


seq_gg <- plot_emissions(
  bau = mod_bau,
  scen1 = ghg.gert::run_scenario_natural_systems(
    .selected_ctu = "Regional",
    tb_inv = natural_systems_data$regional$inventory,
    tb_future = natural_systems_data$regional$null_projections,
    tb_seq = natural_systems_data$land_cover_carbon,
    .enviro_factors = ghg.gert::enviro_factors,

    # user inputs here!
    .urban_tree_start = 2025,
    .urban_tree_time = 25,
    .urban_tree_area_perc = target_urbanTree_2050,
    .grassland_area_perc = target_grassland_2050,
    .cropland_area_perc = target_cropland_2050,
    .bare_area_perc = target_bare_2050,
    .restoration_start = 2025,
    .restoration_time = 25,
  ),
  target = target_seq_for_netZero
)


ggplot2::ggsave(
  plot = seq_gg,
  filename = paste0(here::here(), "/imgs/ns_decarbonization_pathways.png"), # add your file path here
  width = 12,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)


geography_name <- mod_ns$geog_name %>%
  unique()


lc_brks <- c(
  "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
  "Cropland", "Developed_Low", "Developed_Med", "Developed_High",
  "Bare", "Water"
)
lc_labs <- c(
  "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland",
  "Cropland", "Developed (Low)", "Developed (Med)",
  "Developed (High)", "Bare", "Water"
)


plot_colors <- c(
  "Tree" = "#4CAF50",
  "Grassland" = "#FFEB3B",
  "Wetland" = "#75D4D9",
  "Urban Tree" = "#B6E39A",
  "Urban Grassland" = "#D4CA6F",
  "Cropland" = "orange",
  "Developed (Low)" = "#FFB9A8",
  "Developed (Med)" = "#FF8E75",
  "Developed (High)" = "#FF5733",
  "Bare" = "#A9A9A9",
  "Water" = "#1E90FF"
)


plot_activity <-
  mod_ns %>%
  dplyr::mutate(
    land_cover_type = factor(land_cover_type, levels = rev(lc_brks)),
    land_cover_label = dplyr::recode(land_cover_type, !!!setNames(lc_labs, lc_brks)),
    hover = paste0(
      # geog_name, "<br>",
      land_cover_label, ", ", inventory_year, "<br>",
      scales::comma(area, accuracy = 0.01), " km2"
    )
  ) %>%
  plot_ly(
    type = "scatter",
    mode = "lines",
    fill = "tonexty",
    stackgroup = "one",
    # line(list(color = '#FFFFFF')),
    alpha = 0.7,
    alpha_stroke = 1,
    # opacity = 0.8,
    x = ~inventory_year,
    y = ~area,
    color = ~land_cover_label,
    colors = plot_colors,
    hoverinfo = "text",
    text = ~hover
  ) %>%
  councilR::plotly_layout(
    main_title = "Land cover projections",
    subtitle = geography_name,
    x_title = "Year",
    y_title = "Area (km<sup>2</sup>)"
    # y_title = "Metric tons CO<sub>2</sub>"
  ) %>%
  plotly::layout(
    legend = list(
      orientation = "v",
      x = 1.05,
      y = 1,
      xanchor = "left",
      yanchor = "top"
    ),
    # hovermode = "x unified",
    hoverdistance = "10",
    hoverlabel = list(
      font = list(
        size = 18,
        family = "Arial Narrow",
        color = colors$suppBlack
      ),
      # bgcolor = "white",
      stroke = list(
        colors$suppGray,
        colors$suppGray,
        colors$suppGray,
        colors$suppGray
      )
    ),
    shapes = list(
      list(
        type = "line",
        x0 = 2025, # X-coordinate where the vertical line starts
        x1 = 2025, # X-coordinate where the vertical line ends
        y0 = 0, # Y-coordinate where the vertical line starts
        y1 = 1, # Y-coordinate where the vertical line ends (relative to the plot area)
        xref = "x", # Reference to the x-axis
        yref = "paper", # Reference to the y-axis (relative to the plot area)
        line = list(color = "gray20", width = 2, dash = "dash") # Line style
      )
    )
  ) %>%
  plotly::config(displayModeBar = FALSE)


plot_emissions <- mod_ns %>%
  dplyr::mutate(
    land_cover_type = factor(land_cover_type, levels = rev(lc_brks)),
    land_cover_label = dplyr::recode(land_cover_type, !!!setNames(lc_labs, lc_brks)),
    hover = paste0(
      # geog_name, "<br>",
      land_cover_label, ", ", inventory_year, "<br>",
      scales::comma(value_emissions, accuracy = 0.01), " MT CO<sub>2</sub>"
    )
  ) %>%
  plot_ly(
    type = "scatter",
    mode = "lines",
    fill = "tonexty",
    stackgroup = "one",
    # line(list(color = '#FFFFFF')),
    alpha = 0.7,
    alpha_stroke = 1,
    # opacity = 0.2,
    x = ~inventory_year,
    y = ~value_emissions,
    color = ~land_cover_label,
    colors = plot_colors,
    hoverinfo = "text",
    text = ~hover
  ) %>%
  councilR::plotly_layout(
    main_title = "Sequestration by natural systems",
    subtitle = geography_name,
    x_title = "Year",
    # y_title = "Area (km<sup>2</sup>)"
    y_title = "Metric tons CO<sub>2</sub>"
  ) %>%
  plotly::layout(
    legend = list(
      orientation = "v",
      x = 1.05,
      y = 1,
      xanchor = "left",
      yanchor = "top"
    ),
    # hovermode = "x unified",
    hoverdistance = "10",
    hoverlabel = list(
      font = list(
        size = 18,
        family = "Arial Narrow",
        color = colors$suppBlack
      ),
      # bgcolor = "white",
      stroke = list(
        colors$suppGray,
        colors$suppGray,
        colors$suppGray,
        colors$suppGray
      )
    ),
    shapes = list(
      list(
        type = "line",
        x0 = 2025, # X-coordinate where the vertical line starts
        x1 = 2025, # X-coordinate where the vertical line ends
        y0 = 0, # Y-coordinate where the vertical line starts
        y1 = 1, # Y-coordinate where the vertical line ends (relative to the plot area)
        xref = "x", # Reference to the x-axis
        yref = "paper", # Reference to the y-axis (relative to the plot area)
        line = list(color = "gray20", width = 2, dash = "dash") # Line style
      ),
      list(
        type = "line",
        x0 = 2005, # X-coordinate where the vertical line starts
        x1 = 2050, # X-coordinate where the vertical line ends
        y0 = mod_ns %>%
          filter(inventory_year == 2022) %>%
          summarize(value_emissions = sum(value_emissions, na.rm = T)) %>%
          pull(value_emissions), # Y-coordinate where the vertical line starts
        y1 = 0, # Y-coordinate where the vertical line ends (relative to the plot area)
        xref = "paper", # Reference to the x-axis
        yref = "y", # Reference to the y-axis (relative to the plot area)
        line = list(color = "gray20", width = 2, dash = "dash") # Line style
      )
    )
  ) %>%
  plotly::config(displayModeBar = FALSE)


plot_activity
plot_emissions


# Make a base tibble
test_regional_inv <- expand_grid(
  inventory_year = 2005:2022,
  source = c(
    "Landfill", "MSW_Compost", "Onsite", "Organics",
    "Recycling", "Waste to energy", "Wastewater"
  )
) %>%
  mutate(
    geog_id = "00000000",
    geog_name = "Regional",
    geog_level = "REGION",
    # fake population: starts at 100,000 in 2005, grows by ~1% per year
    geog_pop = round(100000 * (1.01^(inventory_year - 2005))),
    # assign some dummy values (different rules depending on source)
    value_activity = case_when(
      source == "Landfill" ~ round(runif(n(), 5000, 7000)),
      source == "MSW_Compost" ~ round(runif(n(), 100, 300)),
      source == "Onsite" ~ round(runif(n(), 50, 150)),
      source == "Organics" ~ round(runif(n(), 1000, 3000)),
      source == "Recycling" ~ round(runif(n(), 4000, 6000)),
      source == "Waste to energy" ~ round(runif(n(), 2000, 4000)),
      source == "Wastewater" ~ NA_real_
    ),
    units_activity = case_when(
      source == "Wastewater" ~ "use population as scalar",
      TRUE ~ "metric tons MSW"
    ),
    data_type = case_when(
      source == "Wastewater" ~ "use population as scalar",
      TRUE ~ "dummy placeholder data"
    )
  )


test_regional_proj <- expand_grid(
  inventory_year = 2023:2050,
  source = c(
    "Landfill", "MSW_Compost", "Onsite", "Organics",
    "Recycling", "Waste to energy", "Wastewater"
  )
) %>%
  mutate(
    geog_id = "00000000",
    geog_name = "Regional",
    geog_level = "REGION",
    # fake population: starts at 100,000 in 2005, grows by ~1% per year
    geog_pop = round(max(test_regional_inv$geog_pop) * (1.01^(inventory_year - 2023))),
    # assign some dummy values (different rules depending on source)
    value_activity = case_when(
      source == "Landfill" ~ round(runif(n(), 5000, 7000)),
      source == "MSW_Compost" ~ round(runif(n(), 100, 300)),
      source == "Onsite" ~ round(runif(n(), 50, 150)),
      source == "Organics" ~ round(runif(n(), 1000, 3000)),
      source == "Recycling" ~ round(runif(n(), 4000, 6000)),
      source == "Waste to energy" ~ round(runif(n(), 2000, 4000)),
      source == "Wastewater" ~ NA_real_
    ),
    units_activity = case_when(
      source == "Wastewater" ~ "use population as scalar",
      TRUE ~ "metric tons MSW"
    ),
    data_type = case_when(
      source == "Wastewater" ~ "use population as scalar",
      TRUE ~ "dummy placeholder data"
    )
  )


test_solid_waste_baseline <-
  ghg.gert::waste_data$inventory %>%
  filter(inventory_year == 2022 & source != "Wastewater") %>%
  filter(geog_name == "Anoka") %>%
  mutate(
    geog_name = "Regional",
    geog_id = "00000000",
    geog_level = "REGION"
  ) %>%
  dplyr::select(-data_type, -inventory_year) %>%
  group_by(geog_id) %>%
  mutate(
    total_activity = sum(value_activity),
    pct_of_total = value_activity / total_activity
  ) %>%
  ungroup() %>%
  left_join(
    tibble(
      source = c("Recycling", "Organics", "Waste to energy", "Landfill", "Onsite", "MSW_Compost"),
      # Add MPCA target percentages for 2030
      # From MPCA Metropolitan Solid Waste Management Policy Plan 2022-2042
      # Table 2: MMSW management system objectives in percentages (2021-2042)
      target2030 = c(0.474, 0.276, 0.2, 0.05, 0, 0)
    )
  ) %>%
  mutate(
    changeFromTarget = pct_of_total - target2030,
    action = case_when(
      changeFromTarget < 0 ~ paste0("Increase ", source),
      changeFromTarget > 0 ~ paste0("Decrease ", source),
      TRUE ~ paste0("Keep ", source)
    )
  )


test <- run_module_waste(
  .selected_ctu = "Regional",
  tb_inv = test_regional_inv,
  tb_future = test_regional_proj,
  tb_base = test_solid_waste_baseline,
  tb_char = ghg.gert::waste_data$characterization,
  tb_target = ghg.gert::waste_data$mpca,
  .waste_reduction_pct = 0.1,
  .waste_reduction_start = 2025,
  .waste_reduction_end = 2030,
  .source_diversion_start = 2030,
  .source_diversion_end = 2040,
  .diverted_to_landfill_pct = 0.1,
  .diverted_to_recycle_pct = 0.4,
  .diverted_to_organics_pct = 0.3,
  .diverted_to_wte_pct = 0.2,
  .diverted_to_onsite_pct = 0
)


test


rbind(
  test$emissions$inv,
  test$emissions$future
) %>%
  rename(emissions_year = inventory_year) %>%
  group_by(emissions_year) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  ggplot() +
  # Base fill (2005-2022, gray)
  geom_ribbon(aes(x = emissions_year, ymin = 0, ymax = value_emissions),
    fill = "gray80", alpha = 0.7
  )


## Next we need to build out the actual data for the 11-county region
