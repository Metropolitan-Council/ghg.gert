# explore_dnr_community_trees.R
# Step 1: Compare DNR 2023 street tree counts to NLCD 2022 Urban_Tree area

library(tidyverse)
library(readxl)

# Load data

dnr_metro <- read_excel("./data-raw/nature_data_processing/CommunityChangeTable.xlsx") %>%
  rename(
    community = `Community *`,
    county = County,
    trees_2013 = `2013 Street Tree Populations`,
    trees_2023 = `2023 Street Tree Population`,
    tree_change = `Street Tree Population Change`,
    pct_forest_2013 = `2013 Percent Forest`,
    pct_forest_2023 = `2023 Percent Forest`
  ) %>%
  mutate(
    across(c(trees_2013, trees_2023, tree_change), ~ as.numeric(.x)),
    community = str_trim(community)
  ) %>%
  filter(county %in% c(
    "Anoka", "Carver", "Dakota", "Hennepin",
    "Ramsey", "Scott", "Washington"
  ))


# Build NLCD summary per CTU (2022 only)

plantable_fraction <- c(
  Developed_Low  = 0.30,
  Developed_Med  = 0.15,
  Developed_High = 0.05
)


nlcd_ctu_long <- bind_rows(ghg.gert::natural_systems_data$inventory) %>%
  filter(
    ctu_class == "CITY" | ctu_class == "TOWNSHIP",
    inventory_year %in% c(2013, 2022)
  ) %>%
  select(geog_name, ctu_class, geog_id, inventory_year, land_cover_type, area) %>%
  pivot_wider(
    id_cols = c(geog_name, ctu_class, geog_id, inventory_year),
    names_from = land_cover_type,
    values_from = area,
    values_fill = 0
  ) %>%
  mutate(
    total_plantable_sqkm = Developed_Low * plantable_fraction["Developed_Low"] +
      Developed_Med * plantable_fraction["Developed_Med"] +
      Developed_High * plantable_fraction["Developed_High"],
    urban_tree_sqkm = if ("Urban_Tree" %in% names(.)) Urban_Tree else 0
  )

# 2022 snapshot for level comparisons
nlcd_ctu <- nlcd_ctu_long %>% filter(inventory_year == 2022)


# Join datasets

joined <- dnr_metro %>%
  left_join(nlcd_ctu, by = c("community" = "geog_name"))

matched <- joined %>% filter(!is.na(geog_id), !is.na(trees_2023), urban_tree_sqkm > 0)
unmatched <- joined %>% filter(is.na(geog_id))

cat("Matched:", nrow(matched), "\n")
cat("Unmatched:", nrow(unmatched), "\n")

if (nrow(unmatched) > 0) {
  nlcd_names <- unique(nlcd_ctu$geog_name)
  unmatched %>%
    rowwise() %>%
    mutate(closest_nlcd = nlcd_names[which.min(
      stringdist::stringdist(community, nlcd_names, method = "jw")
    )]) %>%
    ungroup() %>%
    select(community, county, closest_nlcd) %>%
    print(n = 50)
}


# Compare: DNR tree count vs NLCD Urban_Tree area

matched %>%
  mutate(sqm_canopy_per_tree = (urban_tree_sqkm * 1e6) / trees_2023) %>%
  select(
    community, county, trees_2023, urban_tree_sqkm, total_plantable_sqkm,
    sqm_canopy_per_tree
  ) %>%
  arrange(desc(trees_2023)) %>%
  print(n = 30)


plot_df <- matched %>%
  mutate(
    .resid = residuals(lm(trees_2023 ~ urban_tree_sqkm, data = .)),
    .abs_resid = abs(.resid),
    .label = if_else(.abs_resid >= sort(.abs_resid, decreasing = TRUE)[10],
      community, NA_character_
    )
  )

plot_df %>%
  ggplot(aes(x = urban_tree_sqkm, y = trees_2023)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  geom_text(
    aes(label = .label),
    hjust = -0.1, size = 2.5, check_overlap = TRUE, na.rm = TRUE
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "DNR Street Tree Count (2023) vs NLCD Urban_Tree Area (2022)",
    x = "NLCD Urban_Tree area (sq km)",
    y = "DNR street tree population"
  ) +
  theme_minimal()

# look at this by county


matched %>%
  ggplot(aes(x = urban_tree_sqkm, y = trees_2023, color = county)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "DNR Street Trees vs NLCD Urban_Tree Area by County",
    x = "NLCD Urban_Tree area (sq km)",
    y = "DNR street tree population",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# by community designation

ctu_meta <- readRDS("./data-raw/meta/ccap_ctu.rds") %>%
  sf::st_drop_geometry() %>%
  select(ctu_id_gnis, thrive_designation)

matched_meta <- matched %>%
  left_join(ctu_meta, by = c("geog_id" = "ctu_id_gnis"))

matched_meta %>%
  ggplot(aes(x = urban_tree_sqkm, y = trees_2023)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "DNR Street Trees vs NLCD Urban_Tree Area by Thrive Designation",
    x = "NLCD Urban_Tree area (sq km)",
    y = "DNR street tree population",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(. ~ thrive_designation)

# linear model

mod_base <- lm(trees_2023 ~ urban_tree_sqkm, data = matched_meta)
mod_thrive <- lm(trees_2023 ~ urban_tree_sqkm * thrive_designation, data = matched_meta)

cat("\n── Base model (no designation) ──\n")
summary(mod_base)

cat("\n── With thrive designation (interaction) ──\n")
summary(mod_thrive)

cat("\n── Model comparison ──\n")
anova(mod_base, mod_thrive)


# Residual outliers

plot_df <- matched %>%
  mutate(
    .resid = residuals(lm(trees_2023 ~ urban_tree_sqkm, data = .)),
    .abs_resid = abs(.resid),
    .label = if_else(.abs_resid >= sort(.abs_resid, decreasing = TRUE)[10],
      community, NA_character_
    )
  )

plot_df %>%
  ggplot(aes(x = urban_tree_sqkm, y = trees_2023)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  geom_text(
    aes(label = .label),
    hjust = -0.1, size = 2.5, check_overlap = TRUE, na.rm = TRUE
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "DNR Street Tree Count (2023) vs NLCD Urban_Tree Area (2022)",
    x = "NLCD Urban_Tree area (sq km)",
    y = "DNR street tree population"
  ) +
  theme_minimal()

### Change over time analysis ####


nlcd_2013 <- nlcd_ctu_long %>%
  filter(inventory_year == 2013) %>%
  select(geog_id, urban_tree_sqkm_2013 = urban_tree_sqkm)

change <- matched %>%
  filter(!is.na(trees_2013)) %>%
  left_join(nlcd_2013, by = "geog_id") %>%
  mutate(
    dnr_change      = trees_2023 - trees_2013,
    dnr_change_pct  = dnr_change / trees_2013 * 100,
    nlcd_change     = urban_tree_sqkm - urban_tree_sqkm_2013,
    nlcd_change_pct = nlcd_change / urban_tree_sqkm_2013 * 100
  )

cat("\n── Change comparison (n =", nrow(change), "communities with both DNR years) ──\n")
cat(
  "\nCorrelation (% change): r =",
  round(cor(change$nlcd_change_pct, change$dnr_change_pct, use = "complete.obs"), 3), "\n"
)
cat(
  "Correlation (absolute change): r =",
  round(cor(change$nlcd_change, change$dnr_change, use = "complete.obs"), 3), "\n"
)

cat("\nDNR net tree change: ", scales::comma(sum(change$dnr_change)), "trees\n")
cat("NLCD net area change:", round(sum(change$nlcd_change), 2), "sq km\n")

cat("\nDirection agreement:\n")
print(table(
  NLCD = ifelse(change$nlcd_change > 0.01, "gain", ifelse(change$nlcd_change < -0.01, "loss", "flat")),
  DNR  = ifelse(change$dnr_change > 0, "gain", ifelse(change$dnr_change < 0, "loss", "flat"))
))

change %>%
  ggplot(aes(x = nlcd_change_pct, y = dnr_change_pct)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(alpha = 0.6) +
  geom_text(
    data = . %>% filter(abs(dnr_change_pct) > 40 | abs(nlcd_change_pct) > 30),
    aes(label = community), hjust = -0.1, size = 2.5, check_overlap = TRUE
  ) +
  labs(
    title = "Change Comparison: NLCD Urban_Tree Area vs DNR Street Tree Count",
    subtitle = "NLCD 2013→2022 | DNR 2013→2023",
    x = "NLCD Urban_Tree area change (%)",
    y = "DNR street tree count change (%)"
  ) +
  theme_minimal()
