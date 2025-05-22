ghg_inv <- ghg_inventory

# sector colors
sector_colors <- list(
  "Electricity" = "#1f77b4",
  "Transportation" = "#191970",
  "Residential" = "#9467bd",
  "Commercial" = "#d62728",
  "Business" = "#d62728",
  "Industrial" = "slategray",
  "Waste" = "#8c564b",
  "Agriculture" = "#8fb910",
  "Natural Systems" = "#006f3c"
)


## lighten sector colors to create category colors
category_colors <- ghg_inv %>%
    dplyr::distinct(category, sector) %>%
    dplyr::arrange(sector, category) %>%
    group_by(sector) %>%
    mutate(n_within_sector = dplyr::n(),
           shade_index = dplyr::row_number(),
           lighten_factor = shade_index / (n_within_sector + 1)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(color = colorspace::lighten(sector_colors[[sector]], lighten_factor)) %>%
    ungroup() %>%
    select(name = category, color) %>%
    tibble::deframe()

#visualize
tibble(category = names(category_colors),
       color = category_colors) %>%
  mutate(category = factor(category, levels = category)) %>%
  ggplot2::ggplot(aes(x = category, y = 1, fill = category)) +
  geom_tile() +
  scale_fill_manual(values = category_colors) +
  coord_flip() +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Category Colors")


usethis::use_data(sector_colors, overwrite = TRUE)
