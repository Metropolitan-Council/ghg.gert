ghg_inv <- ghg_inventory

# sector colors
sector_colors <- list(
  "Electricity" = "#9467bd",
  "Building fuel" = "#d62728",
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
  ggplot2::ggplot(ggplot2::aes(x = category, y = 1, fill = category)) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(ggplot2::aes(label = category), color = "black", size = 3.5, hjust = 0.5) +
  ggplot2::scale_fill_manual(values = category_colors) +
  ggplot2::coord_flip() +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "none")

category_alt_colors <- ghg_inv %>%
  dplyr::distinct(category, sector_alt) %>%
  dplyr::arrange(sector_alt, category) %>%
  group_by(sector_alt) %>%
  mutate(n_within_sector = dplyr::n(),
         shade_index = dplyr::row_number(),
         lighten_factor = shade_index / (n_within_sector + 1)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(color = colorspace::lighten(sector_colors[[sector_alt]], lighten_factor)) %>%
  ungroup() %>%
  select(name = category, color) %>%
  tibble::deframe()

#visualize
tibble(category = names(category_alt_colors),
       color = category_alt_colors) %>%
  mutate(category = factor(category, levels = category)) %>%
  ggplot2::ggplot(ggplot2::aes(x = category, y = 1, fill = category)) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(ggplot2::aes(label = category), color = "black", size = 3.5, hjust = 0.5) +
  ggplot2::scale_fill_manual(values = category_alt_colors) +
  ggplot2::coord_flip() +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "none")

inventory_colors <- list(
  sector_colors = sector_colors,
  category_colors = category_colors,
  category_alt_colors = category_alt_colors
)

usethis::use_data(inventory_colors, overwrite = TRUE)
