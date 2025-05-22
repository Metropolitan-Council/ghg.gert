#' @title Create GHG Inventory graphs
#'
#' @return Error if values do not pass
#' @export
#'
#' @examples
#'
plot_inventory <- function(ghg_inv = ghg_inventory,
                         inventory_colors = inventory_colors,
                         sector_split = "Energy",
                         sector_level = "category",
                         inventory_year = 2022,
                         .selected_ctu = "Eagan") {
  if (!sector_level %in% c("sector", "category")) {
      cli::cli_abort("Enter a valid sector level: 'sector' or 'category'.")
  }

  if (!sector_split %in% c("Energy", "Building type")) {
    cli::cli_abort("Enter a valid sector split: 'Energy' or 'Building type'.")
  }

  if (!inventory_year %in% unique(ghg_inv$emissions_year)) {
    cli::cli_abort("Enter a valid inventory year between 2005 and 2022")
  }

  ghg_inv_year <- filter_ctu(ghg_inv, .selected_ctu = .selected_ctu) %>%
    dplyr::filter(emissions_year == inventory_year) %>%
    dplyr::mutate(sector_use = if (sector_split == "Building type") {
      sector
    } else {
      sector_alt
    })

color_use <- if(sector_level == "sector") {
  inventory_colors$sector_colors
} else if (sector_level == "category" & sector_split == "Energy") {
  inventory_colors$category_alt_colors
} else {
  inventory_colors$category_colors
}

  ghg_plot <- ggplot2::ggplot(
    data = ghg_inv_year,
    ggplot2::aes(x = sector_use,
        y = value_emissions / 1000,
        fill = .data[[sector_level]])
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = color_use, guide = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 25, vjust = 0.5, hjust = 0.45, size = 14),
      axis.title.y = ggplot2::element_text(size = 16)
    ) +
    ggplot2::ylab("Thousands of metric tons of CO2e") +
    ggplot2::xlab("") +
    ggplot2::labs(fill = "Subsector")

  ghg_plot

}
