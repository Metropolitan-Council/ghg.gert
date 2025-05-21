#' @title Create GHG Inventory graphs
#'
#' @param name parameter name
#' @param value parameter value
#'
#' @return Error if values do not pass
#' @export
#'
#' @examples
#'

plot_inventory <- function(ghg_inv = ghg_inventory,
                         sector_colors = sector_colors,
                         sector_level = "category",
                         inventory_year = 2022,
                         .selected_ctu = "Eagan") {
  if (!sector_level %in% c("sector", "category", "source")) {
      cli::cli_abort("Enter a valid sector level: 'sector', 'category' or 'source'.")
  }

  if (!inventory_year %in% unique(ghg_inv$emissions_year)) {
    cli::cli_abort("Enter a valid inventory year between 2005 and 2022")
  }

  ghg_inv_year <- filter_ctu(ghg_inv_year, .selected_ctu = .selected_ctu) %>%
    dplyr::filter(ghg_inv,
                  emissions_year == inventory_year)

  if(sector_level == "sector") {
    color_list = sector_colors
  } else {
    grouping_col <- sym(sector_level)

    color_list <- ghg_inv_year %>%
      dplyr::distinct(!!grouping_col, sector) %>%
      dplyr::arrange(sector, !!grouping_col) %>%
      group_by(sector) %>%
      mutate(n_within_sector = dplyr::n(),
             shade_index = dplyr::row_number(),
             lighten_factor = shade_index / (n_within_sector + 1)) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(color = colorspace::lighten(sector_colors[[sector]], lighten_factor)) %>%
      ungroup() %>%
      select(name = !!grouping_col, color) %>%
      tibble::deframe()  # creates named character vector
}

  ghg_plot <- ggplot2::ggplot(
    data = ghg_inv_year,
    ggplot2::aes(x = sector,
        y = value_emissions,
        fill = sector_level)
  ) +
    ggplot2::geom_bar(stat = "identity") +
    scale_fill_manual(values = color_palette_vector_sector, guide = FALSE) +


}
