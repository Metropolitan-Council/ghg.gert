#' @title Apply Natural Systems Module 3: Plant Community Trees
#'
#' @param df_null Input dataframe of land cover area estimates left unchanged from 2023 to 2050
#' @param start_yr Numeric start year for land conversion
#' @param end_yr Numeric end year for land conversion
#' @param area_pct Numeric percentage of developed area to convert to community tree cover (0 to 100)
#'
#'
#' @export
# Plant Community Trees -------------------------------------------
plant_community_trees <- function(df_null,
                                  start_yr,
                                  end_yr,
                                  area_pct) {
  # Total Developed area in 2022
  # Want to determine how much developed area is available for tree planting based on
  # the degree of imperviousness (low, medium and high) where low is 20-49% impervious,
  # medium is 50-79% impervious and high is 80-100% impervious.

  # Plantable fractions per developed type
  plantable_fraction <- c(
    Developed_Low = 0.30, # 30% plantable area, 70% impervious
    Developed_Med = 0.15, # 15% plantable area, 85% impervious
    Developed_High = 0.05 #  5% plantable area, 95% impervious
  )



  df_max <- df_null %>%
    # filter for the last year in the dataset
    filter(inventory_year == max(inventory_year)) %>%
    mutate(
      area_change = case_when(
        land_cover_type == "Developed_Low" ~ -1 * area * plantable_fraction["Developed_Low"] * (area_pct / 100),
        land_cover_type == "Developed_Med" ~ -1 * area * plantable_fraction["Developed_Med"] * (area_pct / 100),
        land_cover_type == "Developed_High" ~ -1 * area * plantable_fraction["Developed_High"] * (area_pct / 100),
        land_cover_type == "Urban_Tree" ~ sum(
          case_when(
            land_cover_type == "Developed_Low" ~ area * plantable_fraction["Developed_Low"] * (area_pct / 100),
            land_cover_type == "Developed_Med" ~ area * plantable_fraction["Developed_Med"] * (area_pct / 100),
            land_cover_type == "Developed_High" ~ area * plantable_fraction["Developed_High"] * (area_pct / 100),
            TRUE ~ 0
          )
        ),
        TRUE ~ 0
      )
    )


  has_urban_tree <- "Urban_Tree" %in% df_max$land_cover_type

  if (!has_urban_tree) {
    urban_tree_template <- df_max %>%
      filter(land_cover_type == "Developed_Low") %>%
      slice(1) %>%
      mutate(
        land_cover_type = "Urban_Tree",
        area = 0,
        area_change = sum(df_max$area_change[df_max$land_cover_type %in%
          c("Developed_Low", "Developed_Med", "Developed_High")]),
        potential_wetland_area = 0
      )

    df_max <- bind_rows(df_max, urban_tree_template)

    # Also add to df_null for all years
    urban_tree_rows <- df_null %>%
      filter(land_cover_type == "Developed_Low") %>%
      group_by(inventory_year) %>%
      slice(1) %>%
      dplyr::ungroup() %>%
      mutate(
        land_cover_type = "Urban_Tree",
        area = 0,
        potential_wetland_area = 0
      )

    df_null <- bind_rows(df_null, urban_tree_rows)
  }


  df_export <- simulate_land_conversion(
    df = df_null %>%
      left_join(
        df_max %>% dplyr::select(c(land_cover_type, area_change)),
        by = join_by(land_cover_type)
      ),
    start_yr = start_yr,
    end_yr = end_yr
  ) %>%
    dplyr::select(colnames(df_null))


  # df_export %>%
  #   ggplot() +
  #     geom_line(alpha = 0.9, linewidth=0.5,
  #               aes(x = inventory_year, y = area,
  #                   color = land_cover_type),show.legend = F) +
  #     theme(
  #       legend.position = "bottom",
  #       legend.direction = "horizontal"
  #     ) +
  #     facet_wrap(~land_cover_type, scales="free_y")


  return(df_export)
}
