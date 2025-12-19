#' @title Apply Natural Systems Module 1: Wetland Restoration
#'
#' @param df_null Input dataframe of land cover area estimates left unchanged from 2023 to 2050
#' @param start_yr Numeric start year for land conversion
#' @param end_yr Numeric end year for land conversion
#' @param tree_pct Numeric percentage of forested area to convert to wetland cover (0 to 100)
#' @param grass_pct Numeric percentage of grassland area to convert to wetland cover (0 to 100)
#' @param bare_pct Numeric percentage of bare area to convert to wetland cover (0 to 100)
#' @param crop_pct Numeric percentage of cropland area to convert to wetland cover (0 to 100)
#'
#'
#' @export
# Wetland Restoration -------------------------------------------
restore_wetlands <- function(df_null,
                             start_yr,
                             end_yr,
                             tree_pct,
                             grass_pct,
                             bare_pct,
                             crop_pct
                             ) {


  # construct a dataframe that calculates total land cover area change by the end
  # of the dataset using the provided percentages and potential wetland area
  df_max <- df_null %>%
    # filter for the last year in the dataset
    filter(inventory_year == max(inventory_year)) %>%
    mutate(
      actual_wetland_area = case_when(
        potential_wetland_area > area ~ area,
        TRUE ~ potential_wetland_area
      ),
      area_change = case_when(
        land_cover_type == "Tree" ~ -1*actual_wetland_area * (tree_pct / 100),
        land_cover_type == "Grassland" ~ -1*actual_wetland_area * (grass_pct / 100),
        land_cover_type == "Bare" ~ -1*actual_wetland_area * (bare_pct / 100),
        land_cover_type == "Cropland" ~ -1*actual_wetland_area * (crop_pct / 100),
        land_cover_type == "Wetland" ~ sum(
          case_when(
            land_cover_type == "Tree" ~ actual_wetland_area * (tree_pct / 100),
            land_cover_type == "Grassland" ~ actual_wetland_area * (grass_pct / 100),
            land_cover_type == "Bare" ~ actual_wetland_area * (bare_pct / 100),
            land_cover_type == "Cropland" ~ actual_wetland_area * (crop_pct / 100),
            TRUE ~ 0
          )
        ),
        TRUE ~ 0
      )

    )


  # browser()

  df_export <- simulate_land_conversion(
    df = df_null %>%
      left_join(
        df_max %>% dplyr::select(c(land_cover_type,area_change)),
        by = join_by(land_cover_type)
      ),
    start_yr = start_yr,
    end_yr   = end_yr
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
