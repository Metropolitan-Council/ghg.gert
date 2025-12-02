#' @title Apply Natural Systems Module 2: Forest Restoration
#'
#' @param df_null Input dataframe of land cover area estimates left unchanged from 2023 to 2050
#' @param start_yr Numeric start year for land conversion
#' @param end_yr Numeric end year for land conversion
#' @param grass_pct Numeric percentage of grassland area to convert to forest cover (0 to 100)
#' @param bare_pct Numeric percentage of bare area to convert to forest cover (0 to 100)
#' @param crop_pct Numeric percentage of cropland area to convert to forest cover (0 to 100)
#'
#'
#' @export
# Forest Restoration -------------------------------------------
restore_forests <- function(df_null,
                             start_yr,
                             end_yr,
                             grass_pct,
                             bare_pct,
                             crop_pct
                             ) {


  df_max <- df_null %>%
    # filter for the last year in the dataset
    filter(inventory_year == max(inventory_year)) %>%
    mutate(
      area_change = case_when(
        land_cover_type == "Grassland" ~ -1*area * (grass_pct / 100),
        land_cover_type == "Bare" ~ -1*area * (bare_pct / 100),
        land_cover_type == "Cropland" ~ -1*area * (crop_pct / 100),
        land_cover_type == "Tree" ~ sum(
          case_when(
            land_cover_type == "Grassland" ~ area * (grass_pct / 100),
            land_cover_type == "Bare" ~ area * (bare_pct / 100),
            land_cover_type == "Cropland" ~ area * (crop_pct / 100),
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
