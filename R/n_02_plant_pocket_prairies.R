#' @title Apply Natural Systems Module 4: Plant Pocket Prairies
#'
#' @param df_null Input dataframe of land cover area estimates left unchanged from 2023 to 2050
#' @param start_yr Numeric start year for land conversion
#' @param end_yr Numeric end year for land conversion
#' @param area_pct Numeric percentage of developed area to convert to community tree cover (0 to 100)
#'
#'
#' @export
# Plant Pocket Prairies -------------------------------------------
plant_pocket_prairies <- function(df_null,
                                  start_yr,
                                  end_yr,
                                  area_pct
) {



  df_max <- df_null %>%
    # filter for the last year in the dataset
    filter(inventory_year == max(inventory_year)) %>%
    mutate(
      area_removed = case_when(
        land_cover_type == "Urban_Grassland" ~ -1*area * (area_pct / 100),
        TRUE ~ 0
      ),
      area_change = case_when(
        land_cover_type == "Urban_Grassland" ~ sum(area_removed),
        land_cover_type == "Grassland" ~ -1*sum(area_removed),
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
