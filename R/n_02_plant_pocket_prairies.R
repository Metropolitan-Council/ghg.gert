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
                                  area_pct) {
  df_max <- df_null %>%
    # filter for the last year in the dataset
    filter(inventory_year == max(inventory_year)) %>%
    mutate(
      area_removed = case_when(
        land_cover_type == "Urban_Grassland" ~ -1 * area * (area_pct / 100),
        TRUE ~ 0
      ),
      area_change = case_when(
        land_cover_type == "Urban_Grassland" ~ sum(area_removed),
        land_cover_type == "Grassland" ~ -1 * sum(area_removed),
        TRUE ~ 0
      )
    )

  # FIX: Check if Grassland exists, if not, create it
  has_grassland <- "Grassland" %in% df_max$land_cover_type

  if (!has_grassland) {
    # Calculate the area being converted from Urban_Grassland
    converted_area <- df_max %>%
      filter(land_cover_type == "Urban_Grassland") %>%
      pull(area) %>%
      sum() * (area_pct / 100)

    # Create Grassland row based on Urban_Grassland template
    grassland_template <- df_max %>%
      filter(land_cover_type == "Urban_Grassland") %>%
      slice(1) %>%
      mutate(
        land_cover_type = "Grassland",
        area = 0, # Will be set by area_change
        area_change = converted_area,
        area_removed = 0,
        potential_wetland_area = 0
      )

    # Add the new Grassland row to df_max
    df_max <- bind_rows(df_max, grassland_template)
  }

  # Also need to add Grassland to ALL years in df_null if it doesn't exist
  if (!has_grassland) {
    # Get unique years
    all_years <- unique(df_null$inventory_year)

    # Create Grassland rows for all years (starting with 0 area)
    grassland_rows <- df_null %>%
      filter(land_cover_type == "Urban_Grassland") %>%
      group_by(inventory_year) %>%
      slice(1) %>%
      dplyr::ungroup() %>%
      mutate(
        land_cover_type = "Grassland",
        area = 0,
        potential_wetland_area = 0
      )

    # Add to df_null
    df_null <- bind_rows(df_null, grassland_rows)
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

  return(df_export)
}
