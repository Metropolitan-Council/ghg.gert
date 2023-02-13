carbon_stocks_and_sequestration <- rbind(
    data.frame(
        land_cover_type = "Impervious",
        sequest_mg_c_per_hectare_per_year = 0,
        stock_mg_c_per_hectare = 33),
    data.frame(
        land_cover_type = "Grass",
        sequest_mg_c_per_hectare_per_year = -0.42,
        stock_mg_c_per_hectare = 77),
    data.frame(
        land_cover_type = "Trees",
        sequest_mg_c_per_hectare_per_year = -1.3,
        stock_mg_c_per_hectare = 115),
    data.frame(
        land_cover_type = "Water",
        sequest_mg_c_per_hectare_per_year = 0,
        stock_mg_c_per_hectare = 0),
    data.frame(
        land_cover_type = "Barren",
        sequest_mg_c_per_hectare_per_year = -0.014,
        stock_mg_c_per_hectare = 0),
    data.frame(
        land_cover_type = "Forest",
        sequest_mg_c_per_hectare_per_year = -0.62,
        stock_mg_c_per_hectare = 115),
    data.frame(
        land_cover_type = "Shrub",
        sequest_mg_c_per_hectare_per_year = -0.29,
        stock_mg_c_per_hectare = 77
    ),
    data.frame(
        land_cover_type = "Grassland",
        sequest_mg_c_per_hectare_per_year = -0.42,
        stock_mg_c_per_hectare = 77
    ),
    data.frame(
        land_cover_type = "Agriculture",
        sequest_mg_c_per_hectare_per_year = -0.19,
        stock_mg_c_per_hectare = 41
    ),
    data.frame(
        land_cover_type = "Woody Wetland",
        sequest_mg_c_per_hectare_per_year = -0.62,
        stock_mg_c_per_hectare = 117
    ),
    data.frame(
        land_cover_type = "Wetland",
        sequest_mg_c_per_hectare_per_year = -1.5,
        stock_mg_c_per_hectare = 297
    ),
    data.frame(
        land_cover_type = "Parking Lot",
        sequest_mg_c_per_hectare_per_year = 0,
        stock_mg_c_per_hectare = 33
    )
)