# test-land_use_data

testthat::expect_true(
  nrow(
    land_use_data$land_cover_percentages_filled %>%
      dplyr::group_by(ctu_name, description_2) %>%
      dplyr::summarise(sum = sum(land_cover_percent, na.rm = T), .groups = "keep") %>%
      dplyr::filter(!round(sum, 3) %in% c(0, 1))
  ) %in% c(0, 100),
  label = "Looks like there are probably land cover issues"
)
