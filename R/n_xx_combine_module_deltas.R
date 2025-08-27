#' @title Combine land cover change results
#'
#' @param mod1_output Input dataset, results from module 1
#' @param mod2_output Input dataset, results from module 2
#' @param mod3_output Input dataset, results from module 3
#'
#' @return [tibble::tibble()].
#'      A tibble containing the land cover change values in square kilometers,
#'      after collecting the results of 3 modules.
#' @export
# Combine all module delta values -----------------------------------------------------
combine_module_deltas <- function(mod1_output,
                                  mod2_output,
                                  mod3_output) {
  # Add namespace to delta columns (except for 'inventory_year')
  mod1_renamed <- mod1_output %>%
    rename_with(~ paste0("mod1_", .), -inventory_year)

  mod2_renamed <- mod2_output %>%
    rename_with(~ paste0("mod2_", .), -inventory_year)

  mod3_renamed <- mod3_output %>%
    rename_with(~ paste0("mod3_", .), -inventory_year)

  # Join all module outputs by inventory_year
  delta_all <- mod1_renamed %>%
    full_join(mod2_renamed, by = "inventory_year") %>%
    full_join(mod3_renamed, by = "inventory_year")

  # Sum deltas across modules for each land cover class
  delta_combined <- delta_all %>%
    as_tibble() %>%
    mutate(
      delta_Grassland = rowSums(across(contains("delta_Grassland")), na.rm = TRUE),
      delta_Tree = rowSums(across(contains("delta_Tree")), na.rm = TRUE),
      delta_Urban_Tree = rowSums(across(contains("delta_Urban_Tree")), na.rm = TRUE),
      delta_Cropland = rowSums(across(contains("delta_Cropland")), na.rm = TRUE),
      delta_Wetland = rowSums(across(contains("delta_Wetland")), na.rm = TRUE),
      delta_Urban_Grassland = rowSums(across(contains("delta_Urban_Grassland")), na.rm = TRUE),
      delta_Developed_Low = rowSums(across(contains("delta_Developed_Low")), na.rm = TRUE),
      delta_Developed_Med = rowSums(across(contains("delta_Developed_Med")), na.rm = TRUE),
      delta_Developed_High = rowSums(across(contains("delta_Developed_High")), na.rm = TRUE)
    ) %>%
    select(inventory_year, starts_with("delta_"))

  return(delta_combined)
}
