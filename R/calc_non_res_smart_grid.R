calc_non_res_smart_grid <- function(.industries_in_smart_grid_ptc){
  non_res_tb %>%
    dplyr::filter(var %in% c("industrial_jobs", "commercial_jobs")) %>%
    tidyr::pivot_wider(
      names_from = c(var, year),
      values_from = value,
      names_sep = "."
    )
}
