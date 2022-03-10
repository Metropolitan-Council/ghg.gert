#' Adjust single and multifamily average floor area forecast in residential table
#'
#' @param res_tb
#' @param .single_family_floor_area_growth_pct numeric, growth rate
#'     in single family home floor area.
#' @param .new_homes_affected_pct numeric, percentage of all new single-family
#'     households that will respond to increased energy costs by decreasing
#'     home size. Default is `0.5` (half of all single family homes).
#'
#' @family building
#'
#' @details
#'    Uses the average single family floor area in 2018
#'
#'
#'
#' @return
#' @export
#'
#' @examples
adj_avg_floor_area <- function(res_tb,
                               .single_family_floor_area_growth_pct,
                               .new_homes_affected_pct = 0.5){
  browser()
  if(.single_family_floor_area_growth_pct == 0){
    warning("No change in single family floor area growth. Returning original table")
    return(res_tb)
  }

  res_tb_units <- res_tb %>%
    filter(var == "single_family_units") %>%
    dplyr::group_by(ctu_name, var) %>%
    tidyr::pivot_wider(names_from = year, values_from = value) %>%
    mutate(diff_units = `2040` - `2018`,
           new_units = ifelse(diff_units < 0, 0, diff_units),
           new_units_affected = new_units * .new_homes_affected_pct,
           prop_of_all_new = new_units/`2040`) %>%
    ungroup() %>%
    select(ctu_name, prop_of_all_new)

  # in the new units ONLY, 50% will have the adjusted floor area mean
  # otherwise, they will have the BAU floor area mean
  # the existing units will keep the BAU floor area
  # so the average floor area for ALL single family units in the forecast year
  # (newly built and built before 2018)
  # will be weighted by
  # proportion of all single family homes  = existing
  # proportion of NEW single family homes  = smaller
  # proportion of NEW single family homes  = same
  # 2000 at (1 - .12)
  # 2300 at (0.12 * 0.5)
  # 2500 at (0.12 * 0.5)
  weighted.mean(c(2000,
                  2100,
                  2500),
                c(1 - prop_of_all_new,
                  prop_of_all_new * .new_homes_affected_pct,
                  prop_of_all_new * (1 - .new_homes_affected_pct)))

  new_avg_floor_area <- res_tb %>%
    filter(var %in% c("single_family_average_floor_area_sqft_ctu")) %>%
    dplyr::group_by(ctu_name, var) %>%
    tidyr::pivot_wider(names_from = year, values_from = value) %>%
    mutate(new_forecast = (.single_family_floor_area_growth_pct * `2018`) + `2018`) %>%
    left_join(res_tb_units) %>%
    mutate(new_weighted_mean_forecast =
             weighted.mean(c(`2018`,
                             new_forecast,
                             `2040`),
                           c(1 - prop_of_all_new,
                             prop_of_all_new * .new_homes_affected_pct,
                             prop_of_all_new * (1 - .new_homes_affected_pct)))
    )


  new_res_avg_floor_area <- res_tb %>%
    filter(var %in% c("single_family_average_floor_area_sqft_ctu"),
           year == 2040) %>%
    left_join(new_avg_floor_area, by = c("ctu_name", "var")) %>%
    mutate(value = new_weighted_mean_forecast) %>%
    select(names(res_tb))

  new_res_tb <- res_tb %>%
    anti_join(new_res_avg_floor_area, by = c("ctu_name", "year", "var")) %>%
    bind_rows(new_res_avg_floor_area)


  return(new_res_tb)
}
