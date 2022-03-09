#' Title
#'
#' @param res_tb
#' @param .new_homes_to_multifamily_pct numeric, percentage of new single-family homes
#'     to instead be built as multifamily homes
#'
#' @return
#' @export
#'
#' @examples
adj_unit_counts <- function(res_tb,
                            .new_homes_to_multifamily_pct){
  n_new_homes<-
    res_tb %>%
    filter(var %in% c(
      "multifamily_units",
      "single_family_units"
      # "single_family_average_floor_area_sqft_ctu"
      # "multifamily_average_floor_area_sqft_county"
    )) %>%
    group_by(ctu_name, var) %>%
    pivot_wider(names_from = year, values_from = value) %>%
    mutate(new_homes = `2040` - `2018`)
           # new_homes = ifelse(new_sf_homes < 0, 0, new_sf_homes)) %>%
    # mutate(new_sf_homes *  0.5) %>%


    sf_now_mf <- n_new_homes %>%
      filter(var == "single_family_units") %>%
      mutate(new_homes = ifelse(new_homes < 0, 0, new_homes),
             now_mf = new_homes * 0.5) %>%
      ungroup() %>%
      select(ctu_name, now_mf) %>%
      unique()


    new_units <-  res_tb %>%
       filter(var %in% c(
         "multifamily_units",
         "single_family_units"
         # "single_family_average_floor_area_sqft_ctu"
         # "multifamily_average_floor_area_sqft_county"
       ),
       year == 2040) %>%
       left_join(sf_now_mf) %>%
       mutate(value = ifelse(var == "multifamily_units", value + now_mf,
                             value - now_mf)) %>%
      select(names(res_tb))


     new_res_tb <- res_tb %>%
       anti_join(new_units, by = c("ctu_name", "year", "var")) %>%
       bind_rows(new_units)


    return(new_res_tb)

  }
