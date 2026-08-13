transportation_defaults <-
  list(
    "transit_avo_pct" = 0,
    "pldv_avo_pct" = 0,
    "transit_service_pct" = 0,
    "vmt_fee" = 0,
    "payd_fee" = 0,
    "gas_tax" = 0,
    "parking_price" = 0,
    "freight_parking_price" = 0,
    "cong_price" = 0,
    "freight_vmt_fee" = 0,
    "pop_dens_pct_change" = 0,
    "emp_dens_pct_change" = 0,
    "land_use_diversity_pct_change" = 0,
    "intersection_design_pct_change" = 0,
    "intersection_density_pct_change" = 0,
    "job_access_pct_change" = 0,
    "transit_dist_pct_change" = 0,
    "comb_5d_impact_pct_change" = 0,
    "telework_pct" = 0,
    "vmt_reduction_pct" = 0,
    "bev_pct_sales" = 0,
    "hev_pct_sales" = 0,
    "bev_pct_stock" = 0,
    "hev_pct_stock" = 0,
    "cbtp_prop_targeted" = 0,
    "cbtp_start_year" = "2030",
    "commute_trip_reduction_voluntary" = TRUE,
    "commute_trip_reduction_employees_targeted" = 0,
    "commute_trip_reduction_start_year" = "2030"
  )

usethis::use_data(transportation_defaults, overwrite = TRUE)
