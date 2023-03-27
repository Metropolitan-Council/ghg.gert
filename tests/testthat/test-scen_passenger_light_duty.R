pass <- suppressMessages(scen_passenger_light_duty(
  .pass_tb = st_paul_passenger,
  .calc_transp_cost = TRUE,
  .calc_transp_fuel_use = TRUE,
  .calc_transp_ghg_embodied = TRUE
))


testthat::expect_length(pass, 5)

testthat::expect_named(pass,
  expected = c(
    "vmt",
    "dir_ghg",
    "emb_ghg",
    "fuel_use",
    "cost"
  ),
  ignore.order = TRUE
)
