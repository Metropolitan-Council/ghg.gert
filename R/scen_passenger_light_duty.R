#' Calculate scenario for passenger light-duty vehicles
#'
#' @inheritParams scenario_results
#' @inheritParams calc_vmt_forecast
#'
#' @family transportation results, passenger
#'
#' @return
#' @export
#'
#' @importFrom emo ji
#'
scen_passenger_light_duty <- function(.scenario = "BAU",
                                      .electric_scenario = "ER",
                                      .aeo_scenario = "REF",
                                      .ctu = "",
                                      .transit_avo = 0,
                                      .transit_rider_pct = 0,
                                      .vmt_fee = 0,
                                      .payd_fee = 0,
                                      .gas_tax = 0,
                                      .parking_price = 0,
                                      .cong_price = 0,
                                      .freight_vmt_fee = 0,
                                      .drs_pct = 0,
                                      .av_pct = 0,
                                      .drs_fuel_type = "",
                                      .av_fuel_type = "",
                                      .pop_dens_pct_change = 0,
                                      .emp_dens_pct_change = 0,
                                      .land_use_pct_change = 0,
                                      .intersection_design_pct_change = 0,
                                      .job_access_pct_change = 0,
                                      .transit_dist_pct_change = 0,
                                      .comb_5d_impact_pct_change = 0,
                                      .telework_pct = 0,
                                      .mit_bau_summary = 0,
                                      .enviro_factors = enviro_factors) {
  # Sequence for each
  # 1. Establish `type`, `var`, `mode`
  # 2. Establish `stock`, `mpg`, `class`
  # 3. Calculate fuel cost per mile with `calc_fuel_cost_mile()`
  # 4. Calculate VMT with `calc`

  # browser()
  # Passenger ------------------------------------------------------------

  type <- "P"
  # For all passenger modes, variable = PMT
  var <- "PMT"
  # PLDV by fuel and CTU
  mode <- "PLDV"

  ## Gasoline -----
  # Calculate aggregate GHG in kt CO2 by year
  # 1. Calculate fuel cost per mile (FCM)
  # 2. Calculate VMT (requires FCM, )
  # 3. Calculate direct emissions (requires VMT)
  # 4. Calculate fuel use (requires VMT)
  # 5. Calculate embodied GHG

  stock <- "SIStock"
  mpg <- "SIMPG"
  class <- "SI"

  # browser()
  message("Passenger vehicles, gasoline")

  # Calculate a fuel cost per mile rather than per gallon
  si_fcm <- calc_fuel_cost_mile(
    transportation_data$passenger,
    mode,
    .aeo_scenario,
    .miles_per_gallon = "SIMPG",
    .enviro_factors$SI_FUEL_COST_GAL
  )

  si_vmt <- calc_vmt_forecast(
    .scenario = .scenario,
    tb = transportation_data$passenger,
    .mode = mode,
    .stock = "SIStock",
    .variable = var,
    .tb_fuel_cost_mile = si_fcm,
    .aeo_scenario = .aeo_scenario,
    .transit_avo = .transit_avo,
    .transit_rider_pct = .transit_rider_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .cong_price = .cong_price,
    .parking_price = .parking_price,
    .drs_pct = .drs_pct,
    .av_pct = .av_pct,
    .freight_vmt_fee = .freight_vmt_fee,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_pct_change = .land_use_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct
  ) %>%
    mutate(class = class)

  si_dir_ghg <- calc_ghg_direct(
    tb_vmt = si_vmt,
    tb = transportation_data$passenger,
    .mode = mode,
    .fuel_type = "SI",
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = mpg
  )

  si_fuel <- calc_fuel_use(
    tb_vmt = si_vmt,
    tb = transportation_data$passenger,
    .mode = mode,
    .fuel_type = "SI",
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = mpg
  )

  si_emb_ghg <-
    calc_ghg_embodied(
      transportation_data$passenger, mode,
      "SISales",
      "SI-EMB"
    )

  si_cost <-
    calc_cost(
      si_vmt, mode,
      "SIPrice"
    )


  # complete gasoline table


  ## Diesel----
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"

  message("Passenger vehicles, diesel")
  # Calculate a fuel cost per mile rather than per gallon
  ci_fcm <- calc_fuel_cost_mile(
    tb = transportation_data$passenger,
    .mode = mode,
    .aeo_scenario = .aeo_scenario,
    .miles_per_gallon = "CIMPG",
    .fuel_cost_gallon = .enviro_factors$CI_FUEL_COST_GAL
  )

  ci_vmt <- calc_vmt_forecast(
    .scenario = .scenario,
    tb = transportation_data$passenger,
    .mode = mode,
    .stock = "CIStock",
    .variable = var,
    .tb_fuel_cost_mile = ci_fcm,
    .aeo_scenario = .aeo_scenario,
    .transit_avo = .transit_avo,
    .transit_rider_pct = .transit_rider_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .cong_price = .cong_price,
    .parking_price = .parking_price,
    .drs_pct = .drs_pct,
    .av_pct = .av_pct,
    .freight_vmt_fee = .freight_vmt_fee,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_pct_change = .land_use_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct
  ) %>%
    mutate(class = class)



  ci_dir_ghg <-
    calc_ghg_direct(
      tb_vmt = ci_vmt,
      tb = transportation_data$passenger,
      .mode = mode,
      .fuel_type = "CI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "CIMPG"
    )


  ci_fuel <-
    calc_fuel_use(
      tb_vmt = ci_vmt,
      tb = transportation_data$passenger,
      .mode = mode,
      .fuel_type = "CI",
      .aeo_scenario = .aeo_scenario,
      .miles_per_gallon = "CIMPG"
    )


  ci_emb_ghg <-
    calc_ghg_embodied(
      tb = transportation_data$passenger,
      .mode = mode,
      .sales_mode = "CISales",
      .fuel_type = "CI-EMB",
      .class = "P"
    )


  ci_cost <-
    calc_cost(
      tb_vmt = ci_vmt,
      .mode = mode,
      .price = "CIPrice"
    )


  ## HEV (Hybrid electric vehicle)----
  stock <- "HEVStock"
  mpg <- "HEVMPG"
  class <- "HEV"

  message("Passenger vehicles, hybrid")

  # Calculate a fuel cost per mile rather than per gallon

  fcm <- calc_fuel_cost_mile(
    transportation_data$passenger, mode, .aeo_scenario,
    mpg, .enviro_factors$SI_FUEL_COST_GAL
  )

  hev_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      tb = transportation_data$passenger,
      .mode = mode,
      .stock = stock,
      .variable = var,
      .tb_fuel_cost_mile = fcm,
      .aeo_scenario = .aeo_scenario,
      .transit_avo = .transit_avo,
      .transit_rider_pct = .transit_rider_pct,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .cong_price = .cong_price,
      .parking_price = .parking_price,
      .drs_pct = .drs_pct,
      .av_pct = .av_pct,
      .freight_vmt_fee = .freight_vmt_fee,
      .pop_dens_pct_change = .pop_dens_pct_change,
      .emp_dens_pct_change = .emp_dens_pct_change,
      .land_use_pct_change = .land_use_pct_change,
      .intersection_design_pct_change = .intersection_design_pct_change,
      .job_access_pct_change = .job_access_pct_change,
      .transit_dist_pct_change = .transit_dist_pct_change,
      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
      .telework_pct = .telework_pct
    ) %>%
    mutate(class = class)

  hev_dir_ghg <-
    calc_ghg_direct(
      hev_vmt, transportation_data$passenger,
      mode, "SI", .aeo_scenario, mpg
    )

  hev_fuel <-
    calc_fuel_use(
      hev_vmt, transportation_data$passenger,
      mode, "SI", .aeo_scenario, mpg
    )

  hev_emb_ghg <-
    calc_ghg_embodied(
      transportation_data$passenger,
      mode, "HEVSales", "HEV-EMB"
    )

  hev_cost <-
    calc_cost(
      hev_vmt, mode,
      "HEVPrice"
    )

  ## PHEV (Plug-in hybrid) -----
  ### Eqn: (PMT in 1000 mi) x Pr(stock by fuel) x ((Pr(Elec) x elec consumption (MWh per 1000 mi) x GHG per elec) + ((1 - Pr(Elec)) x fuel consumption (per 1000 mi) x GHG per fuel)

  stock <- "PHEVStock"
  mpg <- "PHEVMPG"
  mpe <- "PHEVElec"
  class <- "PHEV"

  message("Passenger vehicles, plug-in hybrid")


  # Calculate a fuel cost per mile rather than per gallon
  # browser()

  fcm <- calc_fuel_cost_mile(
    transportation_data$passenger, mode, .aeo_scenario,
    mpg, .enviro_factors$SI_FUEL_COST_GAL
  )

  ### VMT gas ----
  phev_vmt_gas <- calc_vmt_forecast(
    .scenario = .scenario,
    tb = transportation_data$passenger,
    .mode = mode,
    .stock = stock,
    .variable = var,
    .tb_fuel_cost_mile = fcm,
    .aeo_scenario = .aeo_scenario,
    .transit_avo = .transit_avo,
    .transit_rider_pct = .transit_rider_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .cong_price = .cong_price,
    .parking_price = .parking_price,
    .drs_pct = .drs_pct,
    .av_pct = .av_pct,
    .freight_vmt_fee = .freight_vmt_fee,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_pct_change = .land_use_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct
  ) %>%
    mutate(class = class)

  # account for proportion of PHEV
  phev_vmt_gas <- left_join(phev_vmt_gas,
            transportation_data$passenger %>%
              dplyr::filter(mode == mode, var == "PHEVPr"),
            by =  c("mode", "ctu", "year", "aeo_mode", "type")) %>%
    mutate(vmt = vmt * (1-value)) %>%
    select(names(phev_vmt_gas))

  ### VMT electric ------
  fcm <- calc_fuel_cost_mile(
    transportation_data$passenger, mode, .aeo_scenario,
    mpe, .enviro_factors$ELEC_FUEL_COST_KWH
  )

  phev_vmt_electric <- calc_vmt_forecast(
    .scenario = .scenario,
    tb = transportation_data$passenger,
    .mode = mode,
    .stock = stock,
    .variable = var,
    .tb_fuel_cost_mile = fcm,
    .aeo_scenario = .aeo_scenario,
    .transit_avo = .transit_avo,
    .transit_rider_pct = .transit_rider_pct,
    .vmt_fee = .vmt_fee,
    .payd_fee = .payd_fee,
    .gas_tax = .gas_tax,
    .cong_price = .cong_price,
    .parking_price = .parking_price,
    .drs_pct = .drs_pct,
    .av_pct = .av_pct,
    .freight_vmt_fee = .freight_vmt_fee,
    .pop_dens_pct_change = .pop_dens_pct_change,
    .emp_dens_pct_change = .emp_dens_pct_change,
    .land_use_pct_change = .land_use_pct_change,
    .intersection_design_pct_change = .intersection_design_pct_change,
    .job_access_pct_change = .job_access_pct_change,
    .transit_dist_pct_change = .transit_dist_pct_change,
    .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
    .telework_pct = .telework_pct
  ) %>%
    mutate(class = class)


  # account for proportion of PHEV
  phev_vmt_electric <- left_join(phev_vmt_electric,
            transportation_data$passenger %>%
              dplyr::filter(mode == mode, var == "PHEVPr"),
            by =  c("mode", "ctu", "year", "aeo_mode", "type")) %>%
    mutate(vmt = vmt * value) %>%
    select(names(phev_vmt_electric))


  ### VMT all -----
  phev_vmt <- left_join(
    phev_vmt_electric %>%
      select(everything(),
        vmt_electric = vmt
      ),
    phev_vmt_gas %>%
      select(everything(),
        vmt_gas = vmt
      ),
    c(
      "type", "stock", "class",
      "scenario", "mode", "ctu",
      "year", "aeo_mode"
    )
  ) %>%
    rowwise() %>%
    mutate(
      vmt = sum(vmt_electric, vmt_gas),
      class = class
    ) %>%
    select(
      -vmt_electric,
      -vmt_gas
    )



  ## gas ghg direct -----
  phev_ghg_gas <- calc_ghg_direct(
    phev_vmt_gas, transportation_data$passenger, mode,
    "SI", .aeo_scenario, mpg
  ) %>%
    select(everything(),
      dir_ghg_gas = dir_ghg
    )


  phev_fuel_gas <- calc_fuel_use(
    phev_vmt_gas,
    transportation_data$passenger,
    mode,
    "SI",
    .aeo_scenario, mpg
  )

  ## electric ghg direct -----
  phev_ghg_electric <- calc_ghg_direct(
    phev_vmt_electric,
    transportation_data$passenger, mode, .electric_scenario,
    .aeo_scenario, mpe
  ) %>%
    select(everything(),
      dir_ghg_electric = dir_ghg
    )


  phev_dir_ghg <- left_join(
    phev_ghg_gas, phev_ghg_electric,
    c(
      "scenario", "mode", "ctu",
      "year", "aeo_mode", "class"
    )
  ) %>%
    mutate(dir_ghg = sum(dir_ghg_electric, dir_ghg_gas)) %>%
    select(
      -dir_ghg_electric,
      -dir_ghg_gas
    )

  phev_fuel_electric <-
    calc_fuel_use(
      phev_vmt_electric, transportation_data$passenger,
      mode, .electric_scenario, .aeo_scenario,
      mpe
    )

  phev_fuel <- left_join(
    phev_fuel_electric %>%
      select(everything(),
        fuel_use_electric = fuel_use
      ),
    phev_fuel_gas %>%
      select(everything(),
        fuel_use_gas = fuel_use
      ),
    c(
      "scenario", "mode", "ctu", "year",
      "aeo_mode"
    )
  ) %>%
    rowwise() %>%
    mutate(fuel_use = sum(fuel_use_gas, fuel_use_electric)) %>%
    select(
      -fuel_use_gas,
      -fuel_use_electric
    )

  phev_emb_ghg <-
    calc_ghg_embodied(
      transportation_data$passenger,
      mode, "PHEVSales", "PHEV-EMB"
    )


  phev_cost <-
    calc_cost(
      phev_vmt,
      mode,
      "PHEVPrice"
    )


  ## BEV (Battery electric vehicle) -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"

  message("Passenger vehicles, battery electric")


  fcm <- calc_fuel_cost_mile(
    transportation_data$passenger, mode, .aeo_scenario,
    mpe, .enviro_factors$ELEC_FUEL_COST_KWH
  )

  bev_vmt <-
    calc_vmt_forecast(
      .scenario = .scenario,
      tb = transportation_data$passenger,
      .mode = mode,
      .stock = stock,
      .variable = var,
      .tb_fuel_cost_mile = fcm,
      .aeo_scenario = .aeo_scenario,
      .transit_avo = .transit_avo,
      .transit_rider_pct = .transit_rider_pct,
      .vmt_fee = .vmt_fee,
      .payd_fee = .payd_fee,
      .gas_tax = .gas_tax,
      .cong_price = .cong_price,
      .parking_price = .parking_price,
      .drs_pct = .drs_pct,
      .av_pct = .av_pct,
      .freight_vmt_fee = .freight_vmt_fee,
      .pop_dens_pct_change = .pop_dens_pct_change,
      .emp_dens_pct_change = .emp_dens_pct_change,
      .land_use_pct_change = .land_use_pct_change,
      .intersection_design_pct_change = .intersection_design_pct_change,
      .job_access_pct_change = .job_access_pct_change,
      .transit_dist_pct_change = .transit_dist_pct_change,
      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
      .telework_pct = .telework_pct
    ) %>%
    mutate(class = class)



  bev_dir_ghg <-
    calc_ghg_direct(
      bev_vmt, transportation_data$passenger,
      mode, .electric_scenario, .aeo_scenario, mpe
    )



  bev_fuel <-
    calc_fuel_use(
      bev_vmt, transportation_data$passenger,
      mode, .electric_scenario, .aeo_scenario, mpe
    )


  bev_emb_ghg <-
    calc_ghg_embodied(
      transportation_data$passenger, mode,
      "BEVSales", "BEV-EMB"
    )


  bev_cost <-
    calc_cost(
      bev_vmt,
      mode, "BEVPrice"
    )

  # browser()

  # Finish up -----

  fuel_use_all <- dplyr::bind_rows(
    ci_fuel, si_fuel, hev_fuel,
    phev_fuel, bev_fuel
  )


  vmt_all <- dplyr::bind_rows(
    ci_vmt, si_vmt, hev_vmt,
    phev_vmt,
    bev_vmt
  )

  emb_ghg_all <- dplyr::bind_rows(
    ci_emb_ghg, si_emb_ghg, hev_emb_ghg,
    phev_emb_ghg,
    bev_emb_ghg
  )

  dir_ghg_all <- dplyr::bind_rows(
    ci_dir_ghg, si_dir_ghg, hev_dir_ghg,
    phev_dir_ghg,
    bev_dir_ghg
  )

  cost_all <- dplyr::bind_rows(
    si_cost, ci_cost, hev_cost,
    phev_cost,
    bev_cost
  )

  pldv_scenario <- list(
    "vmt" = vmt_all,
    "dir_ghg" = dir_ghg_all,
    "emb_gog" = emb_ghg_all,
    "fuel_use" = fuel_use_all,
    "cost" = cost_all
  )

  usethis::ui_done(paste(
    "Passenger light-duty vehicles",
    emo::ji("automobile")
  ))

  return(pldv_scenario)
}
