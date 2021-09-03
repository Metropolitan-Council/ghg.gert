#' Title
#'
#' @inheritParams scenario_results
#' @inheritParams calc_vmt
#'
#' @return
#' @export
#'
calc_passenger_light_duty <- function(.scenario = "BAU",
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
                                      .mit_bau_summary = 0) {  # Sequence for each
  # 1. Establish `type`, `var`, `mode`
  # 2. Establish `stock`, `mpg`, `class`
  # 3. Calculate fuel cost per mile with `calc_fuel_cost_mile()`
  # 4. Calculate VMT with `calc`


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

  message("Passenger vehicles, gasoline")

  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(
    transportation_data$passenger,
    mode,
    .aeo_scenario,
    mpg,
    SI_FUEL_COST_GAL
  )

  si_vmt <- calc_vmt(
    .scenario, transportation_data$passenger, mode, stock,
    var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct, .vmt_fee,
    .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct,
    .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
    .land_use_pct_change, .intersection_design_pct_change,
    .job_access_pct_change, .transit_dist_pct_change, .comb_5d_impact_pct_change,
    .telework_pct
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
      si_vmt, cost_factors, mode,
      "SIPrice"
    )


  # complete gasoline table


  ## Diesel----
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"

  message("Passenger vehicles, diesel")
  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(
    transportation_data$passenger,
    mode, .aeo_scenario,
    mpg, CI_FUEL_COST_GAL
  )

  ci_vmt <- calc_vmt(
    .scenario, transportation_data$passenger,
    mode, stock, var, fcm,
    .aeo_scenario, .transit_avo, .transit_rider_pct,
    .vmt_fee, .payd_fee, .gas_tax, .cong_price,
    .parking_price, .drs_pct, .av_pct, .freight_vmt_fee,
    .pop_dens_pct_change, .emp_dens_pct_change,
    .land_use_pct_change, .intersection_design_pct_change,
    .job_access_pct_change, .transit_dist_pct_change,
    .comb_5d_impact_pct_change, .telework_pct
  ) %>%
    mutate(class = class)



  ci_dir_ghg <-
    calc_ghg_direct(
      ci_vmt, transportation_data$passenger,
      mode, "CI", .aeo_scenario, mpg
    )


  ci_fuel <-
    calc_fuel_use(
      ci_vmt, transportation_data$passenger,
      mode, "CI", .aeo_scenario, mpg
    )


  ci_emb_ghg <-
    calc_ghg_embodied(
      transportation_data$passenger, mode,
      "CISales", "CI-EMB"
    )


  ci_cost <-
    calc_cost(ci_vmt, cost_factors, mode, "CIPrice")


  ## HEV (Hybrid electric vehicle)----
  stock <- "HEVStock"
  mpg <- "HEVMPG"
  class <- "HEV"

  message("Passenger vehicles, hybrid")

  # Calculate a fuel cost per mile rather than per gallon

  fcm <- calc_fuel_cost_mile(
    transportation_data$passenger, mode, .aeo_scenario,
    mpg, SI_FUEL_COST_GAL
  )

  hev_vmt <-
    calc_vmt(
      .scenario, transportation_data$passenger, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change, .land_use_pct_change,
      .intersection_design_pct_change, .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change, .telework_pct
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
    calc_ghg_embodied(transportation_data$passenger, mode, "HEVSales", "HEV-EMB")

  hev_cost <-
    calc_cost(
      hev_vmt, cost_factors, mode,
      "HEVPrice"
    )

  ## PHEV (Plug-in hybrid) -----
  ### Eqn: (PMT in 1000 mi) x Pr(stock by fuel) x ((Pr(Elec) x elec consumption (MWh per 1000 mi) x GHG per elec) + ((1 - Pr(Elec)) x fuel consumption (per 1000 mi) x GHG per fuel)

  stock <- "PHEVStock"
  mpg <- "PHEVMPG"
  mpe <- "PHEVElec"
  class <- "PHEV"

  # Calculate a fuel cost per mile rather than per gallon
  browser()

  fcm <- calc_fuel_cost_mile(
    transportation_data$passenger, mode, .aeo_scenario,
    mpg, SI_FUEL_COST_GAL
  )
  phev_vmtg <- calc_vmt(
    .scenario, transportation_data$passenger, mode, stock, var,
    fcm, .aeo_scenario, .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee,
    .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change,
    .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
    .transit_dist_pct_change, .comb_5d_impact_pct_change,
    .telework_pct, ch_phev =  1
  )

  left_join(phev_vmtg,
            transportation_data$passenger %>%
              dplyr::filter(mode == mode, var == "PHEVPr"),
            by =  c("mode", "ctu", "year", "aeo_mode", "type"))

  fcm <- calc_fuel_cost_mile(
    transportation_data$passenger, mode, .aeo_scenario,
    mpe, ELEC_FUEL_COST_KWH
  )


  phev_vmte <- calc_vmt(
    .scenario, transportation_data$passenger, mode, stock, var, fcm,
    .aeo_scenario, .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee,
    .gas_tax, .cong_price,
    .parking_price, .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change,
    .emp_dens_pct_change, .land_use_pct_change,
    .intersection_design_pct_change, .job_access_pct_change,
    .transit_dist_pct_change, .comb_5d_impact_pct_change,
    .telework_pct
  ) * transportation_data$passenger %>%
    dplyr::filter(mode == mode, var == "PHEVPr") %>%
    dplyr::select(all_of(YRS))

  phev_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "VMT",
    phev_vmtg + phev_vmte
  )

  phev_ghgg <- calc_ghg_direct(
    phev_vmtg, transportation_data$passenger, mode,
    "SI", .aeo_scenario, mpg
  )
  phev_fuelg <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "PETRO",
    calc_fuel_use(
      phev_vmtg, transportation_data$passenger, mode, "SI",
      .aeo_scenario, mpg
    )
  )

  phev_ghge <- calc_ghg_direct(
    phev_vmte, transportation_data$passenger, mode, .electric_scenario,
    .aeo_scenario, mpe
  )
  phev_dir_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "DIR-GHG",
    phev_ghgg + phev_ghge
  )

  phev_fuele <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "ELEC",
    calc_fuel_use(
      phev_vmte, transportation_data$passenger,
      mode, .electric_scenario, .aeo_scenario,
      mpe
    )
  )

  phev_emb_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "INDIR-GHG",
    calc_ghg_embodied(
      transportation_data$passenger,
      mode, "PHEVSales", "PHEV-EMB"
    )
  )

  phev_cost <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "COST",
    calc_cost(
      phev_vmt, cost_factors,
      mode, "PHEVPrice"
    )
  )

  ## BEV (Battery electric vehicle) -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"

  fcm <- calc_fuel_cost_mile(
    transportation_data$passenger, mode, .aeo_scenario,
    mpe, ELEC_FUEL_COST_KWH
  )

  bev_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "VMT",
    calc_vmt(
      .scenario, transportation_data$passenger, mode,
      stock, var, fcm, .aeo_scenario,
      .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee,
      .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change, .telework_pct
    )
  )


  bev_dir_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(
      bev_vmt, transportation_data$passenger,
      mode, .electric_scenario, .aeo_scenario, mpe
    )
  )


  bev_fuel <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "ELEC",
    calc_fuel_use(
      bev_vmt, transportation_data$passenger,
      mode, .electric_scenario, .aeo_scenario, mpe
    )
  )

  bev_emb_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "INDIR-GHG",
    calc_ghg_embodied(
      transportation_data$passenger, mode,
      "BEVSales", "BEV-EMB"
    )
  )

  bev_cost <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "COST",
    calc_cost(
      bev_vmt, cost_factors,
      mode, "BEVPrice"
    )
  )

  browser()
  # Combine the PLDV data
  out_sum <- dplyr::bind_rows(
    si_vmt, si_dir_ghg, si_fuel,
    si_emb_ghg, si_cost, ci_vmt,
    ci_dir_ghg, ci_fuel, ci_emb_ghg,
    ci_cost, hev_vmt, hev_dir_ghg,
    hev_fuel, hev_emb_ghg, hev_cost,
    phev_vmt, phev_dir_ghg, phev_fuelg,
    phev_fuele, phev_emb_ghg, phev_cost,
    bev_vmt, bev_dir_ghg, bev_fuel,
    bev_emb_ghg, bev_cost
  )}
