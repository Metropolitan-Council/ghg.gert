#' @title  Main function to call other functions for determining VMT,
#'      direct GHG, indirect GHG, and costs
#'
#' @param .electric_scenario electricity scenario
#' @param .aeo_scenario selected EIA Annual Energy Outlook scenario
#' @param .ctu chosen CTU
#' @param .drs_fuel_type input dynamic ride sharing (DRS) fuel type. Default is `0`.
#' @param .av_fuel_type input AV fuel type
#' @param .mit_bau_summary input of BAU data for calculations in MIT scenario
#' @inheritParams calc_vmt
#'
#' @return
#' @export
#' @importFrom dplyr filter select case_when across bind_rows cur_column
#' @importFrom tidyselect all_of
#' @importFrom tibble tibble
#' @family transportation
scenario_results <- function(.scenario = "BAU",
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
                             .mit_bau_summary = 0) {
  # If the user has specified DRS, then reduce the PMT for non-DRS trips
  # browser()
  if (.drs_pct > 0) {
    pass_transpo <- pass_transpo %>%
      dplyr::mutate(
        dplyr::across(
          all_of(YRS), ~ dplyr::case_when(
            ((mode == "PLDV") &
              var == "PMT") ~ .x *
              dplyr::case_when(
                .drs_pct > 0 ~ (1 - pass_transpo %>%
                  dplyr::filter(var == "DRSShare") %>%
                  dplyr::select(dplyr::cur_column()) %>%
                  as.numeric() * .drs_pct / 100),
                TRUE ~ 1
              ),
            TRUE ~ .x
          )
        )
      )
  }


  # Sequence for each
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

  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(
    pass_transpo,
    mode,
    .aeo_scenario,
    mpg,
    SI_FUEL_COST_GAL
  )

  si_vmt <- tibble::tibble(
    type = type,
    scenario = .scenario,
    mode = mode,
    class = class,
    ctu = .ctu,
    output = "VMT",
    calc_vmt(
      .scenario, pass_transpo, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct, .vmt_fee,
      .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct,
      .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change, .comb_5d_impact_pct_change,
      .telework_pct
    )
  )

  si_dir_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode,
    class = class, ctu = .ctu,
    output = "DIR-GHG",
    calc_ghg_direct(
      tb_vmt = si_vmt,
      tb = pass_transpo,
      .mode = mode,
      .fuel_type = "SI",
      .aeo_scenario = .aeo_scenario,
      .mles_per_gallon = mpg
    )
  )

  si_fuel <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu,
    output = "PETRO",
    calc_fuel_use(
      si_vmt, pass_transpo,
      mode, "SI",
      .aeo_scenario, mpg
    )
  )

  si_emb_ghg <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu,
    output = "INDIR-GHG",
    calc_ghg_embodied(
      pass_transpo, mode,
      "SISales",
      "SI-EMB"
    )
  )

  si_cost <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu,
    output = "COST",
    calc_cost(
      si_vmt, cost_factors, mode,
      "SIPrice"
    )
  )

  ## Diesel----
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"

  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(
    pass_transpo, mode, .aeo_scenario,
    mpg, CI_FUEL_COST_GAL
  )

  ci_vmt <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class, ctu = .ctu,
    output = "VMT", calc_vmt(
      .scenario, pass_transpo,
      mode, stock, var, fcm,
      .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price,
      .parking_price, .drs_pct, .av_pct, .freight_vmt_fee,
      .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change, .telework_pct
    )
  )

  ci_dir_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(
      ci_vmt, pass_transpo,
      mode, "CI", .aeo_scenario, mpg
    )
  )

  ci_fuel <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "PETRO",
    calc_fuel_use(
      ci_vmt, pass_transpo,
      mode, "CI", .aeo_scenario, mpg
    )
  )

  ci_emb_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "INDIR-GHG",
    calc_ghg_embodied(
      pass_transpo, mode,
      "CISales", "CI-EMB"
    )
  )

  ci_cost <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "COST",
    calc_cost(ci_vmt, cost_factors, mode, "CIPrice")
  )

  ## HEV (Hybrid electric vehicle)----
  stock <- "HEVStock"
  mpg <- "HEVMPG"
  class <- "HEV"

  # Calculate a fuel cost per mile rather than per gallon

  fcm <- calc_fuel_cost_mile(
    pass_transpo, mode, .aeo_scenario,
    mpg, SI_FUEL_COST_GAL
  )

  hev_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "VMT",
    calc_vmt(
      .scenario, pass_transpo, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change, .land_use_pct_change,
      .intersection_design_pct_change, .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change, .telework_pct
    )
  )

  hev_dir_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(
      hev_vmt, pass_transpo,
      mode, "SI", .aeo_scenario, mpg
    )
  )

  hev_fuel <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "PETRO",
    calc_fuel_use(
      hev_vmt, pass_transpo,
      mode, "SI", .aeo_scenario, mpg
    )
  )

  hev_emb_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "INDIR-GHG",
    calc_ghg_embodied(pass_transpo, mode, "HEVSales", "HEV-EMB")
  )

  hev_cost <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "COST",
    calc_cost(
      hev_vmt, cost_factors, mode,
      "HEVPrice"
    )
  )

  ## PHEV (Plug-in hybrid) -----
  ### Eqn: (PMT in 1000 mi) x Pr(stock by fuel) x ((Pr(Elec) x elec consumption (MWh per 1000 mi) x GHG per elec) + ((1 - Pr(Elec)) x fuel consumption (per 1000 mi) x GHG per fuel)

  stock <- "PHEVStock"
  mpg <- "PHEVMPG"
  mpe <- "PHEVElec"
  class <- "PHEV"

  # Calculate a fuel cost per mile rather than per gallon

  fcm <- calc_fuel_cost_mile(
    pass_transpo, mode, .aeo_scenario,
    mpg, SI_FUEL_COST_GAL
  )
  phev_vmtg <- calc_vmt(
    .scenario, pass_transpo, mode, stock, var,
    fcm, .aeo_scenario, .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee,
    .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change,
    .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
    .transit_dist_pct_change, .comb_5d_impact_pct_change,
    .telework_pct, 1
  ) * (1 - pass_transpo %>%
    dplyr::filter(mode == mode, var == "PHEVPr") %>%
    dplyr::select(all_of(YRS)))

  fcm <- calc_fuel_cost_mile(
    pass_transpo, mode, .aeo_scenario,
    mpe, ELEC_FUEL_COST_KWH
  )
  phev_vmte <- calc_vmt(
    .scenario, pass_transpo, mode, stock, var, fcm,
    .aeo_scenario, .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee, .gas_tax, .cong_price,
    .parking_price, .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change, .land_use_pct_change,
    .intersection_design_pct_change, .job_access_pct_change, .transit_dist_pct_change, .comb_5d_impact_pct_change,
    .telework_pct
  ) * pass_transpo %>%
    dplyr::filter(mode == mode, var == "PHEVPr") %>%
    dplyr::select(all_of(YRS))

  phev_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "VMT",
    phev_vmtg + phev_vmte
  )

  phev_ghgg <- calc_ghg_direct(
    phev_vmtg, pass_transpo, mode,
    "SI", .aeo_scenario, mpg
  )
  phev_fuelg <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "PETRO",
    calc_fuel_use(
      phev_vmtg, pass_transpo, mode, "SI",
      .aeo_scenario, mpg
    )
  )

  phev_ghge <- calc_ghg_direct(
    phev_vmte, pass_transpo, mode, .electric_scenario,
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
      phev_vmte, pass_transpo,
      mode, .electric_scenario, .aeo_scenario,
      mpe
    )
  )

  phev_emb_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "INDIR-GHG",
    calc_ghg_embodied(
      pass_transpo,
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
    pass_transpo, mode, .aeo_scenario,
    mpe, ELEC_FUEL_COST_KWH
  )

  bev_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "VMT",
    calc_vmt(
      .scenario, pass_transpo, mode,
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
      bev_vmt, pass_transpo,
      mode, .electric_scenario, .aeo_scenario, mpe
    )
  )


  bev_fuel <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "ELEC",
    calc_fuel_use(
      bev_vmt, pass_transpo,
      mode, .electric_scenario, .aeo_scenario, mpe
    )
  )

  bev_emb_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "INDIR-GHG",
    calc_ghg_embodied(
      pass_transpo, mode,
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
  )

  # Transit-----
  # Caculate a fuel cost for use in DRS and transit calculations. Use gasoline PLDV value.
  mpg <- "SIMPG"
  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(
    pass_transpo, mode,
    .aeo_scenario, mpg, SI_FUEL_COST_GAL
  )

  ## Bus Urban -----
  mode <- "BU"

  ## Bus with Biodiesel-----
  stock <- "BCIStock"
  mpg <- "BCIMPG"
  class <- "BCI"

  ci_vmt <- tibble::tibble(
    type = type,
    scenario = .scenario,
    mode = mode,
    class = class, ctu = .ctu, output = "VMT",
    calc_vmt(
      .scenario, pass_transpo, mode, stock, var,
      fcm, .aeo_scenario, .transit_avo, .transit_rider_pct, .vmt_fee,
      .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct, .freight_vmt_fee,
      .pop_dens_pct_change, .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change, .comb_5d_impact_pct_change
    )
  )

  ci_ghg <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(
      ci_vmt, pass_transpo, mode,
      "CI", .aeo_scenario, mpg
    )
  )

  ci_fuel <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "PETRO",
    calc_fuel_use(
      ci_vmt, pass_transpo, mode, "CI",
      .aeo_scenario, mpg
    )
  )

  ci_emb_ghg <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu,
    output = "INDIR-GHG",
    calc_ghg_embodied(
      pass_transpo,
      mode, "BCISales",
      "BU-BCI-EMB", class, .transit_avo,
      ci_vmt, .mit_bau_summary
    )
  )

  ci_cost <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "COST",
    calc_cost(ci_vmt, cost_factors, mode, "BCIPrice")
  )

  ## HEV Bus ------
  stock <- "HEVStock"
  mpg <- "HEVMPG"
  class <- "HEV"

  hev_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "VMT",
    calc_vmt(
      .scenario, pass_transpo, mode, stock, var,
      fcm, .aeo_scenario, .transit_avo, .transit_rider_pct, .vmt_fee,
      .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change, .land_use_pct_change,
      .intersection_design_pct_change, .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change
    )
  )

  hev_ghg <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu,
    output = "DIR-GHG", calc_ghg_direct(
      hev_vmt, pass_transpo, mode, "CI",
      .aeo_scenario, mpg
    )
  )

  hev_fuel <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "PETRO",
    calc_fuel_use(
      hev_vmt, pass_transpo,
      mode, "CI", .aeo_scenario, mpg
    )
  )

  hev_emb_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "INDIR-GHG",
    calc_ghg_embodied(
      pass_transpo,
      mode, "HEVSales",
      "BU-HEV-EMB", class, .transit_avo,
      hev_vmt, .mit_bau_summary
    )
  )

  hev_cost <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "COST",
    calc_cost(hev_vmt, cost_factors, mode, "HEVPrice")
  )

  ## BEV Bus -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"

  bev_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "VMT",
    calc_vmt(
      .scenario, pass_transpo, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct,
      .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change
    )
  )

  bev_ghg <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(
      bev_vmt, pass_transpo,
      mode,
      .electric_scenario, .aeo_scenario, mpe
    )
  )

  bev_fuel <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "ELEC",
    calc_fuel_use(
      bev_vmt, pass_transpo,
      mode, .electric_scenario, .aeo_scenario, mpe
    )
  )

  bev_emb_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class, ctu = .ctu,
    output = "INDIR-GHG",
    calc_ghg_embodied(
      pass_transpo, mode,
      "BEVSales", "BU-BEV-EMB",
      class, .transit_avo,
      bev_vmt, .mit_bau_summary
    )
  )

  bev_cost <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "COST",
    calc_cost(
      bev_vmt, cost_factors, mode,
      "BEVPrice"
    )
  )

  # Add the BU data
  out_sum <- dplyr::bind_rows(
    out_sum, ci_vmt, ci_ghg, ci_fuel,
    ci_emb_ghg, ci_cost, hev_vmt,
    hev_ghg, hev_fuel, hev_emb_ghg,
    hev_cost, bev_vmt, bev_ghg,
    bev_fuel, bev_emb_ghg, bev_cost
  )

  # Bus Rapid Transit----

  ## BCI BRT -----
  mode <- "BRT"
  stock <- "BCIStock"
  mpg <- "BCIMPG"
  class <- "BCI"

  ci_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "VMT",
    calc_vmt(
      .scenario, pass_transpo, mode, stock, var,
      fcm, .aeo_scenario, .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee,
      .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct, .freight_vmt_fee,
      .pop_dens_pct_change, .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change, .comb_5d_impact_pct_change
    )
  )

  ci_ghg <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(
      ci_vmt, pass_transpo, mode,
      "CI", .aeo_scenario, mpg
    )
  )

  ci_fuel <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "PETRO",
    calc_fuel_use(
      ci_vmt, pass_transpo, mode, "CI",
      .aeo_scenario, mpg
    )
  )

  ci_emb_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class, ctu = .ctu,
    output = "INDIR-GHG",
    calc_ghg_embodied(
      pass_transpo, mode,
      "BCISales", "BU-BCI-EMB",
      class, .transit_avo, ci_vmt,
      .mit_bau_summary
    )
  )

  ci_cost <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "COST",
    calc_cost(
      ci_vmt,
      cost_factors, mode, "BCIPrice"
    )
  )

  ## HEV BRT -----
  stock <- "HEVStock"
  mpg <- "HEVMPG"
  class <- "HEV"

  hev_vmt <- tibble::tibble(
    type = type,
    scenario = .scenario,
    mode = mode, class = class, ctu = .ctu, output = "VMT",
    calc_vmt(
      .scenario, pass_transpo, mode, stock, var, fcm, .aeo_scenario,
      .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    )
  )

  hev_ghg <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(
      hev_vmt, pass_transpo,
      mode, "CI", .aeo_scenario, mpg
    )
  )

  hev_fuel <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "PETRO",
    calc_fuel_use(
      hev_vmt, pass_transpo, mode, "CI",
      .aeo_scenario, mpg
    )
  )

  hev_emb_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "INDIR-GHG",
    calc_ghg_embodied(
      pass_transpo, mode,
      "HEVSales", "BU-HEV-EMB",
      class, .transit_avo, hev_vmt,
      .mit_bau_summary
    )
  )

  hev_cost <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "COST",
    calc_cost(
      hev_vmt, cost_factors,
      mode, "HEVPrice"
    )
  )

  ## BEV BRT -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"

  bev_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "VMT",
    calc_vmt(
      .scenario, pass_transpo, mode, stock, var, fcm,
      .aeo_scenario, .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee, .gas_tax,
      .cong_price, .parking_price, .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change,
      .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    )
  )

  bev_ghg <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(
      bev_vmt, pass_transpo,
      mode, .electric_scenario, .aeo_scenario, mpe
    )
  )

  bev_fuel <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "ELEC",
    calc_fuel_use(
      bev_vmt, pass_transpo, mode,
      .electric_scenario, .aeo_scenario, mpe
    )
  )

  bev_emb_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class, ctu = .ctu,
    output = "INDIR-GHG",
    calc_ghg_embodied(
      pass_transpo, mode,
      "BEVSales", "BU-BEV-EMB",
      class, .transit_avo, bev_vmt,
      .mit_bau_summary
    )
  )

  bev_cost <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "COST",
    calc_cost(
      bev_vmt,
      cost_factors, mode, "BEVPrice"
    )
  )

  # Add the BRT data
  out_sum <- dplyr::bind_rows(
    out_sum, ci_vmt, ci_ghg,
    ci_fuel, ci_emb_ghg, ci_cost,
    hev_vmt, hev_ghg, hev_fuel,
    hev_emb_ghg, hev_cost, bev_vmt,
    bev_ghg, bev_fuel, bev_emb_ghg, bev_cost
  )

  # Rail Urban-----

  ## EV Rail -----
  mode <- "RU"
  stock <- "EVStock"
  mpe <- "EVElec"
  class <- "EV"

  ev_vmt <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "VMT",
    calc_vmt(
      .scenario, pass_transpo, mode,
      stock, var, fcm, .aeo_scenario,
      .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee,
      .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    )
  )

  ev_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(
      bev_vmt,
      pass_transpo,
      mode, .electric_scenario,
      .aeo_scenario,
      mpe
    )
  )

  ev_fuel <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "ELEC",
    calc_fuel_use(
      bev_vmt, pass_transpo,
      mode, .electric_scenario, .aeo_scenario, mpe
    )
  )

  ev_cost <- tibble::tibble(
    type = type,
    scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "COST",
    calc_cost(ev_vmt, cost_factors, mode, "EVPrice")
  )

  # Add the RU data
  out_sum <- dplyr::bind_rows(
    out_sum, ev_vmt,
    ev_ghg, ev_fuel, ev_cost
  )

  # Rail Interurban-----
  mode <- "RI"

  ## BCI Rail interurban -----
  stock <- "BCIStock"
  mpg <- "BCIMPG"
  class <- "BCI"
  ci_vmt <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "VMT",
    calc_vmt(
      .scenario, pass_transpo,
      mode, stock, var, fcm,
      .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price,
      .parking_price, .drs_pct, .av_pct, .freight_vmt_fee,
      .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    )
  )

  ci_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(ci_vmt, pass_transpo, mode, "BCI", .aeo_scenario, mpg)
  )

  ci_fuel <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "PETRO",
    calc_fuel_use(
      ci_vmt, pass_transpo,
      mode, "BCI", .aeo_scenario, mpg
    )
  )

  ci_cost <- tibble::tibble(
    type = type,
    scenario = .scenario, mode = mode, class = class,
    ctu = .ctu, output = "COST",
    calc_cost(ci_vmt, cost_factors, mode, "BCIPrice")
  )


  ## EV Rail Inter -----
  stock <- "EVStock"
  mpe <- "EVElec"
  class <- "EV"
  ev_vmt <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "VMT",
    calc_vmt(
      .scenario, pass_transpo, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change, .comb_5d_impact_pct_change
    )
  )

  ev_ghg <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode, class = class,
    ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(bev_vmt, pass_transpo, mode, .electric_scenario, .aeo_scenario, mpe)
  )

  ev_fuel <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "ELEC",
    calc_fuel_use(bev_vmt, pass_transpo, mode, .electric_scenario, .aeo_scenario, mpe)
  )

  ev_cost <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "COST",
    calc_cost(ev_vmt, cost_factors, mode, "EVPrice")
  )

  # Add the RI data
  out_sum <- dplyr::bind_rows(
    out_sum, ci_vmt,
    ci_ghg, ci_fuel, ci_cost,
    ev_vmt, ev_ghg, ev_fuel, ev_cost
  )

  # School Bus-----
  mode <- "BS"

  ## CI School bus -----
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"

  ci_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode, class = class,
    ctu = .ctu, output = "VMT",
    calc_vmt(
      .scenario, pass_transpo,
      mode, stock, var, fcm,
      .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price,
      .parking_price, .drs_pct, .av_pct, .freight_vmt_fee,
      .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change
    )
  )

  ci_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(
      ci_vmt, pass_transpo,
      mode, "CI", .aeo_scenario, mpg
    )
  )

  ci_fuel <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "PETRO",
    calc_fuel_use(
      ci_vmt, pass_transpo, mode,
      "CI", .aeo_scenario, mpg
    )
  )

  ci_cost <- tibble::tibble(
    type = type,
    scenario = .scenario, mode = mode,
    class = class, ctu = .ctu,
    output = "COST",
    calc_cost(
      ci_vmt,
      cost_factors, mode, "CIPrice"
    )
  )


  ## BEV school bus -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"

  bev_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "VMT",
    calc_vmt(
      .scenario, pass_transpo, mode, stock, var, fcm,
      .aeo_scenario, .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee, .gas_tax, .cong_price,
      .parking_price, .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change
    )
  )

  bev_ghg <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(
      bev_vmt,
      pass_transpo, mode, .electric_scenario,
      .aeo_scenario, mpe
    )
  )

  bev_fuel <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class, ctu = .ctu,
    output = "ELEC",
    calc_fuel_use(
      bev_vmt, pass_transpo, mode,
      .electric_scenario, .aeo_scenario, mpe
    )
  )

  bev_cost <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "COST",
    calc_cost(
      bev_vmt,
      cost_factors,
      mode, "BEVPrice"
    )
  )


  # Add the BS data
  out_sum <- dplyr::bind_rows(
    out_sum,
    ci_vmt, ci_ghg, ci_fuel,
    ci_cost, bev_vmt, bev_ghg, bev_cost
  )

  # Active Modes-----
  ## Walk -----
  mode <- "WALK"
  stock <- ""
  class <- "WALK"
  walk_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "VMT",
    calc_vmt(
      .scenario, pass_transpo, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct,
      .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    )
  )

  ## Bike -----
  mode <- "BIKE"
  stock <- ""
  class <- "BIKE"

  bike_vmt <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "VMT",
    calc_vmt(
      .scenario, pass_transpo, mode,
      stock, var, fcm, .aeo_scenario,
      .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee,
      .cong_price, .parking_price, .drs_pct,
      .av_pct, .freight_vmt_fee,
      .pop_dens_pct_change, .emp_dens_pct_change, .land_use_pct_change,
      .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    )
  )


  # Add the ACTIVE data
  out_sum <- dplyr::bind_rows(out_sum, walk_vmt, bike_vmt)

  # Dynamic Ride Sharing -----
  mode <- "DRS"

  # Use PLDV features in some cases
  mode_1 <- "PLDV"

  # If DRS is included,
  # then perform calculations depending if fuel is BEV, HEV, or PHEV

  if (.scenario != "BAU" & .drs_pct > 0) {
    # Calculate DRS sales in each year
    drs_sales <- tibble::tibble(
      mode = mode,
      var = "DRSSales", ctu = .ctu,
      calc_drs_sales(pass_transpo, .drs_pct)
    )

    pass_transpo <- dplyr::bind_rows(pass_transpo, drs_sales)

    if (.drs_fuel_type == "HEV") {
      ## DRS Hybrid fuel -----
      stock <- "DRSStock"
      mpg <- "HEVMPG"
      class <- "HEV"
      fcm <- calc_fuel_cost_mile(
        pass_transpo, mode,
        .aeo_scenario, mpg, SI_FUEL_COST_GAL
      )

      drs_vmt <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "VMT",
        calc_drs_vmt(
          pass_transpo, .drs_pct,
          class, fcm, .vmt_fee,
          .payd_fee, .gas_tax, .cong_price,
          .parking_price, .pop_dens_pct_change,
          .emp_dens_pct_change, .land_use_pct_change,
          .intersection_design_pct_change, .job_access_pct_change,
          .transit_dist_pct_change, .comb_5d_impact_pct_change
        )
      )


      drs_dir_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "DIR-GHG",
        calc_ghg_direct(
          drs_vmt,
          pass_transpo,
          mode_1, "SI",
          .aeo_scenario, mpg, 1
        )
      )
      .drs_fuel_type <- tibble::tibble(
        type = type, scenario = .scenario, mode = mode,
        class = class, ctu = .ctu, output = "PETRO",
        calc_fuel_use(
          drs_vmt,
          pass_transpo,
          mode_1, "SI",
          .aeo_scenario, mpg, 1
        )
      )

      temp <- calc_ghg_embodied(
        pass_transpo,
        mode, "DRSSales",
        "HEV-EMB"
      ) %>%
        as.numeric()

      out_sum <- out_sum %>%
        dplyr::mutate(
          dplyr::across(all_of(YRS), ~ dplyr::case_when(
            (mode == mode_1 &
              class == class &
              output == "INDIR-GHG") ~ .x + temp %>% as.numeric(),
            TRUE ~ .x
          ))
        )

      drs_cost <- tibble::tibble(
        type = type, scenario = .scenario, mode = mode,
        class = class, ctu = .ctu, output = "COST",
        calc_cost(
          drs_vmt, cost_factors,
          mode_1, "HEVPrice", 1
        )
      )

      # Add the DRS data
      out_sum <- dplyr::bind_rows(
        out_sum,
        drs_vmt, drs_dir_ghg,
        .drs_fuel_type, drs_cost
      )
    } else if (.drs_fuel_type == "PHEV") {
      ## DRS Plug-in hybrid -----
      stock <- "DRSStock"
      mpg <- "PHEVMPG"
      mpe <- "PHEVElec"
      class <- "PHEV"

      # Don't apply the .gas_tax factors, etc. to PHEV for DRS
      phev_vmtg <- calc_drs_vmt(
        pass_transpo, .drs_pct, class, fcm, .vmt_fee,
        .payd_fee, .gas_tax, .cong_price, .parking_price, .pop_dens_pct_change,
        .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change,
        .job_access_pct_change, .transit_dist_pct_change,
        .comb_5d_impact_pct_change
      ) * (
        1 - pass_transpo %>%
          dplyr::filter(mode == mode, var == "PHEVPr") %>%
          dplyr::select(all_of(YRS)))

      phev_vmte <- calc_drs_vmt(
        pass_transpo, .drs_pct, class,
        fcm, .vmt_fee, .payd_fee, .gas_tax, .cong_price,
        .parking_price, .pop_dens_pct_change, .emp_dens_pct_change,
        .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
        .transit_dist_pct_change, .comb_5d_impact_pct_change
      ) *
        pass_transpo %>%
          dplyr::filter(mode == mode, var == "PHEVPr") %>%
          dplyr::select(all_of(YRS))

      drs_vmt <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "VMT",
        phev_vmtg + phev_vmte
      )

      phev_ghgg <- calc_ghg_direct(
        phev_vmtg, pass_transpo,
        mode, "SI", .aeo_scenario, mpg, 1
      )

      phev_ghge <- calc_ghg_direct(
        phev_vmte, pass_transpo,
        mode, .electric_scenario, .aeo_scenario, mpe, 1
      )

      drs_dir_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "DIR-GHG",
        phev_ghgg + phev_ghge
      )

      drs_fuelg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "PETRO",
        calc_fuel_use(
          phev_vmtg,
          pass_transpo, mode, "SI",
          .aeo_scenario, mpg, 1
        )
      )

      drs_fuele <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "ELEC",
        calc_fuel_use(
          phev_vmte, pass_transpo,
          mode, .electric_scenario, .aeo_scenario, mpe, 1
        )
      )

      temp <- calc_ghg_embodied(
        pass_transpo, mode,
        "DRSSales", "PHEV-EMB"
      ) %>% as.numeric()

      out_sum <- out_sum %>%
        dplyr::mutate(
          dplyr::across(all_of(YRS), ~ dplyr::case_when(
            (mode == mode_1 &
              class == class &
              output == "INDIR-GHG") ~ .x + temp %>% as.numeric(),
            TRUE ~ .x
          ))
        )

      drs_cost <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "COST",
        calc_cost(
          drs_vmt,
          cost_factors,
          mode_1, "PHEVPrice", 1
        )
      )

      # Add the DRS data
      out_sum <- dplyr::bind_rows(
        out_sum, drs_vmt, drs_dir_ghg, drs_fuelg,
        drs_fuele, drs_cost
      )
    } else {
      ## DRS Battery Electric -----
      stock <- "DRSStock"
      mpe <- "BEVElec"
      class <- "BEV"
      drs_vmt <- tibble::tibble(
        type = type,
        scenario = .scenario, mode = mode,
        class = class, ctu = .ctu,
        output = "VMT",
        calc_drs_vmt(
          pass_transpo, .drs_pct,
          class, fcm, .vmt_fee, .payd_fee,
          .gas_tax, .cong_price, .parking_price,
          .pop_dens_pct_change, .emp_dens_pct_change,
          .land_use_pct_change, .intersection_design_pct_change,
          .job_access_pct_change, .transit_dist_pct_change,
          .comb_5d_impact_pct_change
        )
      )

      drs_dir_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "DIR-GHG",
        calc_ghg_direct(
          drs_vmt,
          pass_transpo,
          mode_1, .electric_scenario,
          .aeo_scenario, mpe, 1
        )
      )

      .drs_fuel_type <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "ELEC",
        calc_fuel_use(
          drs_vmt,
          pass_transpo, mode_1, .electric_scenario,
          .aeo_scenario, mpe, 1
        )
      )

      temp <- calc_ghg_embodied(
        pass_transpo,
        mode, "DRSSales", "BEV-EMB"
      ) %>%
        as.numeric()

      out_sum <- out_sum %>%
        dplyr::mutate(dplyr::across(all_of(YRS), ~ dplyr::case_when(
          (mode == mode_1 &
            class == class &
            output == "INDIR-GHG") ~ .x + temp %>% as.numeric(),
          TRUE ~ .x
        )))

      drs_cost <- tibble::tibble(
        type = type,
        scenario = .scenario, mode = mode,
        class = class, ctu = .ctu,
        output = "COST",
        calc_cost(
          drs_vmt,
          cost_factors,
          mode_1, "BEVPrice"
        )
      )

      # Add the DRS data
      out_sum <- dplyr::bind_rows(
        out_sum,
        drs_vmt, drs_dir_ghg,
        .drs_fuel_type, drs_cost
      )
    }
  }

  # AV-----
  mode <- "AV"
  # Use PLDV features in some cases
  mode_1 <- "PLDV"

  # If AV is included, then perform calculations depending
  # if fuel type is BEV, HEV, or PHEV
  if (.scenario != "BAU" & .av_pct > 0) {
    # Calculate DRS sales in each year
    av_sales <- tibble::tibble(
      mode = mode,
      var = "AVSales", ctu = .ctu,
      calc_av_sales(
        pass_transpo,
        .av_pct
      )
    )

    pass_transpo <- dplyr::bind_rows(pass_transpo, av_sales)


    if (.av_fuel_type == "HEV") {
      ## AV Hybrid electric ----
      stock <- "AVStock"
      mpg <- "HEVMPG"
      class <- "HEV"

      # Calculate a fuel cost per mile rather than per gallon
      fcm <- calc_fuel_cost_mile(
        pass_transpo,
        mode_1, .aeo_scenario, mpg, SI_FUEL_COST_GAL, .av_pct
      )
      av_vmt <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class, ctu = .ctu,
        output = "VMT",
        calc_vmt(
          .scenario, pass_transpo,
          mode, stock, var,
          fcm, .aeo_scenario,
          .transit_avo, .transit_rider_pct,
          .vmt_fee, .payd_fee, .gas_tax,
          .cong_price, .parking_price, .drs_pct,
          .av_pct, .freight_vmt_fee,
          .pop_dens_pct_change, .emp_dens_pct_change,
          .land_use_pct_change, .intersection_design_pct_change,
          .job_access_pct_change, .transit_dist_pct_change,
          .comb_5d_impact_pct_change,
          .telework_pct
        )
      )


      av_dir_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "DIR-GHG",
        calc_ghg_direct(
          av_vmt,
          pass_transpo,
          mode_1, "SI", .aeo_scenario,
          mpg, .av_pct
        )
      )


      .av_fuel_type <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "PETRO",
        calc_fuel_use(
          av_vmt, pass_transpo,
          mode_1, "SI", .aeo_scenario,
          mpg, .av_pct
        )
      )

      av_emb_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "INDIR-GHG",
        calc_ghg_embodied(
          pass_transpo,
          mode, "AVSales",
          "HEV-EMB"
        )
      )

      av_cost <- tibble::tibble(
        type = type, scenario = .scenario, mode = mode,
        class = class, ctu = .ctu, output = "COST",
        calc_cost(
          av_vmt,
          cost_factors, mode_1,
          "HEVPrice", 1
        )
      )

      # Add the AV data
      out_sum <- dplyr::bind_rows(
        out_sum, av_vmt, av_dir_ghg, .av_fuel_type,
        av_emb_ghg, av_cost
      )
    } else if (.av_fuel_type == "PHEV") {
      ## AV Plug-in hygbrid -----
      stock <- "AVStock"
      mpg <- "PHEVMPG"
      mpe <- "PHEVElec"
      class <- "PHEV"
      # Calculate a fuel cost per mile rather than per gallon
      fcm <- calc_fuel_cost_mile(
        pass_transpo, mode_1, .aeo_scenario,
        mpg, SI_FUEL_COST_GAL, .av_pct
      )

      phev_vmtg <- calc_vmt(
        .scenario, pass_transpo, mode, stock,
        var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
        .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
        .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change, .land_use_pct_change,
        .intersection_design_pct_change, .job_access_pct_change, .transit_dist_pct_change,
        .comb_5d_impact_pct_change, .telework_pct, 1
      ) * (1 - pass_transpo %>%
        dplyr::filter(mode == mode, var == "PHEVPr") %>%
        dplyr::select(all_of(YRS)))

      fcm <- calc_fuel_cost_mile(
        pass_transpo, mode,
        .aeo_scenario, mpe, ELEC_FUEL_COST_KWH
      )

      phev_vmte <- calc_vmt(
        .scenario, pass_transpo, mode,
        stock, var, fcm, .aeo_scenario,
        .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee,
        .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
        .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
        .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
        .transit_dist_pct_change, .comb_5d_impact_pct_change,
        .telework_pct
      ) * pass_transpo %>%
        dplyr::filter(mode == mode, var == "PHEVPr") %>%
        dplyr::select(all_of(YRS))

      av_vmt <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "VMT",
        phev_vmtg + phev_vmte
      )

      phev_ghgg <- calc_ghg_direct(
        phev_vmtg, pass_transpo,
        mode, "SI", .aeo_scenario, mpg, .av_pct
      )

      phev_ghge <- calc_ghg_direct(
        phev_vmtg, pass_transpo,
        mode, .electric_scenario, .aeo_scenario, mpe, .av_pct
      )

      av_dir_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "DIR-GHG",
        phev_ghgg + phev_ghge
      )

      av_fuelg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "PETRO",
        calc_fuel_use(
          phev_vmtg,
          pass_transpo, mode, "SI",
          .aeo_scenario, mpg, .av_pct
        )
      )

      av_fuele <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "ELEC",
        calc_fuel_use(
          phev_vmte,
          pass_transpo,
          mode, .electric_scenario, .aeo_scenario,
          mpe, .av_pct
        )
      )

      av_emb_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "INDIR-GHG",
        calc_ghg_embodied(
          pass_transpo, mode,
          "AVSales", "PHEV-EMB"
        )
      )

      av_cost <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "COST",
        calc_cost(
          av_vmt,
          ost_factors, mode_1,
          "PHEVPrice", 1
        )
      )
      # Add the AV data
      out_sum <- dplyr::bind_rows(
        out_sum,
        av_vmt, av_dir_ghg, av_fuelg, av_fuele,
        av_emb_ghg, av_cost
      )
    } else {
      ## AV Battery electric -----
      stock <- "AVStock"
      mpe <- "BEVElec"
      class <- "BEV"
      fcm <- calc_fuel_cost_mile(
        pass_transpo,
        mode_1, .aeo_scenario, mpe, ELEC_FUEL_COST_KWH
      )

      av_vmt <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "VMT",
        calc_vmt(
          .scenario, pass_transpo,
          mode, stock, var, fcm,
          .aeo_scenario, .transit_avo, .transit_rider_pct,
          .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price,
          .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change,
          .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change,
          .job_access_pct_change, .transit_dist_pct_change,
          .comb_5d_impact_pct_change, .telework_pct
        )
      )

      av_dir_ghg <- tibble::tibble(
        type = type, scenario = .scenario, mode = mode,
        class = class, ctu = .ctu,
        output = "DIR-GHG",
        calc_ghg_direct(
          av_vmt,
          pass_transpo,
          mode_1, .electric_scenario,
          .aeo_scenario, mpe, .av_pct
        )
      )

      .av_fuel_type <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "ELEC",
        calc_fuel_use(
          av_vmt, pass_transpo,
          mode_1, .electric_scenario, .aeo_scenario, mpe, .av_pct
        )
      )

      av_emb_ghg <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "INDIR-GHG",
        calc_ghg_embodied(
          pass_transpo,
          mode,
          "AVSales", "BEV-EMB"
        )
      )

      av_cost <- tibble::tibble(
        type = type, scenario = .scenario,
        mode = mode, class = class,
        ctu = .ctu, output = "COST",
        calc_cost(
          av_vmt,
          cost_factors,
          mode_1, "BEVPrice", 1
        )
      )

      # Add the AV data
      out_sum <- dplyr::bind_rows(
        out_sum, av_vmt,
        av_dir_ghg, .av_fuel_type,
        av_emb_ghg, av_cost
      )
    }
  }

  # Freight -------------------------------
  # (measured in ton-miles NOT miles)
  type <- "F"
  # For all freight, var = TMT
  var <- "TMT"



  ## Heavy truck (CUT) ----
  mode <- "CUT"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(
    freight_transpo, mode,
    .aeo_scenario, mpg, CI_FUEL_COST_GAL
  )

  ci_vmt <- tibble::tibble(
    type = type,
    scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "TVMT",
    calc_vmt(
      .scenario, freight_transpo,
      mode, stock, var, fcm,
      .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price,
      .parking_price, .drs_pct, .av_pct, .freight_vmt_fee,
      .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change
    )
  )

  ci_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(
      ci_vmt, freight_transpo,
      mode, "CUTCI", .aeo_scenario, mpg
    )
  )


  ### Heavy  battery electric -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"
  bev_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "TVMT",
    calc_vmt(
      .scenario, freight_transpo, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    )
  )

  bev_ghg <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu,
    output = "DIR-GHG",
    calc_ghg_direct(
      bev_vmt,
      freight_transpo, mode,
      .electric_scenario, .aeo_scenario, mpe
    )
  )

  # Add the CUT data
  out_sum <- dplyr::bind_rows(
    out_sum, ci_vmt, ci_ghg,
    bev_vmt, bev_ghg
  )

  ## Medium truck (SUT) -----
  mode <- "SUT"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(
    freight_transpo, mode,
    .aeo_scenario, mpg, CI_FUEL_COST_GAL
  )

  ci_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "TVMT",
    calc_vmt(
      .scenario, freight_transpo, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct,
      .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    )
  )

  ci_ghg <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu,
    output = "DIR-GHG",
    calc_ghg_direct(
      ci_vmt,
      freight_transpo, mode,
      "SUTCI", .aeo_scenario, mpg
    )
  )


  ### Medium truck battery electric -----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"
  bev_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "TVMT",
    calc_vmt(
      .scenario, freight_transpo, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo,
      .transit_rider_pct, .vmt_fee, .payd_fee, .gas_tax,
      .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change, .comb_5d_impact_pct_change
    )
  )


  bev_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(
      bev_vmt, freight_transpo,
      mode, .electric_scenario, .aeo_scenario, mpe
    )
  )

  # Add the SUT data
  out_sum <- dplyr::bind_rows(
    out_sum, ci_vmt,
    ci_ghg, bev_vmt, bev_ghg
  )

  ## Freight Rail -----
  mode <- "FR"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  ci_vmt <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "TVMT",
    calc_vmt(
      .scenario, freight_transpo,
      mode, stock, var, fcm,
      .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price,
      .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change,
      .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change, .comb_5d_impact_pct_change
    )
  )

  ci_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(
      ci_vmt,
      freight_transpo,
      mode, "RCI", .aeo_scenario, mpg
    )
  )


  ### Freight rail battery electric ------
  stock <- "EVStock"
  mpe <- "EVElec"
  class <- "EV"
  ev_vmt <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "TVMT",
    calc_vmt(
      .scenario, freight_transpo,
      mode, stock, var, fcm,
      .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price,
      .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change,
      .emp_dens_pct_change, .land_use_pct_change, .intersection_design_pct_change,
      .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change
    )
  )

  ev_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(
      bev_vmt,
      freight_transpo, mode,
      .electric_scenario, .aeo_scenario, mpe
    )
  )

  # Add the FR data
  out_sum <- dplyr::bind_rows(
    out_sum, ci_vmt, ci_ghg,
    ev_vmt, ev_ghg
  )

  ## Multimodal -----
  mode <- "MM"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  ci_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu,
    output = "TVMT",
    calc_vmt(
      .scenario, freight_transpo, mode,
      stock, var, fcm, .aeo_scenario,
      .transit_avo, .transit_rider_pct, .vmt_fee, .payd_fee,
      .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    )
  )

  ci_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class,
    ctu = .ctu, output = "DIR-GHG",
    calc_ghg_direct(
      ci_vmt,
      freight_transpo,
      mode, "MMCI", .aeo_scenario, mpg
    )
  )


  ### Multimodal battery electric-----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"
  bev_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "TVMT",
    calc_vmt(
      .scenario, freight_transpo, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct,
      .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    )
  )

  bev_ghg <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu,
    output = "DIR-GHG",
    calc_ghg_direct(
      bev_vmt,
      freight_transpo, mode,
      .electric_scenario, .aeo_scenario, mpe
    )
  )

  # Add the MM data
  out_sum <- dplyr::bind_rows(out_sum, ci_vmt, ci_ghg, bev_vmt, bev_ghg)

  ## Air------
  mode <- "AIR"

  ## SI
  stock <- "SIStock"
  mpg <- "SIMPG"
  class <- "SI"

  si_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu, output = "TVMT",
    calc_vmt(
      .scenario, freight_transpo, mode,
      stock, var, fcm, .aeo_scenario, .transit_avo,
      .transit_rider_pct, .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price,
      .drs_pct, .av_pct, .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change,
      .land_use_pct_change, .intersection_design_pct_change, .job_access_pct_change,
      .transit_dist_pct_change, .comb_5d_impact_pct_change
    )
  )

  si_ghg <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    class = class, ctu = .ctu,
    output = "DIR-GHG",
    calc_ghg_direct(
      si_vmt, freight_transpo, mode,
      "ASI",
      .aeo_scenario, mpg
    )
  )

  # Add the AIR data
  out_sum <- dplyr::bind_rows(out_sum, si_vmt, si_ghg)

  ## Water ------
  mode <- "WAT"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  ci_vmt <- tibble::tibble(
    type = type, scenario = .scenario, mode = mode,
    lass = class, ctu = .ctu, output = "TVMT",
    calc_vmt(
      .scenario, freight_transpo, mode, stock,
      var, fcm, .aeo_scenario, .transit_avo, .transit_rider_pct,
      .vmt_fee, .payd_fee, .gas_tax, .cong_price, .parking_price, .drs_pct, .av_pct,
      .freight_vmt_fee, .pop_dens_pct_change, .emp_dens_pct_change, .land_use_pct_change,
      .intersection_design_pct_change, .job_access_pct_change, .transit_dist_pct_change,
      .comb_5d_impact_pct_change
    )
  )

  ci_ghg <- tibble::tibble(
    type = type, scenario = .scenario,
    mode = mode, class = class, ctu = .ctu,
    output = "DIR-GHG",
    calc_ghg_direct(
      ci_vmt, freight_transpo,
      mode, "WCI", .aeo_scenario, mpg
    )
  )

  # Add the WAT data
  out_sum <- dplyr::bind_rows(out_sum, ci_vmt, ci_ghg)

  # Return final ------
  return(out_sum)
}
