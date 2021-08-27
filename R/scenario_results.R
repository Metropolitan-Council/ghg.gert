#' @title  Main function to call other functions for determining VMT,
#'      direct GHG, indirect GHG, and costs
#'
#' @param e_scen electricity scenario
#' @param aeo_scen selected EIA Annual Energy Outlook scenario
#' @param ch_ctu chosen CTU
#' @param drs_fuel input dynamic ride sharing (DRS) fuel type. Default is `0`.
#' @param av_fuel input AV fuel type
#' @param bau_summary input of BAU data for calculations in MIT scenario
#' @inheritParams calc_vmt
#'
#' @return
#' @export
#' @importFrom dplyr filter select case_when across bind_rows cur_column
#' @importFrom tidyselect all_of
#' @importFrom tibble tibble
#'
scenario_results <- function(scen = "BAU",
                             e_scen = "ER",
                             aeo_scen = "REF",
                             ch_ctu = "",
                             t_avo = 0,
                             t_rider = 0,
                             vmt = 0,
                             payd = 0,
                             gas = 0,
                             park = 0,
                             cong = 0,
                             fvmt = 0,
                             drs = 0,
                             av_pct = 0,
                             drs_fuel = "",
                             av_fuel = "",
                             pop_dens = 0,
                             emp_dens = 0,
                             diverse = 0,
                             design = 0,
                             job_access = 0,
                             trans_dist = 0,
                             comb_5d_impact_dr = 0,
                             telework = 0,
                             bau_summary = 0) {
  # If the user has specified DRS, then reduce the PMT for non-DRS trips
  if (drs > 0) {
    pass_transpo <- pass_transpo %>%
      dplyr::mutate(dplyr::across(all_of(YRS), ~ dplyr::case_when(
        ((mode == "PLDV") & var == "PMT") ~ .x *
          dplyr::case_when(
            drs > 0 ~ (1 - pass_transpo %>% dplyr::filter(var == "DRSShare") %>%
                         dplyr::select(tidyselect::cur_column()) %>%
                         as.numeric() * drs / 100),
            TRUE ~ 1
          ),
        TRUE ~ .x
      )))
  }

  type <- "P"
  #### For all passenger modes, variable = PMT ####
  var <- "PMT"
  # PLDV by fuel and CTU
  mode <- "PLDV"
  # Calculate aggregate GHG in kt CO2 by year
  ## Gasoline
  stock <- "SIStock"
  mpg <- "SIMPG"
  class <- "SI"

  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(
    pass_transpo,
    mode,
    aeo_scen,
    mpg,
    SI_FUEL_COST_GAL
  )

  si_vmt <- tibble::tibble(
    type = type,
    scenario = scen,
    mode = mode,
    class = class,
    ctu = ch_ctu,
    output = "VMT",
    calc_vmt(
      scen,
      pass_transpo,
      mode,
      stock,
      var,
      fcm,
      aeo_scen,
      t_avo,
      t_rider,
      vmt, payd,
      gas,
      cong,
      park,
      drs,
      av,
      fvmt,
      pop_dens,
      emp_dens,
      diverse,
      design,
      job_access,
      trans_dist,
      comb_5d_impact_dr,
      telework
    )
  )

  si_dir_ghg <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu, output = "DIR-GHG",
    calc_ghg_direct(
      si_vmt, pass_transpo, mode, "SI",
      aeo_scen, mpg
    )
  )

  si_fuel <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu, output = "PETRO",
    calc_fuel(
      si_vmt, pass_transpo, mode, "SI",
      aeo_scen, mpg
    )
  )

  si_emb_ghg <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu,
    output = "INDIR-GHG",
    calc_ghg_embodied(
      pass_transpo, mode,
      "SISales", "SI-EMB"
    )
  )

  si_cost <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "COST",
    calc_cost(si_vmt, cost_factors, mode, "SIPrice")
  )

  # Diesel----
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"

  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(pass_transpo, mode, aeo_scen, mpg, CI_FUEL_COST_GAL)

  ci_vmt <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class, ctu = ch_ctu,
    output = "VMT", calc_vmt(
      scen, pass_transpo,
      mode, stock, var, fcm,
      aeo_scen, t_avo, t_rider,
      vmt, payd, gas, cong,
      park, drs, av, fvmt,
      pop_dens, emp_dens,
      diverse, design,
      job_access, trans_dist,
      comb_5d_impact_dr, telework
    )
  )

  ci_dir_ghg <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "DIR-GHG",
    calc_ghg_direct(
      ci_vmt, pass_transpo,
      mode, "CI", aeo_scen, mpg
    )
  )

  ci_fuel <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "PETRO",
    calc_fuel(
      ci_vmt, pass_transpo,
      mode, "CI", aeo_scen, mpg
    )
  )

  ci_emb_ghg <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "INDIR-GHG",
    calc_ghg_embodied(
      pass_transpo, mode,
      "CISales", "CI-EMB"
    )
  )

  ci_cost <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "COST",
    calc_cost(ci_vmt, cost_factors, mode, "CIPrice")
  )

  ## HEV----
  stock <- "HEVStock"
  mpg <- "HEVMPG"
  class <- "HEV"

  # Calculate a fuel cost per mile rather than per gallon

  fcm <- calc_fuel_cost_mile(pass_transpo, mode, aeo_scen, mpg, SI_FUEL_COST_GAL)

  hev_vmt <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu, output = "VMT",
    calc_vmt(
      scen, pass_transpo, mode, stock,
      var, fcm, aeo_scen, t_avo, t_rider,
      vmt, payd, gas, cong, park, drs, av,
      fvmt, pop_dens, emp_dens, diverse,
      design, job_access, trans_dist,
      comb_5d_impact_dr, telework
    )
  )

  hev_dir_ghg <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "DIR-GHG",
    calc_ghg_direct(
      hev_vmt, pass_transpo,
      mode, "SI", aeo_scen, mpg
    )
  )

  hev_fuel <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "PETRO",
    calc_fuel(
      hev_vmt, pass_transpo,
      mode, "SI", aeo_scen, mpg
    )
  )

  hev_emb_ghg <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "INDIR-GHG",
    calc_ghg_embodied(pass_transpo, mode, "HEVSales", "HEV-EMB")
  )

  hev_cost <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "COST",
    calc_cost(
      hev_vmt, cost_factors, mode,
      "HEVPrice"
    )
  )

  ## PHEV-----
  ### Eqn: (PMT in 1000 mi) x Pr(stock by fuel) x ((Pr(Elec) x elec consumption (MWh per 1000 mi) x GHG per elec) + ((1 - Pr(Elec)) x fuel consumption (per 1000 mi) x GHG per fuel)

  stock <- "PHEVStock"
  mpg <- "PHEVMPG"
  mpe <- "PHEVElec"
  class <- "PHEV"

  # Calculate a fuel cost per mile rather than per gallon

  fcm <- calc_fuel_cost_mile(pass_transpo, mode, aeo_scen, mpg, SI_FUEL_COST_GAL)
  phev_vmtg <- calc_vmt(
    scen, pass_transpo, mode, stock, var,
    fcm, aeo_scen, t_avo, t_rider, vmt, payd,
    gas, cong, park, drs, av, fvmt, pop_dens,
    emp_dens, diverse, design, job_access,
    trans_dist, comb_5d_impact_dr,
    telework, 1
  ) * (1 - pass_transpo %>%
         dplyr::filter(mode == mode, var == "PHEVPr") %>%
         dplyr::select(all_of(YRS)))

  fcm <- calc_fuel_cost_mile(pass_transpo, mode, aeo_scen, mpe, ELEC_FUEL_COST_KWH)
  phev_vmte <- calc_vmt(
    scen, pass_transpo, mode, stock, var, fcm,
    aeo_scen, t_avo, t_rider, vmt, payd, gas, cong,
    park, drs, av, fvmt, pop_dens, emp_dens, diverse,
    design, job_access, trans_dist, comb_5d_impact_dr,
    telework
  ) * pass_transpo %>%
    dplyr::filter(mode == mode, var == "PHEVPr") %>%
    dplyr::select(all_of(YRS))

  phev_vmt <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu, output = "VMT",
    phev_vmtg + phev_vmte
  )

  phev_ghgg <- calc_ghg_direct(phev_vmtg, pass_transpo, mode, "SI", aeo_scen, mpg)
  phev_fuelg <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu, output = "PETRO",
    calc_fuel(
      phev_vmtg, pass_transpo, mode, "SI",
      aeo_scen, mpg
    )
  )

  phev_ghge <- calc_ghg_direct(
    phev_vmte, pass_transpo, mode, e_scen,
    aeo_scen, mpe
  )
  phev_dir_ghg <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "DIR-GHG",
    phev_ghgg + phev_ghge
  )

  phev_fuele <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "ELEC",
    calc_fuel(
      phev_vmte, pass_transpo,
      mode, e_scen, aeo_scen,
      mpe
    )
  )

  phev_emb_ghg <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "INDIR-GHG",
    calc_ghg_embodied(
      pass_transpo,
      mode, "PHEVSales", "PHEV-EMB"
    )
  )

  phev_cost <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "COST",
    calc_cost(
      phev_vmt, cost_factors,
      mode, "PHEVPrice"
    )
  )

  ## BEV-----
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"

  fcm <- calc_fuel_cost_mile(pass_transpo, mode, aeo_scen, mpe, ELEC_FUEL_COST_KWH)

  bev_vmt <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu, output = "VMT",
    calc_vmt(
      scen, pass_transpo, mode,
      stock, var, fcm, aeo_scen,
      t_avo, t_rider, vmt, payd,
      gas, cong, park, drs, av,
      fvmt, pop_dens, emp_dens,
      diverse, design, job_access,
      trans_dist, comb_5d_impact_dr, telework
    )
  )


  bev_dir_ghg <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "DIR-GHG",
    calc_ghg_direct(
      bev_vmt, pass_transpo,
      mode, e_scen, aeo_scen, mpe
    )
  )


  bev_fuel <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "ELEC",
    calc_fuel(
      bev_vmt, pass_transpo,
      mode, e_scen, aeo_scen, mpe
    )
  )

  bev_emb_ghg <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "INDIR-GHG",
    calc_ghg_embodied(
      pass_transpo, mode,
      "BEVSales", "BEV-EMB"
    )
  )

  bev_cost <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class,
    ctu = ch_ctu, output = "COST",
    calc_cost(
      bev_vmt, cost_factors,
      mode, "BEVPrice"
    )
  )

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
  fcm <- calc_fuel_cost_mile(pass_transpo, mode, aeo_scen, mpg, SI_FUEL_COST_GAL)

  # Bus Urban
  mode <- "BU"

  ## Biodiesel
  stock <- "BCIStock"
  mpg <- "BCIMPG"
  class <- "BCI"

  ci_vmt <- tibble::tibble(
    type = type,
    scenario = scen,
    mode = mode,
    class = class, ctu = ch_ctu, output = "VMT",
    calc_vmt(
      scen, pass_transpo, mode, stock, var,
      fcm, aeo_scen, t_avo, t_rider, vmt,
      payd, gas, cong, park, drs, av, fvmt,
      pop_dens, emp_dens, diverse, design,
      job_access, trans_dist, comb_5d_impact_dr
    )
  )

  ci_ghg <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu, output = "DIR-GHG",
    calc_ghg_direct(
      ci_vmt, pass_transpo, mode,
      "CI", aeo_scen, mpg
    )
  )

  ci_fuel <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu, output = "PETRO",
    calc_fuel(
      ci_vmt, pass_transpo, mode, "CI",
      aeo_scen, mpg
    )
  )

  ci_emb_ghg <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu,
    output = "INDIR-GHG",
    calc_ghg_embodied(
      pass_transpo,
      mode, "BCISales",
      "BU-BCI-EMB", class, t_avo,
      ci_vmt, bau_summary
    )
  )

  ci_cost <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu, output = "COST",
    calc_cost(ci_vmt, cost_factors, mode, "BCIPrice")
  )

  ## HEV
  stock <- "HEVStock"
  mpg <- "HEVMPG"
  class <- "HEV"

  hev_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "VMT", calc_vmt(scen, pass_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))

  hev_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(hev_vmt, pass_transpo, mode, "CI", aeo_scen, mpg))

  hev_fuel <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "PETRO", calc_fuel(hev_vmt, pass_transpo, mode, "CI", aeo_scen, mpg))

  hev_emb_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "INDIR-GHG", calc_ghg_embodied(pass_transpo, mode, "HEVSales", "BU-HEV-EMB", class, t_avo, hev_vmt, bau_summary))

  hev_cost <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "COST", calc_cost(hev_vmt, cost_factors, mode, "HEVPrice"))

  ## BEV
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"

  bev_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "VMT", calc_vmt(scen, pass_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))

  bev_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(bev_vmt, pass_transpo, mode, e_scen, aeo_scen, mpe))

  bev_fuel <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "ELEC", calc_fuel(bev_vmt, pass_transpo, mode, e_scen, aeo_scen, mpe))

  bev_emb_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "INDIR-GHG", calc_ghg_embodied(pass_transpo, mode, "BEVSales", "BU-BEV-EMB", class, t_avo, bev_vmt, bau_summary))

  bev_cost <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "COST", calc_cost(bev_vmt, cost_factors, mode, "BEVPrice"))

  # Add the BU data
  out_sum <- dplyr::bind_rows(out_sum, ci_vmt, ci_ghg, ci_fuel, ci_emb_ghg, ci_cost, hev_vmt, hev_ghg, hev_fuel, hev_emb_ghg, hev_cost, bev_vmt, bev_ghg, bev_fuel, bev_emb_ghg, bev_cost)

  # Bus Rapid Transit----
  ## BCI
  mode <- "BRT"
  stock <- "BCIStock"
  mpg <- "BCIMPG"
  class <- "BCI"

  ci_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "VMT", calc_vmt(scen, pass_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))

  ci_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(ci_vmt, pass_transpo, mode, "CI", aeo_scen, mpg))

  ci_fuel <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "PETRO", calc_fuel(ci_vmt, pass_transpo, mode, "CI", aeo_scen, mpg))

  ci_emb_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "INDIR-GHG", calc_ghg_embodied(pass_transpo, mode, "BCISales", "BU-BCI-EMB", class, t_avo, ci_vmt, bau_summary))

  ci_cost <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "COST", calc_cost(ci_vmt, cost_factors, mode, "BCIPrice"))

  ## HEV
  stock <- "HEVStock"
  mpg <- "HEVMPG"
  class <- "HEV"

  hev_vmt <- tibble::tibble(
    type = type,
    scenario = scen,
    mode = mode, class = class, ctu = ch_ctu, output = "VMT",
    calc_vmt(scen, pass_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr)
  )

  hev_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(hev_vmt, pass_transpo, mode, "CI", aeo_scen, mpg))

  hev_fuel <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "PETRO", calc_fuel(hev_vmt, pass_transpo, mode, "CI", aeo_scen, mpg))

  hev_emb_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "INDIR-GHG", calc_ghg_embodied(pass_transpo, mode, "HEVSales", "BU-HEV-EMB", class, t_avo, hev_vmt, bau_summary))

  hev_cost <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "COST", calc_cost(hev_vmt, cost_factors, mode, "HEVPrice"))

  ## BEV
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"

  bev_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "VMT", calc_vmt(scen, pass_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))

  bev_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(bev_vmt, pass_transpo, mode, e_scen, aeo_scen, mpe))

  bev_fuel <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "ELEC", calc_fuel(bev_vmt, pass_transpo, mode, e_scen, aeo_scen, mpe))

  bev_emb_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "INDIR-GHG", calc_ghg_embodied(pass_transpo, mode, "BEVSales", "BU-BEV-EMB", class, t_avo, bev_vmt, bau_summary))

  bev_cost <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "COST", calc_cost(bev_vmt, cost_factors, mode, "BEVPrice"))

  # Add the BRT data
  out_sum <- dplyr::bind_rows(out_sum, ci_vmt, ci_ghg, ci_fuel, ci_emb_ghg, ci_cost, hev_vmt, hev_ghg, hev_fuel, hev_emb_ghg, hev_cost, bev_vmt, bev_ghg, bev_fuel, bev_emb_ghg, bev_cost)

  # Rail Urban-----
  ## EV
  mode <- "RU"
  stock <- "EVStock"
  mpe <- "EVElec"
  class <- "EV"

  ev_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "VMT", calc_vmt(scen, pass_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))

  ev_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(bev_vmt, pass_transpo, mode, e_scen, aeo_scen, mpe))

  ev_fuel <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "ELEC", calc_fuel(bev_vmt, pass_transpo, mode, e_scen, aeo_scen, mpe))

  ev_cost <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "COST", calc_cost(ev_vmt, cost_factors, mode, "EVPrice"))

  # Add the RU data
  out_sum <- dplyr::bind_rows(out_sum, ev_vmt, ev_ghg, ev_fuel, ev_cost)

  ## Rail Interurban-----
  mode <- "RI"

  ## BCI
  stock <- "BCIStock"
  mpg <- "BCIMPG"
  class <- "BCI"
  ci_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "VMT", calc_vmt(scen, pass_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))

  ci_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(ci_vmt, pass_transpo, mode, "BCI", aeo_scen, mpg))

  ci_fuel <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "PETRO", calc_fuel(ci_vmt, pass_transpo, mode, "BCI", aeo_scen, mpg))

  ci_cost <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "COST", calc_cost(ci_vmt, cost_factors, mode, "BCIPrice"))

  ## EV
  stock <- "EVStock"
  mpe <- "EVElec"
  class <- "EV"
  ev_vmt <- tibble::tibble(
    type = type, scenario = scen, mode = mode, class = class,
    ctu = ch_ctu, output = "VMT", calc_vmt(scen, pass_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr)
  )

  ev_ghg <- tibble::tibble(
    type = type, scenario = scen, mode = mode, class = class,
    ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(bev_vmt, pass_transpo, mode, e_scen, aeo_scen, mpe)
  )

  ev_fuel <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu, output = "ELEC",
    calc_fuel(bev_vmt, pass_transpo, mode, e_scen, aeo_scen, mpe)
  )

  ev_cost <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu, output = "COST",
    calc_cost(ev_vmt, cost_factors, mode, "EVPrice")
  )

  # Add the RI data
  out_sum <- dplyr::bind_rows(out_sum, ci_vmt, ci_ghg, ci_fuel, ci_cost, ev_vmt, ev_ghg, ev_fuel, ev_cost)

  # School Bus-----
  mode <- "BS"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  ci_vmt <- tibble::tibble(
    type = type, scenario = scen, mode = mode, class = class,
    ctu = ch_ctu, output = "VMT", calc_vmt(scen, pass_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr)
  )

  ci_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(ci_vmt, pass_transpo, mode, "CI", aeo_scen, mpg))

  ci_fuel <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "PETRO", calc_fuel(ci_vmt, pass_transpo, mode, "CI", aeo_scen, mpg))

  ci_cost <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "COST", calc_cost(ci_vmt, cost_factors, mode, "CIPrice"))


  ## BEV
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"
  bev_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "VMT", calc_vmt(scen, pass_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))

  bev_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(bev_vmt, pass_transpo, mode, e_scen, aeo_scen, mpe))

  bev_fuel <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "ELEC", calc_fuel(bev_vmt, pass_transpo, mode, e_scen, aeo_scen, mpe))

  bev_cost <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "COST", calc_cost(bev_vmt, cost_factors, mode, "BEVPrice"))


  # Add the BS data
  out_sum <- dplyr::bind_rows(out_sum, ci_vmt, ci_ghg, ci_fuel, ci_cost, bev_vmt, bev_ghg, bev_cost)

  # Active Modes-----
  ### Walk -----
  mode <- "WALK"
  stock <- ""
  class <- "WALK"
  walk_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "VMT", calc_vmt(scen, pass_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))

  ### Bike -----
  mode <- "BIKE"
  stock <- ""
  class <- "BIKE"

  bike_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "VMT", calc_vmt(scen, pass_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))


  # Add the ACTIVE data
  out_sum <- dplyr::bind_rows(out_sum, walk_vmt, bike_vmt)

  # Dynamic Ride Sharing
  mode <- "DRS"
  # Use PLDV features in some cases
  mode_1 <- "PLDV"

  # If DRS is included, then perform calculations depending if fuel is BEV, HEV, or PHEV
  if (scen != "BAU" & drs > 0) {
    # Calculate DRS sales in each year
    drs_sales <- tibble::tibble(mode = mode, var = "DRSSales", ctu = ch_ctu, calc_drs_sales(pass_transpo, drs))
    pass_transpo <- dplyr::bind_rows(pass_transpo, drs_sales)

    if (drs_fuel == "HEV") {
      ## HEV
      stock <- "DRSStock"
      mpg <- "HEVMPG"
      class <- "HEV"
      fcm <- calc_fuel_cost_mile(pass_transpo, mode, aeo_scen, mpg, SI_FUEL_COST_GAL)
      drs_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "VMT", calc_drs_vmt(pass_transpo, drs, class, fcm, vmt, payd, gas, cong, park, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))


      drs_dir_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(drs_vmt, pass_transpo, mode_1, "SI", aeo_scen, mpg, 1))
      drs_fuel <- tibble::tibble(
        type = type, scenario = scen, mode = mode,
        class = class, ctu = ch_ctu, output = "PETRO",
        calc_fuel(drs_vmt, pass_transpo, mode_1, "SI", aeo_scen, mpg, 1)
      )

      temp <- calc_ghg_embodied(pass_transpo, mode, "DRSSales", "HEV-EMB") %>% as.numeric()
      out_sum <- out_sum %>% dplyr::mutate(dplyr::across(all_of(YRS), ~ dplyr::case_when(
        (mode == mode_1 & class == class & output == "INDIR-GHG") ~ .x + temp %>% as.numeric(),
        TRUE ~ .x
      )))

      drs_cost <- tibble::tibble(
        type = type, scenario = scen, mode = mode,
        class = class, ctu = ch_ctu, output = "COST", calc_cost(drs_vmt, cost_factors, mode_1, "HEVPrice", 1)
      )

      # Add the DRS data
      out_sum <- dplyr::bind_rows(out_sum, drs_vmt, drs_dir_ghg, drs_fuel, drs_cost)
    } else if (drs_fuel == "PHEV") {
      ## PHEV
      stock <- "DRSStock"
      mpg <- "PHEVMPG"
      mpe <- "PHEVElec"
      class <- "PHEV"

      # Don't apply the gas factors, etc. to PHEV for DRS
      phev_vmtg <- calc_drs_vmt(pass_transpo, drs, class, fcm, vmt, payd, gas, cong, park, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr) * (1 - pass_transpo %>% dplyr::filter(mode == mode, var == "PHEVPr") %>% dplyr::select(all_of(YRS)))

      phev_vmte <- calc_drs_vmt(pass_transpo, drs, class, fcm, vmt, payd, gas, cong, park, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr) * pass_transpo %>%
        dplyr::filter(mode == mode, var == "PHEVPr") %>%
        dplyr::select(all_of(YRS))

      drs_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "VMT", phev_vmtg + phev_vmte)

      phev_ghgg <- calc_ghg_direct(phev_vmtg, pass_transpo, mode, "SI", aeo_scen, mpg, 1)

      phev_ghge <- calc_ghg_direct(phev_vmte, pass_transpo, mode, e_scen, aeo_scen, mpe, 1)

      drs_dir_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", phev_ghgg + phev_ghge)

      drs_fuelg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "PETRO", calc_fuel(phev_vmtg, pass_transpo, mode, "SI", aeo_scen, mpg, 1))

      drs_fuele <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "ELEC", calc_fuel(phev_vmte, pass_transpo, mode, e_scen, aeo_scen, mpe, 1))

      temp <- calc_ghg_embodied(pass_transpo, mode, "DRSSales", "PHEV-EMB") %>% as.numeric()
      out_sum <- out_sum %>% dplyr::mutate(dplyr::across(all_of(YRS), ~ dplyr::case_when(
        (mode == mode_1 & class == class & output == "INDIR-GHG") ~ .x + temp %>% as.numeric(),
        TRUE ~ .x
      )))

      drs_cost <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "COST", calc_cost(drs_vmt, cost_factors, mode_1, "PHEVPrice", 1))

      # Add the DRS data
      out_sum <- dplyr::bind_rows(
        out_sum, drs_vmt, drs_dir_ghg, drs_fuelg,
        drs_fuele, drs_cost
      )
    } else {
      ## BEV
      stock <- "DRSStock"
      mpe <- "BEVElec"
      class <- "BEV"
      drs_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "VMT", calc_drs_vmt(pass_transpo, drs, class, fcm, vmt, payd, gas, cong, park, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))

      drs_dir_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(drs_vmt, pass_transpo, mode_1, e_scen, aeo_scen, mpe, 1))

      drs_fuel <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "ELEC", calc_fuel(drs_vmt, pass_transpo, mode_1, e_scen, aeo_scen, mpe, 1))
      temp <- calc_ghg_embodied(pass_transpo, mode, "DRSSales", "BEV-EMB") %>% as.numeric()
      out_sum <- out_sum %>% dplyr::mutate(dplyr::across(all_of(YRS), ~ dplyr::case_when(
        (mode == mode_1 & class == class & output == "INDIR-GHG") ~ .x + temp %>% as.numeric(),
        TRUE ~ .x
      )))

      drs_cost <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "COST", calc_cost(drs_vmt, cost_factors, mode_1, "BEVPrice"))

      # Add the DRS data
      out_sum <- dplyr::bind_rows(out_sum, drs_vmt, drs_dir_ghg, drs_fuel, drs_cost)
    }
  }

  # AV-----
  mode <- "AV"
  # Use PLDV features in some cases
  mode_1 <- "PLDV"

  # If AV is included, then perform calculations depending if fuel type is BEV, HEV, or PHEV
  if (scen != "BAU" & av > 0) {
    # Calculate DRS sales in each year
    av_sales <- tibble::tibble(mode = mode, var = "AVSales", ctu = ch_ctu, calc_av_sales(pass_transpo, av))
    pass_transpo <- dplyr::bind_rows(pass_transpo, av_sales)

    if (av_fuel == "HEV") {
      ## HEV
      stock <- "AVStock"
      mpg <- "HEVMPG"
      class <- "HEV"

      # Calculate a fuel cost per mile rather than per gallon
      fcm <- calc_fuel_cost_mile(pass_transpo, mode_1, aeo_scen, mpg, SI_FUEL_COST_GAL, av)
      av_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "VMT", calc_vmt(scen, pass_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr, telework))

      av_dir_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(av_vmt, pass_transpo, mode_1, "SI", aeo_scen, mpg, av))

      av_fuel <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "PETRO", calc_fuel(av_vmt, pass_transpo, mode_1, "SI", aeo_scen, mpg, av))

      av_emb_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "INDIR-GHG", calc_ghg_embodied(pass_transpo, mode, "AVSales", "HEV-EMB"))

      av_cost <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "COST", calc_cost(av_vmt, cost_factors, mode_1, "HEVPrice", 1))

      # Add the AV data
      out_sum <- dplyr::bind_rows(out_sum, av_vmt, av_dir_ghg, av_fuel, av_emb_ghg, av_cost)
    } else if (av_fuel == "PHEV") {
      ## PHEV
      stock <- "AVStock"
      mpg <- "PHEVMPG"
      mpe <- "PHEVElec"
      class <- "PHEV"
      # Calculate a fuel cost per mile rather than per gallon
      fcm <- calc_fuel_cost_mile(pass_transpo, mode_1, aeo_scen, mpg, SI_FUEL_COST_GAL, av)

      phev_vmtg <- calc_vmt(
        scen, pass_transpo, mode, stock,
        var, fcm, aeo_scen, t_avo, t_rider,
        vmt, payd, gas, cong, park, drs, av,
        fvmt, pop_dens, emp_dens, diverse,
        design, job_access, trans_dist,
        comb_5d_impact_dr, telework, 1
      ) * (1 - pass_transpo %>%
             dplyr::filter(mode == mode, var == "PHEVPr") %>% dplyr::select(all_of(YRS)))

      fcm <- calc_fuel_cost_mile(pass_transpo, mode, aeo_scen, mpe, ELEC_FUEL_COST_KWH)

      phev_vmte <- calc_vmt(scen, pass_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr, telework) * pass_transpo %>%
        dplyr::filter(mode == mode, var == "PHEVPr") %>%
        dplyr::select(all_of(YRS))

      av_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "VMT", phev_vmtg + phev_vmte)

      phev_ghgg <- calc_ghg_direct(phev_vmtg, pass_transpo, mode, "SI", aeo_scen, mpg, av)
      phev_ghge <- calc_ghg_direct(phev_vmtg, pass_transpo, mode, e_scen, aeo_scen, mpe, av)
      av_dir_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", phev_ghgg + phev_ghge)

      av_fuelg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "PETRO", calc_fuel(phev_vmtg, pass_transpo, mode, "SI", aeo_scen, mpg, av))

      av_fuele <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "ELEC", calc_fuel(phev_vmte, pass_transpo, mode, e_scen, aeo_scen, mpe, av))

      av_emb_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "INDIR-GHG", calc_ghg_embodied(pass_transpo, mode, "AVSales", "PHEV-EMB"))

      av_cost <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "COST", calc_cost(av_vmt, cost_factors, mode_1, "PHEVPrice", 1))
      # Add the AV data
      out_sum <- dplyr::bind_rows(out_sum, av_vmt, av_dir_ghg, av_fuelg, av_fuele, av_emb_ghg, av_cost)
    } else {
      ## BEV
      stock <- "AVStock"
      mpe <- "BEVElec"
      class <- "BEV"
      fcm <- calc_fuel_cost_mile(pass_transpo, mode_1, aeo_scen, mpe, ELEC_FUEL_COST_KWH)

      av_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "VMT", calc_vmt(scen, pass_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr, telework))

      av_dir_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(av_vmt, pass_transpo, mode_1, e_scen, aeo_scen, mpe, av))

      av_fuel <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "ELEC", calc_fuel(av_vmt, pass_transpo, mode_1, e_scen, aeo_scen, mpe, av))
      av_emb_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "INDIR-GHG", calc_ghg_embodied(pass_transpo, mode, "AVSales", "BEV-EMB"))
      av_cost <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "COST", calc_cost(av_vmt, cost_factors, mode_1, "BEVPrice", 1))

      # Add the AV data
      out_sum <- dplyr::bind_rows(out_sum, av_vmt, av_dir_ghg, av_fuel, av_emb_ghg, av_cost)
    }
  }

  #### Freight (measured in ton-miles NOT miles) #####
  type <- "F"
  # For all freight, var = TMT
  var <- "TMT"
  ## Heavy truck (CUT)
  mode <- "CUT"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(freight_transpo, mode, aeo_scen, mpg, CI_FUEL_COST_GAL)
  ci_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "TVMT", calc_vmt(scen, freight_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))

  ci_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(ci_vmt, freight_transpo, mode, "CUTCI", aeo_scen, mpg))


  ## BEV
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"
  bev_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "TVMT", calc_vmt(scen, freight_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))

  bev_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(bev_vmt, freight_transpo, mode, e_scen, aeo_scen, mpe))

  # Add the CUT data
  out_sum <- dplyr::bind_rows(out_sum, ci_vmt, ci_ghg, bev_vmt, bev_ghg)

  # Medium truck (SUT)
  mode <- "SUT"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  # Calculate a fuel cost per mile rather than per gallon
  fcm <- calc_fuel_cost_mile(freight_transpo, mode, aeo_scen, mpg, CI_FUEL_COST_GAL)
  ci_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "TVMT", calc_vmt(scen, freight_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))

  ci_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(ci_vmt, freight_transpo, mode, "SUTCI", aeo_scen, mpg))


  ## BEV
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"
  bev_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "TVMT", calc_vmt(scen, freight_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))

  bev_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(bev_vmt, freight_transpo, mode, e_scen, aeo_scen, mpe))

  # Add the SUT data
  out_sum <- dplyr::bind_rows(out_sum, ci_vmt, ci_ghg, bev_vmt, bev_ghg)

  ## Freight rail
  mode <- "FR"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  ci_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "TVMT", calc_vmt(scen, freight_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))

  ci_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(ci_vmt, freight_transpo, mode, "RCI", aeo_scen, mpg))


  ## BEV
  stock <- "EVStock"
  mpe <- "EVElec"
  class <- "EV"
  ev_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "TVMT", calc_vmt(scen, freight_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))

  ev_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(bev_vmt, freight_transpo, mode, e_scen, aeo_scen, mpe))

  # Add the FR data
  out_sum <- dplyr::bind_rows(out_sum, ci_vmt, ci_ghg, ev_vmt, ev_ghg)

  ## Multimodal
  mode <- "MM"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  ci_vmt <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "TVMT", calc_vmt(scen, freight_transpo, mode, stock, var, fcm, aeo_scen, t_avo, t_rider, vmt, payd, gas, cong, park, drs, av, fvmt, pop_dens, emp_dens, diverse, design, job_access, trans_dist, comb_5d_impact_dr))
  ci_ghg <- tibble::tibble(type = type, scenario = scen, mode = mode, class = class, ctu = ch_ctu, output = "DIR-GHG", calc_ghg_direct(ci_vmt, freight_transpo, mode, "MMCI", aeo_scen, mpg))


  ## BEV
  stock <- "BEVStock"
  mpe <- "BEVElec"
  class <- "BEV"
  bev_vmt <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu, output = "TVMT",
    calc_vmt(
      scen, freight_transpo, mode, stock,
      var, fcm, aeo_scen, t_avo, t_rider,
      vmt, payd, gas, cong, park, drs,
      av, fvmt, pop_dens, emp_dens,
      diverse, design, job_access,
      trans_dist, comb_5d_impact_dr
    )
  )

  bev_ghg <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu,
    output = "DIR-GHG",
    calc_ghg_direct(
      bev_vmt,
      freight_transpo, mode,
      e_scen, aeo_scen, mpe
    )
  )

  # Add the MM data
  out_sum <- dplyr::bind_rows(out_sum, ci_vmt, ci_ghg, bev_vmt, bev_ghg)

  ## Freight air
  mode <- "AIR"

  ## SI
  stock <- "SIStock"
  mpg <- "SIMPG"
  class <- "SI"
  si_vmt <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu, output = "TVMT",
    calc_vmt(
      scen, freight_transpo, mode,
      stock, var, fcm, aeo_scen, t_avo,
      t_rider, vmt, payd, gas, cong, park,
      drs, av, fvmt, pop_dens, emp_dens,
      diverse, design, job_access,
      trans_dist, comb_5d_impact_dr
    )
  )

  si_ghg <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    class = class, ctu = ch_ctu, output = "DIR-GHG",
    calc_ghg_direct(
      si_vmt, freight_transpo, mode, "ASI",
      aeo_scen, mpg
    )
  )

  # Add the AIR data
  out_sum <- dplyr::bind_rows(out_sum, si_vmt, si_ghg)

  ## Freight water
  mode <- "WAT"

  ## CI
  stock <- "CIStock"
  mpg <- "CIMPG"
  class <- "CI"
  ci_vmt <- tibble::tibble(
    type = type, scenario = scen, mode = mode,
    lass = class, ctu = ch_ctu, output = "TVMT",
    calc_vmt(
      scen, freight_transpo, mode, stock,
      var, fcm, aeo_scen, t_avo, t_rider,
      vmt, payd, gas, cong, park, drs, av,
      fvmt, pop_dens, emp_dens, diverse,
      design, job_access, trans_dist,
      comb_5d_impact_dr
    )
  )

  ci_ghg <- tibble::tibble(
    type = type, scenario = scen,
    mode = mode, class = class, ctu = ch_ctu,
    output = "DIR-GHG", calc_ghg_direct(ci_vmt, freight_transpo, mode, "WCI", aeo_scen, mpg)
  )

  # Add the WAT data
  out_sum <- dplyr::bind_rows(out_sum, ci_vmt, ci_ghg)

  # Return
  return(out_sum)
}
