appendix_enviro_factors <- rbind(
  data.frame(
    var = "PLDV_TRANSIT_RATIO",
    value = enviro_factors$PLDV_TRANSIT_RATIO,
    desc = "Passenger Light Duty Vehicle to Transit Ratio"
  ),
  data.frame(
    var = "SI_FUEL_COST_GAL",
    value = enviro_factors$SI_FUEL_COST_GAL,
    desc = "Fuel Cost per Gallon"
  ),
  data.frame(
    var = "CI_FUEL_COST_GAL",
    value = enviro_factors$CI_FUEL_COST_GAL,
    desc = "Fuel Cost per Gallon"
  ),
  data.frame(
    var = "ELEC_FUEL_COST_KWH",
    value = enviro_factors$ELEC_FUEL_COST_KWH,
    desc = "Fuel Cost per KWH"
  ),
  data.frame(
    var = "F_FRACT",
    value = enviro_factors$F_FRACT,
    desc = "Fraction of Fuel Cost"
  ),
  data.frame(
    var = "AUTO_COST_MI",
    value = enviro_factors$AUTO_COST_MI,
    desc = "Auto Cost per Mile"
  ),
  data.frame(
    var = "TIME_COST_MI",
    value = enviro_factors$TIME_COST_MI,
    desc = "Time Cost per Mile"
  ),
  data.frame(
    var = "F_TIME_COST_MI",
    value = enviro_factors$F_TIME_COST_MI,
    desc = "Fraction of Time Cost per Mile"
  ),
  data.frame(
    var = "INS_COST_MI",
    value = enviro_factors$INS_COST_MI,
    desc = "Insurance Cost per Mile"
  ),
  data.frame(
    var = "CONG_VMT",
    value = enviro_factors$CONG_VMT,
    desc = "Congestion VMT"
  ),
  data.frame(
    var = "BUS_AV",
    value = enviro_factors$BUS_AV,
    desc = "Bus Availability"
  ),
  data.frame(
    var = "RAIL_AV",
    value = enviro_factors$RAIL_AV,
    desc = "Rail Availability"
  ),
  data.frame(
    var = "VMT_AV",
    value = enviro_factors$VMT_AV,
    desc = "VMT Availability"
  ),
  data.frame(
    var = "EVCS_VMT",
    value = enviro_factors$EVCS_VMT,
    desc = "EVCS VMT"
  ),
  data.frame(
    var = "MPG_AV",
    value = enviro_factors$MPG_AV,
    desc = "MPG Availability"
  ),
  data.frame(
    var = "MAX_5D_DR",
    value = enviro_factors$MAX_5D_DR,
    desc = "Max 5D DR"
  ),
  data.frame(
    var = "MAX_5D_ACT",
    value = enviro_factors$MAX_5D_ACT,
    desc = "Max 5D ACT"
  ),
  data.frame(
    var = "MAX_5D_TRANS",
    value = enviro_factors$MAX_5D_TRANS,
    desc = "Max 5D Transit"
  ),
  data.frame(
    var = "MARG_TELEWORK",
    value = enviro_factors$MARG_TELEWORK,
    desc = "Margin Telework"
  ),
  data.frame(
    var = "KG_CO2E_PER_THERM_BASELINE",
    value = enviro_factors$KG_CO2E_PER_THERM_BASELINE,
    desc = "KG CO2E per Therm Baseline"
  ),
  data.frame(
    var = "KG_CO2E_PER_THERM_FORECAST",
    value = enviro_factors$KG_CO2E_PER_THERM_FORECAST,
    desc = "KG CO2E per Therm Forecast"
  ),
  data.frame(
    var = "KG_CO2E_PER_MHW_BASELINE",
    value = enviro_factors$KG_CO2E_PER_MHW_BASELINE,
    desc = "KG CO2E per MHW Baseline"
  ),
  data.frame(
    var = "KG_CO2E_PER_MHW_FORECAST",
    value = enviro_factors$KG_CO2E_PER_MHW_FORECAST,
    desc = "KG CO2E per MHW Forecast"
  ),
  data.frame(
    var = "LEED_GOLD_REDUCTION_PCT",
    value = enviro_factors$LEED_GOLD_REDUCTION_PCT,
    desc = "LEED Gold Reduction Percent"
  ),
  data.frame(
    var = "EXISTING_HOME_RETROFIT_REDUCTION_PCT",
    value = enviro_factors$EXISTING_HOME_RETROFIT_REDUCTION_PCT,
    desc = "Existing Home Retrofit Reduction Percent"
  ),
  data.frame(
    var = "EXISTING_HOME_ULTRA_RETROFIT_REDUCTION_PCT",
    value = enviro_factors$EXISTING_HOME_ULTRA_RETROFIT_REDUCTION_PCT,
    desc = "Existing Home Ultra Retrofit Reduction Percent"
  ),
  data.frame(
    var = "BEHAVIOR_CHANGE_REDUCTION_PCT",
    value = enviro_factors$BEHAVIOR_CHANGE_REDUCTION_PCT,
    desc = "Behavior Change Reduction Percent"
  ),
  data.frame(
    var = "SMART_GRID_EFFICIENCY_PCT",
    value = enviro_factors$SMART_GRID_EFFICIENCY_PCT,
    desc = "Smart Grid Efficiency Percent"
  ),
  data.frame(
    var = "THERM_TO_MWH",
    value = enviro_factors$THERM_TO_MWH,
    desc = "Therm to MWH"
  ),
  data.frame(
    var = "BOILER_TO_HEAT_PUMP_EFFICIENCY_RATIO",
    value = enviro_factors$BOILER_TO_HEAT_PUMP_EFFICIENCY_RATIO,
    desc = "Boiler to Heat Pump Efficiency Ratio"
  ),
  data.frame(
    var = "AGRI_LAND_CARBON_STOCK",
    value = enviro_factors$AGRI_LAND_CARBON_STOCK,
    desc = "Agricultural Land Carbon Stock"
  ),
  data.frame(
    var = "MAX_SOC_ACCUMULATION_UNDER_REDUCED_OR_NO_TILL_AGRI_PCT",
    value = enviro_factors$MAX_SOC_ACCUMULATION_UNDER_REDUCED_OR_NO_TILL_AGRI_PCT,
    desc = "Max SOC Accumulation under Reduced or No Till Agriculture Percent"
  ),
  data.frame(
    var = "W2W_DIESEL_EMISSIONS_FACTOR",
    value = enviro_factors$W2W_DIESEL_EMISSIONS_FACTOR,
    desc = "W2W Diesel Emissions Factor"
  ),
  data.frame(
    var = "AVOIDED_EMISSIONS_TRACTOR_USE",
    value = enviro_factors$AVOIDED_EMISSIONS_TRACTOR_USE,
    desc = "Avoided Emissions Tractor Use"
  )
)
