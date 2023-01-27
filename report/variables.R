variables <- rbind(

  # baseline
  data.frame(var = "population",
             desc = "Population"),
  data.frame(var = "households",
             desc = "Households"),
  data.frame(var = "jobs",
             desc = "Jobs"),

  # residential
  data.frame(var = "single_family_units",
             desc = "Number of Single Family Units"),
  data.frame(var = "single_family_average_floor_area_sqft_ctu",
             desc = "Single Family Average Floor Area (ft<sup>2</sup>)"),
  data.frame(var = "single_family_average_floor_area_sqft_county",
             desc = "Single Family Average Floor Area for the County (ft<sup>2</sup>)"),
  data.frame(var = "multifamily_units",
             desc = "Number of Multifamily Units"),
  data.frame(var = "multifamily_average_floor_area_sqft_ctu",
             desc = "Multifamily Average Floor Area (ft<sup>2</sup>)"),
  data.frame(var = "multifamily_average_floor_area_sqft_county",
             desc = "Multifamily Average Floor Area for the County (ft<sup>2</sup>)"),

  # commercial_industrial
  data.frame(var = "commercial_jobs",
             desc = "Commercial Jobs"),
  data.frame(var = "industrial_jobs",
             desc = "Industrial Jobs"),
  data.frame(var = "commercial_workers_county",
             desc = "Commercial Jobs"),
  data.frame(var = "industrial_workers_county",
             desc = "Industrial Jobs"),

  # enegy
  data.frame(var = "elec_emis_factor_kg_mwh",
             desc = "Emissions Factor for Electricity (kg/MWh)"),
  data.frame(var = "ng_emis_factor_kg_therm",
             desc = "Emissions Factor for Natural Gas (kg/therm)"),

  # energy baseline (residential, electricity)
  data.frame(var = "residential_mwh",
             desc = "Residential Electricity (MWh)"),
  data.frame(var = "residential_kwh_per_floor_area",
             desc = "Residential Electricity per Floor Area (KWh/ft<sup>2</sup>)"),
  data.frame(var = "residential_mwh_per_households",
             desc = "Residential Electricity per Household (MWh/household)"),
  data.frame(var = "residential_elec_emis_t_co2e",
             desc = "Residential Electricity Emissions (tonnes CO<sub>2</sub>e)"),

  # energy baseline (residential, natural gas)
  data.frame(var = "residential_therms",
             desc = "Residential Natural Gas (Therms)"),
  data.frame(var = "residential_ng_therms",
             desc = "Residential Natural Gas (Therms)"),
  data.frame(var = "residential_therms_per_floor_area",
             desc = "Residential Therms per Floor Area (therms/ft<sup>2</sup>)"),
  data.frame(var = "residential_therms_per_households",
             desc = "Residential Therms per Household (therms/household)"),
  data.frame(var = "residential_ng_emis_t_co2e",
             desc = "Residential Natural Gas Emissions (tonnes CO<sub>2</sub>e)"),

  # energy forecast (residential, electricity)
  data.frame(var = "total_residential_kwh_forecast",
             desc = "Residential Electricity - Forecast (MWh)"),
  data.frame(var = "residential_electricity_emissions_kg_co",
             desc = "Residential Electricity Emissions (kg CO<sub>2</sub>e)"),

  # energy forecast (residential, natural_gas)
  data.frame(var = "total_residential_therms_forecast",
             desc = "Residential Natural Gas - Forecast (Therms)"),
  data.frame(var = "residential_natural_gas_emissions_kg_co",
             desc = "Residential Natural Gas Emissions (kg CO<sub>2</sub>e)"),

  # energy baseline (commercial, electricity)
  data.frame(var = "commercial_mwh",
             desc = "Commercial Electricity (MWh)"),
  data.frame(var = "commercial_mwh_per_worker",
             desc = "Commercial Electricity per Worker (MWh/worker)"),
  data.frame(var = "commercial_elec_kwh_per_worker",
             desc = "Commercial Electricity per Worker (KWh/worker)"),
  data.frame(var = "commercial_elec_emis_t_co2e",
             desc = "Commercial Electricity Emissions (tonnes CO<sub>2</sub>e))"),

  # energy baseline (commercial, natural_gas)
  data.frame(var = "commercial_therms",
             desc = "Commercial Natural Gas (Therms)"),
  data.frame(var = "commercial_ng_therms",
             desc = "Commercial Natural Gas (Therms)"),
  data.frame(var = "commercial_ng_therms_per_worker",
             desc = "Commercial Natural Gas per Worker (therms/worker)"),
  data.frame(var = "commercial_therm_per_worker",
             desc = "Commercial Natural Gas per Worker (therms/worker)"),
  data.frame(var = "commercial_ng_emis_t_co2e",
             desc = "Commercial Natural Gas Emissions (tonnes CO<sub>2</sub>e))"),

  # energy baseline (industrial, electricity)
  data.frame(var = "industrial_mwh",
             desc = "Industrial Electricity (MWh)"),
  data.frame(var = "industrial_elec_kwh_per_worker",
             desc = "Industrial Electricity per Worker (KWh/worker)"),
  data.frame(var = "industrial_mwh_per_worker",
             desc = "Industrial Electricity per Worker (MWh/worker)"),
  data.frame(var = "industrial_elec_emis_t_co2e",
             desc = "Industrial Electricity Emissions (tonnes CO<sub>2</sub>e))"),

  # energy baseline (industrial, natural_gas)
  data.frame(var = "industrial_therms",
             desc = "Industrial Natural Gas (Therms)"),
  data.frame(var = "industrial_ng_therms",
             desc = "Industrial Natural Gas (Therms)"),
  data.frame(var = "industrial_ng_therms_per_worker",
             desc = "Industrial Natural Gas per Worker (therms/worker)"),
  data.frame(var = "industrial_therm_per_worker",
             desc = "Industrial Natural Gas per Worker (therms/worker)"),
  data.frame(var = "industrial_ng_emis_t_co2e",
             desc = "Industrial Natural Gas Emissions (tonnes CO<sub>2</sub>e))"),

  # energy emissions totals
  data.frame(var = "total_residential_emissions",
             desc = "Total Residential Emissions (tonnes CO<sub>2</sub>e)"),
  data.frame(var = "total_industrial_commercial_emissions",
             desc = "Total Industrial Emissions (tonnes CO<sub>2</sub>e)")
)
