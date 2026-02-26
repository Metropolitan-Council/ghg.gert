# global warming potential
# 100-year, accurate to AR6
# * Following  revised reporting requirements under the UNFCCC, this tool presents CO2 equivalent values based on the IPCC Sixth Assessment Report (AR6) GWP values.
# see Table 7.SM.7 in the Supplementary Materials for Chp.7 of the Climate Change 2021: The Physical Science Basis report prepared by Working Group I for the AR6 -- https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_Chapter07_SM.pdf -- for full data table of GWPs
# Zotero: ipccAR62021
gwp_list <-
  list(
    "co2" = 1,
    "ch4" = 27.9,
    "n2o" = 273,
    "cf4" = 7380,
    "hfc" = 164, # HFC-152a
    "sf6" = 24300,
    "nf3" = 17400
  )

usethis::use_data(gwp_list, overwrite=T)
