# list of 7 counties that make the metro area
l_metro_counties <-
  c(
    "Anoka",
    "Carver",
    "Dakota",
    "Hennepin",
    "Ramsey",
    "Scott",
    "Washington"
  )


# NAICS codes -----

naics_codes <- list(
  commerical = c(
    "NAICS 44; 722",
    "NAICS 51-55",
    "NAICS 61",
    "NAICS 62",
    "NAICS 71; 721; 81",
    "NAICS 92"
  ),
  industrial = c(
    "NAICS 21-22; 31-33; 42; 48-49",
    "NAICS 23; 56"
  ),
  led_commercial = c(
    "Accommodation and Food Services",
    "Administrative and Waste Services",
    "Agriculture, Forestry, Fishing & Hunting",
    "Arts, Entertainment, and Recreation",
    "Educational Services",
    "Finance and Insurance",
    "Health Care and Social Assistance",
    "Information",
    "Management of Companies and Enterprises",
    "Other Services, Ex. Public Admin",
    "Professional and Technical Services",
    "Public Administration",
    "Real Estate and Rental and Leasing",
    "Retail Trade",
    "Wholesale Trade"
  ),
  led_industrial = c(
    "Manufacturing",
    "Mining",
    "Utilities",
    "Transportation and Warehousing",
    "Construction"
  )
)

# waldo::compare(ghg.sp::naics_codes, naics_codes)

usethis::use_data(naics_codes, overwrite = T)
