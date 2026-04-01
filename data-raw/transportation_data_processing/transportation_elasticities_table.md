# Transportation Elasticities Reference Table

## Price and Policy Elasticities (`elast` dataset)

| Elasticity | Value | Description | Source | Zotero Key | Notes |
|------------|-------|-------------|--------|------------|-------|
| `vmt_elast` | -0.34 | VMT pricing effect on VMT | INFRAS (2000), Luk (1999) for range; Hymel and Small (2015) for mean; Small and Van Dender (2007) | Not in references.bib | Range: -0.1 to -0.8 |
| `gas_elast` | -0.1066 | Gas tax effect on VMT | Goodwin, Dargay, and Hanly (2003) for range; Small (2007) for mean | Not in references.bib | Range: -0.05 to -0.17 |
| `cong_elast` | -0.10 | Congestion pricing effect on VMT | Arentze, Hofman and Timmermans (2004); PSRC (2005) | Not in references.bib | Range: -0.04 to -0.16 |
| `park_elast` | -0.07 | Parking cost effect on VMT | TRACE (1999); Litman (2019) | `traceElasticityHandbookElasticities1999a`, `litmanUnderstandingTransportDemands2021a` | Range: -0.03 to -0.17; Both found in CPRG bib |
| `freight_vmt_elast` | -0.25 | Freight vehicle pricing effect on freight VMT | Small and Winston (1999) quoted in Litman (2011) | Not in references.bib | |
| `vehicle_ownership_elast` | -0.10 | Vehicle ownership response to price changes | Not specified | | |
| `vmt_cross` | 0.13 | Cross elasticity: transit/walk/bike wrt PLDV VMT price | Litman (2019) | `litmanUnderstandingTransportDemands2021a` | URL: https://www.vtpi.org/elasticities.pdf; Found in CPRG bib |
| `park_active` | 0.03 | Parking price effect on active transportation VMT | TRACE (1999) | `traceElasticityHandbookElasticities1999a` | Found in CPRG bib |
| `park_transit` | 0.01 | Parking price effect on transit VMT | TRACE (1999) | `traceElasticityHandbookElasticities1999a` | Found in CPRG bib |

## 5D Built Environment Elasticities (`elast_5d` dataset)

### Driving Mode

| Elasticity | Value | Description | Source | Zotero Key | Notes |
|------------|-------|-------------|--------|------------|-------|
| `population_density` | -0.04 | Population density effect on driving VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | Found in CPRG bib |
| `employment_density` | -0.07 | Employment density effect on driving VMT | Ewing and Cervero (2010); Stevens (2017) | `ewingTravelBuiltEnvironment2010`, `stevensDoesCompactDevelopment2017` | Range: -0.01 to -0.07; Stevens reference found in CPRG bib |
| `diversity` | -0.09 | Land use diversity effect on driving VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | Found in CPRG bib |
| `design` | -0.12 | Intersection design effect on driving VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | Found in CPRG bib |
| `job_access` | -0.20 | Job accessibility via transit effect on driving VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | Found in CPRG bib |
| `distance` | -0.05 | Minimum distance to transit stops effect on driving VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | Found in CPRG bib |
| `combined_density` | -0.22 | Combined effect of all land use factors on driving | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | Found in CPRG bib |
| `MAX_5D_DR` | -0.25 | Maximum 5D combined effect cap for driving | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | From enviro_factors; Found in CPRG bib |

### Walking/Active Transportation Mode

| Elasticity | Value | Description | Source | Zotero Key | Notes |
|------------|-------|-------------|--------|------------|-------|
| `population_density` | 0.07 | Population density effect on walking VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | |
| `employment_density` | 0.04 | Employment density effect on walking VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | |
| `diversity` | 0.15 | Land use diversity effect on walking VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | |
| `design` | -0.06 | Intersection design effect on walking VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | |
| `job_access` | -0.06 | Job accessibility via transit effect on walking VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | |
| `distance` | 0.15 | Minimum distance to transit stops effect on walking VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | |
| `combined_density` | 0.33 | Combined effect of all land use factors on walking | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | |
| `MAX_5D_ACT` | 0.37 | Maximum 5D combined effect cap for active transport | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | From enviro_factors |

### Transit Mode

| Elasticity | Value | Description | Source | Zotero Key | Notes |
|------------|-------|-------------|--------|------------|-------|
| `population_density` | 0.07 | Population density effect on transit VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | |
| `employment_density` | 0.01 | Employment density effect on transit VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | |
| `diversity` | 0.12 | Land use diversity effect on transit VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | |
| `design` | 0.29 | Intersection design effect on transit VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | |
| `job_access` | 0.128 | Job accessibility via transit effect on transit VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | |
| `distance` | 0.29 | Minimum distance to transit stops effect on transit VMT | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | |
| `combined_density` | 0.62 | Combined effect of all land use factors on transit | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | |
| `MAX_5D_TRANS` | 0.71 | Maximum 5D combined effect cap for transit | Ewing and Cervero (2010) | `ewingTravelBuiltEnvironment2010` | From enviro_factors |

## Other Transportation Elasticities (`enviro_factors`)

| Elasticity | Value | Description | Source | Zotero Key | Notes |
|------------|-------|-------------|--------|------------|-------|
| `TRANSIT_SERVICE_ELAST` | 0.9 | Effect of transit service increase on ridership and PLDV decrease | Metro Transit SI folks (TODO) | Not in references.bib | Citation needed |
| `MARG_TELEWORK` | -0.02749 | Telework marginal effect: percent change in PMT per household | Kim et al. (2015) | Not in references.bib | -2.749/100 |

## References Missing from references.bib

The following sources are cited in the code. Some have been **found in the CPRG bibliography** (`/Users/rotenle/Documents/MetC_Locals/Interdivisional/ghg-cprg/metcouncil-cprg-ghg.bib`):

### Found in CPRG Bibliography:

1. **TRACE (1999)** - **FOUND** as `traceElasticityHandbookElasticities1999a` - Elasticity Handbook
2. **Litman (2019/2021/2024)** - **FOUND** multiple versions:
   - `litmanUnderstandingTransportDemands2021a` (2021 version)
   - `litmanElasticities2024` (2024 version)
   - Various other elasticity reports
3. **Stevens (2017)** - **FOUND** as `stevensDoesCompactDevelopment2017` (not 2016 as cited in code)
4. **Lehner and Peer (2019)** - **FOUND** as `lehnerPriceElasticityParking2019` - Parking price elasticity meta-analysis

### Still Missing from Both Bibliographies:

1. **Harvey and Deakin (1998)** - Cited for VMT elasticity
2. **INFRAS (2000)** - Cited for VMT elasticity range
3. **Luk (1999)** - Cited for VMT elasticity range
4. **Hymel and Small (2015)** - Cited for VMT elasticity mean
5. **Small and Van Dender (2007)** - Cited for VMT elasticity verification (0.34 value)
6. **Goodwin, Dargay, and Hanly (2003)** - Cited for gas tax elasticity range
7. **Small (2007)** - Cited for gas tax elasticity mean
8. **Arentze, Hofman and Timmermans (2004)** - Cited for congestion elasticity
9. **PSRC (2005)** - Puget Sound Regional Council - Cited for congestion elasticity
10. **Small and Winston (1999)** - Cited for freight VMT elasticity, quoted in Litman (2011)
11. **Kim et al. (2015)** - Cited for telework marginal effect

## Data Sources

- **Price elasticities**: Found in `/data-raw/transportation_data_processing/elasticity.R`
- **5D elasticities**: Found in `/data-raw/transportation_data_processing/elasticity.R`
- **Other factors**: Found in `/data-raw/enviro_factors.R`
- **Data objects**: `ghg.ccap::elast`, `ghg.ccap::elast_5d`, `ghg.ccap::enviro_factors`
- **Documentation**: `R/x_data.R`, `man/elast.Rd`, `man/elast_5d.Rd`

**Note**: The `factor_values` dataset was checked but does not contain elasticities. It contains Annual Energy Outlook (AEO) factors for VMT and MPG changes, cost factors, and GHG emission factors - all different from elasticity values.

## Usage in Functions

These elasticities are used in the following transportation functions:

- `calc_elasticity()` - General elasticity calculation function (`R/t_04_preprocessing.R`)
- `calc_5d_effects()` - Applies 5D built environment elasticities (`R/t_04_vmt_effects.R`)
- Various VMT and mode choice functions in `R/t_04_vmt_effects.R`

## Notes

- All elasticity values in forecast years only (2025, 2030, 2035, 2040, 2045, 2050)
- Initial years (2015, 2018, 2020) have elasticity values of 0
- Elasticities are applied using the `calc_elasticity()` function which distributes the effect linearly over forecast years
- Most sources are peer-reviewed publications or meta-analyses
- Some sources need to be added to references.bib for complete documentation
