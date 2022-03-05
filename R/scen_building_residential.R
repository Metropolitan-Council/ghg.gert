scen_building_residential <- function(tb = building_data,
                                      .new_homes_to_multifamily_pct = 0.5,
                                      .single_family_floor_area_growth_pct = 0.05,
                                      .new_homes_leed_gold_pct = 0.5,
                                      .existing_home_retrofit_pct = 0.8,
                                      .existing_home_ultra_retrofit_pct = 0.2, # needs to be 1 - .existing_home_retrofit_pct
                                      .home_behavior_change_pct = 1,
                                      .homes_electric_heating_pct = 0.59){

  ### Compact buildings: half of new SF homes become MF
  ## 50% new multifamily
  s_percent_new_homes <- 0.5

  # tool assumes that floor area will increase over time

  # of all the new single family homes that will be built between 2018 and 2040,
  # make 50% of them multifamily units.

  # increse multifamily unit production
  building_data %>%
    filter(ctu_name == "Lake Elmo",
           var %in% c("single_family_units",
                      "multifamily_units",
                      "single_family_average_floor_area_sqft_ctu",
                      "multifamily_average_floor_area_sqft_county")) %>%
    select(-kg_co2e_per_floor_area) %>%
    group_by(ctu_name, var) %>%
    pivot_wider(names_from = "year", values_from = "value") %>%
    mutate(diff = `2040` - `2018`)

  # 7731 * 2459 = 19010529 sqft of SF
  # 446 * 1551 = 691746 sqft of MF

  # 4137 SF units * 0.5 = 2068.5 MF units
  # 469 +
  #

  ## 5% Growth Rate of Single Family Floor Area
  s_growth_rate_singlefamily_floor_area <- 0.05

  ## Percent of New Homes that LEED
  # 50% new homes to LEED Gold (25 kBTU/sf; electricity reduces by 64% and gas by 64%)
  s_percent_of_new_leed_homes <- 0.5

  ### Existing Homes (80%) Retrofitted to Performance-Based High Energy Efficiency Standards
  ## Percent of Single Family Units to be Retrofitted
  s_percent_of_existing_homes_to_retrofitted <- 0.8

  ## Percent of Largest Existing Homes to Become Passive Homes
  s_percent_of_largest_existing_homes_to_be_passive_homes <- 0.2

  # All homes receive effective messages, use in-home display, and smart meters (reduce HH energy use by 11%)
  s_percent_of_homes_changes_behaviors <- 1


  s_percent_of_additional_households_with_heating_electrified <- 0.59
}
