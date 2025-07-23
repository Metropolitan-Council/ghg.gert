rm(list=ls())
# 1. Load CTU and county boundaries ---------------------------------------

# Set your reference year
# This is the last year of inventory data, and the year before projections
ref_year <- 2022

# Load geographic index data and demographic_data, both part of ghg.ccap
# (only contains 7 counties)
lookup_ctu_county <- rbind(
  geog_index %>%
    dplyr::select(geog_name, geog_level, geog_id, geog_id_type),

  demographic_data %>% filter(geog_level == "COUNTY") %>%
    filter(inventory_year == ref_year & sp_categories == "population") %>%
    dplyr::select(geog_name, geog_level, geog_id, geog_id_type)
) %>% arrange(geog_name)



# Return CTU population percentages by county (for dealing with CTUs that cross county boundaries)
ctu_pop_by_county <-
  ctu_county %>% # Load ctu_county - includes information about CTUs that span multiple counties
  dplyr::select(-pct_land_area) %>%
  mutate(ctu_id = stringr::str_pad(ctu_id, width = 8, pad = "0"),
         co_code = paste0("27",stringr::str_pad(co_code, width = 3, pad = "0"))) %>%

  # NOTE: Empire (ctu_id = "02831011") was not in the ctu_county dataframe,
  # so it needs to be added here. Not sure why it was missing...
  bind_rows(tibble(ctu_id = "02831011", co_code = "27037", n_counties = 1, pct_population = 1)) %>%

  # add county metadata
  left_join(lookup_ctu_county %>%
              dplyr::select(geog_id, "co_name"=geog_name), by=join_by(co_code == geog_id)) %>%
  # add ctu metadata
  left_join(lookup_ctu_county %>%
              dplyr::select(geog_id, "ctu_name"=geog_name), by=join_by(ctu_id == geog_id)) %>%
  dplyr::select(ctu_id, co_code, ctu_name, co_name, everything()) %>%
  filter(pct_population != 0) %>% # remove rows where the population is zero
  # after removing the rows with zero population, re-classify the CTUs that span
  # multiple counties geographically but have 100% of their population within a
  # single county's boundary.
  mutate(n_counties = case_when(
    pct_population == 1 & n_counties > 1 ~ 1,
    .default=n_counties
  ))


# 2. Return CTUs with multiple counties ------------------------------------

# In some cases, a CTU will span two counties in terms of geographic area, but
# 100% of the population will be localized in one county and not the other.
# Return a dataframe of only the CTUs that both (1) span two counties and (2)
# contain population in both counties.
duplicate_ctus <- ctu_pop_by_county %>% filter(n_counties > 1)



# 3. Load demographic data for 2005 to 2050 -------------------------------

# Next we need to bring in population data
# these are population estimates for counties and CTUs from 2005 to 2050.
# - by county
county_population_data <- demographic_data %>%
  dplyr::filter(sp_categories == "population" & geog_level == "COUNTY") %>%
  rename(county_pop = value)

# - by CTU
ctu_population_data <- demographic_data %>%
  dplyr::filter(sp_categories == "population" & geog_level != "COUNTY") %>%
  rename(ctu_pop = value) %>%
  # Tag your ctu dataframe for cases where the population spans more
  # than one county. This will affect which per capita estimates we use for solid
  # waste source.
  mutate(flag = case_when(
    geog_id %in% duplicate_ctus$ctu_id~"duplicate",
    .default = NA
  ))





# 4. Load MPCA score data (2005 to 2022) ----------------------------------

# inpath_mpca_scores <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_waste/data-raw/solid_waste/"
inpath_mpca_scores <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/200-incorporate-wet-histosols-into-land-cover-inventory/_waste/data-raw/solid_waste/"
# inpath_cprg_sw_inventory <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_waste/data/"
#
#
# county_baseline <- readr::read_rds(paste0(inpath_cprg_sw_inventory, "final_solid_waste_allyrs.RDS")) %>%
#   dplyr::select(geog_id = geoid, inventory_year, source, value_activity, units_activity) %>%
#   arrange(geog_id, inventory_year, source)
# ctu_baseline <- readr::read_rds(paste0(inpath_cprg_sw_inventory, "final_solid_waste_ctu_allyrs.RDS"))%>%
#   dplyr::select(co_id = geoid, geog_id = ctuid, inventory_year, source, value_activity, units_activity) %>%
#   arrange(geog_id, inventory_year, source)


# load in annual MSW totals for each of the 9 counties (we only need 7)
waste_baseline <- readr::read_rds(paste0(inpath_mpca_scores, "mpca_score_allyrs.RDS"))
# state_total is for methane recovery or FOD. we don't need it

# Clean your county-level waste dataset
# This includes county-wide estimates of municipal solid waste from 2005 to 2022
# Next, we will downscale this to the CTU level
waste_baseline <- waste_baseline %>%
  # add geographic metadata
  left_join(lookup_ctu_county, by=join_by(geoid==geog_id)) %>%
  # remove geographies that have NA values in their name
  # (these are Chisago and Sherburne County)
  filter(!is.na(geog_name)) %>%
  rename(geog_id = geoid) %>%
  # add county level population data from 2005 to 2022
  left_join(county_population_data %>%
              dplyr::select(geog_id, inventory_year, county_pop),
            by = join_by(geog_id, inventory_year))




# 5. Calculate waste per capita for reference year ------------------------

#### generate per cap activity data for reference year, e.g. 2022 (by county)
waste_per_cap <- waste_baseline %>%
  dplyr::filter(inventory_year == ref_year) %>%
  dplyr::mutate(
    value_per_cap = value_activity/county_pop
  ) %>%
  dplyr::select(
    geog_id,
    source,
    value_per_cap
  ) %>% arrange(geog_id)





# 6. County level projections (2022 to 2050) ------------------------------

#### generate per-county activity projections past reference year (e.g. 2022)
# assuming per cap activity data is constant for each county and consistent with 2022
waste_proj_county <- county_population_data %>%
  dplyr::filter(inventory_year > ref_year) %>%
  dplyr::left_join(waste_per_cap, relationship = "many-to-many", by = join_by(geog_id)) %>%
  dplyr::mutate(
    value_activity = county_pop * value_per_cap,
    units_activity = "metric tons MSW"
  ) %>%
  dplyr::select(
    inventory_year,
    geog_id, geog_name, geog_level,
    county_pop,
    source,
    value_activity,
    units_activity
  ) %>%
  mutate(data_type = paste0("forecast using ", ref_year, " per capita estimates"))


# 7. County level baseline (2005 to 2022) ------------------------------

# create a dataframe that includes county-level waste use data prior to the reference year
# here, we have actual estimates going back to 2005, so we'll not be applying a fixed per capita
# rate based on population.
waste_baseline_county <- waste_baseline %>%
  filter(inventory_year <= ref_year) %>%
  dplyr::select(
    inventory_year,
    geog_id, geog_name, geog_level,
    county_pop,
    source,
    value_activity,
    units_activity
  ) %>%
  arrange(geog_name, inventory_year, source) %>%
  mutate(data_type = "county-wide MPCA estimates")




# rbind(
#   waste_baseline_county,
#   waste_proj_county
# ) %>%
#   ggplot() +
#   geom_line(aes(x=inventory_year, y=value_activity, color=source)) +
#   facet_wrap(~geog_name)



# 8. CTU level projections (2022 to 2050) ------------------------------
## CTU-level estimates
## IMPORTANT: Let's focus on CTUs that do NOT span county borders first
waste_proj_ctu_nonDupe <-
  # Take CTU population data
  ctu_population_data %>%
  # Filter for years after our reference year
  dplyr::filter(inventory_year > ref_year) %>%

  # IMPORTANT: Remove CTUs whose boundary exists in more than one county
  dplyr::filter(is.na(flag)) %>%
  dplyr::select(-c(geog_id_type, sp_categories, value_change_from_base, flag)) %>%

  # Join county metadata
  dplyr::left_join(ctu_pop_by_county %>%
                     # filter for cases where CTUs do not span county borders
                      dplyr::filter(n_counties==1) %>%
                      rename(geog_id = ctu_id,
                             geog_name = ctu_name) %>%
                      dplyr::select(geog_name, geog_id, co_code, co_name),
                    by = join_by(geog_name, geog_id)) %>%
  #
  dplyr::left_join(waste_per_cap %>%
                     rename(co_code = geog_id), relationship = "many-to-many", by = join_by(co_code)) %>%
  dplyr::mutate(
    value_activity = ctu_pop * value_per_cap,
    units_activity = "metric tons MSW"
  ) %>%
  dplyr::select(
    inventory_year,
    geog_id, geog_name, geog_level,
    ctu_pop,
    source,
    value_activity,
    units_activity
  ) %>%
  mutate(data_type = paste0("forecast using ", ref_year, " per capita estimates"))



waste_proj_ctu_Dupe <-
  # Take CTU population data
  ctu_population_data %>%
  # Filter for years after our reference year
  dplyr::filter(inventory_year > ref_year) %>%

  # IMPORTANT: Select CTUs whose boundary exists in more than one county
  dplyr::filter(!is.na(flag)) %>%
  dplyr::select(-c(geog_id_type, sp_categories, value_change_from_base, flag)) %>%

  dplyr::full_join(duplicate_ctus %>%
                     dplyr::rename(geog_name = ctu_name,
                                   geog_id = ctu_id),
                   by = join_by(geog_name, geog_id),
                   relationship = "many-to-many") %>%
  # Since these CTUs have population that span different county boundaries AND
  # since different counties have different solid waste estimates, we first need
  # to determine how much of a CTUs population exists in either county, and then
  # apply the county-specific solid waste activity rates.

  group_by(geog_id, inventory_year) %>%
  mutate(
    # Step 1: Raw estimate and floor
    pop_est = ctu_pop * pct_population,
    pop_floor = floor(pop_est),
    residual = pop_est - pop_floor,
    # Step 2: Calculate how many people are left to allocate
    total_floor = sum(pop_floor),
    remainder = round(ctu_pop - total_floor),  # This is how many people we need to "round up"
    # Step 3: Rank counties by largest residuals
    rank = rank(-residual, ties.method = "first"),
    # Step 4: Assign 1 extra person to top N counties by residual
    extra = as.integer(rank <= remainder),
    ctu_pop_actual = pop_floor + extra
  ) %>%
  select(-pop_est, -pop_floor, -residual, -total_floor, -remainder, -rank, -extra) %>%
  ungroup() %>%


  dplyr::left_join(waste_per_cap %>%
                     rename(co_code = geog_id), relationship = "many-to-many",
                   by = join_by(co_code)) %>%

  dplyr::mutate(
    value_activity = ctu_pop_actual * value_per_cap,
    units_activity = "metric tons MSW"
  ) %>%
  group_by(geog_name, geog_id, geog_level,
           inventory_year, source) %>%
  summarize(ctu_pop = head(ctu_pop,1),
            value_activity = sum(value_activity),
            units_activity = head(units_activity,1), .groups="keep") %>%

  ungroup() %>%
  dplyr::select(
    inventory_year,
    geog_id, geog_name, geog_level,
    ctu_pop,
    source,
    value_activity,
    units_activity
  ) %>%
  mutate(data_type = paste0("forecast using ", ref_year, " per capita estimates"))


waste_proj_ctu <- rbind(waste_proj_ctu_nonDupe, waste_proj_ctu_Dupe) %>%
  arrange(geog_name, inventory_year, source)


# 9. CTU level baseline (2005 to 2022) ------------------------------
waste_baseline_ctu_nonDupe <-
  ctu_population_data %>%
  filter(inventory_year <= ref_year) %>%
  # IMPORTANT: Remove CTUs whose boundary exists in more than one county
  dplyr::filter(is.na(flag)) %>%
  dplyr::select(-c(geog_id_type, sp_categories, value_change_from_base, flag)) %>%

  # Join county metadata
  dplyr::left_join(ctu_pop_by_county %>%
                     # filter for cases where CTUs do not span county borders
                     dplyr::filter(n_counties==1) %>%
                     rename(geog_id = ctu_id,
                            geog_name = ctu_name) %>%
                     dplyr::select(geog_name, geog_id, co_code, co_name),
                   by = join_by(geog_name, geog_id)) %>%
  dplyr::left_join(waste_baseline %>%
                     mutate(value_per_cap =  value_activity/county_pop) %>%
                     dplyr::select(co_code = geog_id, source,
                                   inventory_year, value_per_cap),
                   relationship = "many-to-many",
                   by = join_by(co_code, inventory_year)) %>%
  dplyr::mutate(
    value_activity = ctu_pop * value_per_cap,
    units_activity = "metric tons MSW"
  ) %>%
  dplyr::select(
    inventory_year,
    geog_id, geog_name, geog_level,
    ctu_pop,
    source,
    value_activity,
    units_activity
  ) %>%
  mutate(data_type = "downscaled from county-wide MPCA estimates")


waste_baseline_ctu_Dupe <-
  # Take CTU population data
  ctu_population_data %>%
  # Filter for years before our reference year
  filter(inventory_year <= ref_year) %>%

  # IMPORTANT: Select CTUs whose boundary exists in more than one county
  dplyr::filter(!is.na(flag)) %>%
  dplyr::select(-c(geog_id_type, sp_categories, value_change_from_base, flag)) %>%

  dplyr::full_join(duplicate_ctus %>%
                     dplyr::rename(geog_name = ctu_name,
                                   geog_id = ctu_id),
                   by = join_by(geog_name, geog_id),
                   relationship = "many-to-many") %>%
  # Since these CTUs have population that span different county boundaries AND
  # since different counties have different solid waste estimates, we first need
  # to determine how much of a CTUs population exists in either county, and then
  # apply the county-specific solid waste activity rates.

  group_by(geog_id, inventory_year) %>%
  mutate(
    # Step 1: Raw estimate and floor
    pop_est = ctu_pop * pct_population,
    pop_floor = floor(pop_est),
    residual = pop_est - pop_floor,
    # Step 2: Calculate how many people are left to allocate
    total_floor = sum(pop_floor),
    remainder = round(ctu_pop - total_floor),  # This is how many people we need to "round up"
    # Step 3: Rank counties by largest residuals
    rank = rank(-residual, ties.method = "first"),
    # Step 4: Assign 1 extra person to top N counties by residual
    extra = as.integer(rank <= remainder),
    ctu_pop_actual = pop_floor + extra
  ) %>%
  select(-pop_est, -pop_floor, -residual, -total_floor, -remainder, -rank, -extra) %>%
  ungroup() %>%


  dplyr::left_join(waste_baseline %>%
                     mutate(value_per_cap =  value_activity/county_pop) %>%
                     dplyr::select(co_code = geog_id, source,
                                   inventory_year, value_per_cap),
                   relationship = "many-to-many",
                   by = join_by(co_code, inventory_year)) %>%

  dplyr::mutate(
    value_activity = ctu_pop_actual * value_per_cap,
    units_activity = "metric tons MSW"
  ) %>%
  group_by(geog_name, geog_id, geog_level,
           inventory_year, source) %>%
  summarize(ctu_pop = head(ctu_pop,1),
            value_activity = sum(value_activity),
            units_activity = head(units_activity,1), .groups="keep") %>%

  ungroup() %>%
  dplyr::select(
    inventory_year,
    geog_id, geog_name, geog_level,
    ctu_pop,
    source,
    value_activity,
    units_activity
  ) %>%
  mutate(data_type = "downscaled from county-wide MPCA estimates")

waste_baseline_ctu <- rbind(waste_baseline_ctu_nonDupe, waste_baseline_ctu_Dupe) %>%
  arrange(geog_name, inventory_year, source)



# 9. Compile waste data ---------------------------------------------------
## Store data in a list
waste_data <-list()
waste_data$ctu$baseline <- waste_baseline_ctu %>% dplyr::select(-ctu_pop)
waste_data$ctu$projections <- waste_proj_ctu %>% dplyr::select(-ctu_pop)

waste_data$county$baseline <- waste_baseline_county %>% dplyr::select(-county_pop)
waste_data$county$projections <- waste_proj_county %>% dplyr::select(-county_pop)



# waste characterization
waste_char <- readr::read_rds(paste0(inpath_mpca_scores, "mpca_waste_composition.RDS"))
waste_data$characterization <- waste_char


usethis::use_data(waste_data, overwrite = TRUE)














#
# ## ZOEY CODE BELOW
# #### bring in population data (2020)
# population_data <- demographic_data %>%
#   dplyr::filter(sp_categories == "population") %>% # filter for population counts
#   # dplyr::right_join(ctu_county, relationship = "many-to-many") %>%
#   dplyr::right_join(., ctu_county %>%
#                       dplyr::mutate(
#                         geog_id = paste0("27", stringr::str_pad(as.character(co_code), 3, pad = "0"))
#                       ),
#                     relationship = "many-to-many"
#                     ) %>%
#
#   # dplyr::mutate(
#   #   geoid = paste0("27", stringr::str_pad(as.character(co_code), 3, pad = "0"))
#   # ) %>%
#   dplyr::select(
#     -c(co_code, pct_land_area)
#   )
#
# population_county <- population_data %>%
#   dplyr::mutate(co_pop_allocation = round(value * pct_population)) %>%
#   dplyr::group_by(inventory_year, geoid) %>%
#   dplyr::summarise(
#     county_pop = sum(co_pop_allocation)
#   ) %>%
#   tidyr::drop_na(county_pop)
#
# #### generate per cap activity data for 2020
# waste_per_cap <- waste_baseline %>%
#   dplyr::right_join(population_county %>% dplyr::filter(inventory_year == 2020)) %>%
#   dplyr::mutate(
#     value_per_cap = value_activity/county_pop
#   ) %>%
#   dplyr::select(
#     geoid,
#     source,
#     value_per_cap
#   )
#
# #### generate per-county activity projections past 2020
#
# # assuming per cap activity data is constant for each county and consistent with 2020
# waste_data_county <- population_county %>%
#   dplyr::filter(inventory_year != 2010) %>%
#   dplyr::left_join(waste_per_cap, relationship = "many-to-many") %>%
#   dplyr::mutate(
#     value_activity = county_pop * value_per_cap,
#     class = "COUNTY",
#     units_activity = "metric tons MSW"
#   ) %>%
#   dplyr::select(
#     inventory_year,
#     geoid,
#     county_pop,
#     source,
#     value_activity,
#     units_activity
#   )
#
# #### allocate to ctus
#
# waste_data_ctu <- population_data %>%
#   dplyr::right_join(ctu_county, relationship = "many-to-many") %>%
#   dplyr::right_join(waste_data_county, relationship = "many-to-many") %>%
#   dplyr::mutate(ctu_percent_of_county_pop = value/county_pop) %>%
#   dplyr::mutate(value_activity = value_activity*ctu_percent_of_county_pop,
#                 units_activity = "metric tons MSW"
#   ) %>%
#   dplyr::select(
#     inventory_year,
#     ctu_id,
#     ctu_name,
#     source,
#     value_activity,
#     units_activity
#   )
#
# # to test: is waste_projections 2020 activity data equal to waste_baseline 2020 data
#
# waste_data <-list()
# waste_data$ctu <- waste_data_ctu
# waste_data$county <- waste_data_county %>%
#   dplyr::select(
#     -county_pop
#   )
#
# # waste characterization
# waste_char <- readr::read_rds(paste0(inpath, "mpca_waste_composition.RDS"))
# waste_data$characterization <- waste_char
#
# # usethis::use_data(waste_data, overwrite = TRUE)

