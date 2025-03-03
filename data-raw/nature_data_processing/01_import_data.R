# Clear old variables
rm(list = ls())

# Load required packages --------------------------------------------------
# List the packages you'll need
ListOfPackages <- c("tidyverse", "plotly", "patchwork", "usethis", "readr")

# From this list, check any that aren't currently installed
newPackages <- ListOfPackages[!(ListOfPackages %in% installed.packages()[,"Package"])]

# If any new packages are not currently loaded, load them now
if(length(newPackages)) install.packages(newPackages)
lapply(ListOfPackages, library, character.only=TRUE)

# Install the released version of councilR from GitHub.
remotes::install_github("Metropolitan-Council/councilR")
library(councilR)


inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/dev-2005-baseline/_nature/data/"

county_seq <- readr::read_rds(paste0(inpath, "nlcd_county_landcover_sequestration_2001_2021.rds"))
ctu_seq <- readr::read_rds(paste0(inpath, "nlcd_ctu_landcover_sequestration_2001_2021.rds"))

county_waterways <- readr::read_rds(paste0(inpath, "nhd_county_waterways_emissions_2001_2021.rds"))
ctu_waterways <- readr::read_rds(paste0(inpath, "nhd_ctu_waterways_emissions_2001_2021.rds"))

land_cover_c <- readr::read_rds(paste0(inpath, "land_cover_carbon.rds"))




# Scenario 0 - Project no changes out to 2050 -----------------------------


# Here we'll do a simple case where we assume that the sequestration rates for each land cover type are constant over time.
# Next, we'll take the sequestration rates and project out to 2050 for each land cover type.
# Finally, we'll create a wedge diagram using ggplot to visualize the sequestration rates over time.
# Ultimately, we will build more complex models to simulate change over time, but for now, this is a good start.

# Create a new data frame to hold the projected sequestration rates
# We're going to start with the county_seq data frame

# First let's create a wedge diagram with what we already have: data from 2001 to 2021
# We will use the county_seq data frame to do this
p0 <- rbind(
  county_seq,
  county_seq %>%
    filter(year==2021) %>%
    dplyr::select(-year) %>%
    crossing(
      year = seq(2022, 2050, by = 1)
    )
) %>%
  arrange(year, county_name, land_cover_type) %>%
  ggplot() +
  # geom_area(aes(x = year, y = sequestration_potential, fill = land_cover_type)) +
  geom_area(aes(x = year, y = sequestration_potential, fill = land_cover_type), alpha=0.5) +
  geom_area(data=. %>% filter(year <= 2021),
            mapping=aes(x = year, y = sequestration_potential, fill = land_cover_type)) +
  geom_vline(xintercept = 2021, linetype="dashed")+
  scale_fill_manual(
    values = c(
      "Tree" = "#4CAF50",
      "Grassland" = "#FFEB3B",
      "Wetland" = "#75D4D9",
      "Urban_Tree" = "#B6E39A",
      "Urban_Grassland" = "#D4CA6F"
    ),
    breaks = c(
      "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland"
    ),
    labels = c(
      "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland"
    ),
  ) +
  facet_wrap(~county_name,scale="free_y") +
  labs(title = "Sequestration Rates by Land Cover Type",
       subtitle = "Area is unchanged out to 2050",
       x = "Year",
       y = expression("Sequestration Rate (metric tons"~CO[2]*"e)"),
       fill = "Land Cover Type") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2000, 2050, by = 5)) +
  scale_y_continuous(
    # want to show labels in metric tons where 50000 would be 50k on the axis bar
    labels = scales::label_number(scale = 1e-3, suffix = "k", accuracy = 1)
  ) +
  theme(
    axis.text.x = element_text(angle=45, hjust=1)
  )



# Scenario 1 - Half of wetland area gets converted to forest ------------------

# Next we should import the emissions factors
# Instead of simply projecting out a straight line, let's get weird with it!

# Scenario 1: every year, we convert __ acres of wetland to forest
# Rules:
#+ area must be conserved
#+ sequestration rates do not change


df <- county_seq %>% ungroup() %>%
  filter(year==2021) %>%
  dplyr::select(c(county_name, state_name, land_cover_type, area))

start_year <- 2021
end_year <- 2050

years <- seq(start_year, end_year)
n_years <- length(years)

df_noChange <- df %>%
  filter(!(land_cover_type %in% c("Wetland","Tree"))) %>%
  crossing(
    year=seq(start_year,end_year,1)
  ) %>%
  arrange(county_name,land_cover_type,year)

df_projected <- df %>%
  filter(land_cover_type %in% c("Wetland","Tree")) %>%
  group_by(county_name) %>%
  mutate(
    wetland_start = area[land_cover_type == "Wetland"],
    tree_start = area[land_cover_type == "Tree"],
    wetland_end = wetland_start / 2, # reduce wetland area by 50%
    tree_end = tree_start + (wetland_start - wetland_end) # wetland area loss gets converted to tree area gain
  ) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  dplyr::select(county_name, state_name, wetland_start, tree_start, wetland_end, tree_end)

df_coverChange <- rbind(
  df_projected %>%
    dplyr::select(county_name, state_name, wetland_start, wetland_end) %>%
    pivot_longer(
      cols=starts_with("wetland"),
      names_to="type",
      values_to = "area"
    ) %>%
    mutate(year=case_when(
      type=="wetland_start"~start_year,
      type=="wetland_end"~end_year
    )) %>% dplyr::select(-type) %>%
    mutate(land_cover_type="Wetland"),

  df_projected %>%
    dplyr::select(county_name, state_name, tree_start, tree_end) %>%
    pivot_longer(
      cols=starts_with("tree"),
      names_to="type",
      values_to = "area"
    ) %>%
    mutate(year=case_when(
      type=="tree_start"~start_year,
      type=="tree_end"~end_year
    )) %>% dplyr::select(-type) %>%
    mutate(land_cover_type="Tree")
) %>%
  full_join(
    crossing(
      county_name=unique(df$county_name),
      land_cover_type=c("Wetland","Tree"),
      year=seq(start_year+1,end_year-1,1)
    ) %>% left_join( # add state_name
      county_seq %>% ungroup() %>%
        group_by(county_name, state_name) %>%
        summarize(idx = head(year,1)) %>% dplyr::select(-idx)
      , by = join_by(county_name))
  ) %>%
  group_by(county_name, state_name, land_cover_type) %>%
  arrange(county_name,land_cover_type,year) %>%
  mutate(
    area = zoo::na.approx(area, na.rm = FALSE))


county_proj_1 <- rbind(df_coverChange,
                       df_noChange) %>%
  arrange(county_name,land_cover_type,year) %>%
  filter(year != 2021) %>%
  left_join(land_cover_c, by = join_by(land_cover_type)) %>%
  mutate(
    sequestration_potential = area * seq_mtco2e_sqkm,
    stock_potential = area * stock_mtco2e_sqkm
  ) %>%
  dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm)) %>%
  arrange(county_name, year, land_cover_type)


county_inv_1 <- county_seq %>% ungroup() %>%
  dplyr::select(c(county_name, state_name, area, year, land_cover_type, sequestration_potential, stock_potential))



p1 <- rbind(
  county_inv_1,
  county_proj_1
) %>%
  arrange(year, county_name, land_cover_type) %>%
  ggplot() +
  # geom_area(aes(x = year, y = sequestration_potential, fill = land_cover_type)) +
  geom_area(aes(x = year, y = sequestration_potential, fill = land_cover_type), alpha=0.5) +
  geom_area(data=. %>% filter(year <= 2021),
            mapping=aes(x = year, y = sequestration_potential, fill = land_cover_type)) +
  geom_vline(xintercept = 2021, linetype="dashed")+
  scale_fill_manual(
    values = c(
      "Tree" = "#4CAF50",
      "Grassland" = "#FFEB3B",
      "Wetland" = "#75D4D9",
      "Urban_Tree" = "#B6E39A",
      "Urban_Grassland" = "#D4CA6F"
    ),
    breaks = c(
      "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland"
    ),
    labels = c(
      "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland"
    ),
  ) +
  facet_wrap(~county_name,scale="free_y") +
  labs(title = "Sequestration Rates by Land Cover Type",
       subtitle = "`Wetlands` lose 50% by 2050; converted to `Tree`",
       x = "Year",
       y = expression("Sequestration Rate (metric tons"~CO[2]*"e)"),
       fill = "Land Cover Type") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2000, 2050, by = 5)) +
  scale_y_continuous(
    # want to show labels in metric tons where 50000 would be 50k on the axis bar
    labels = scales::label_number(scale = 1e-3, suffix = "k", accuracy = 1)
  ) +
  theme(
    axis.text.x = element_text(angle=45, hjust=1)
  )

p0
p1




