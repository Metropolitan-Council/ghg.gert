##### Parse Imagine 2050 Local Forecasts (post-model-adjusted)
# Source: Metropolitan Council Imagine 2050 Local Forecasts (December 2025)
# These are the post-processing adjusted totals (population, households,
# employment) that reflect municipality feedback. They are coarser than
# UrbanSim modeling outputs but represent the official published numbers.
# We use them to proportionally scale the detailed UrbanSim breakdowns
# so that subcategory totals match what is reported externally.

library(dplyr)
library(readxl)
library(tidyr)
library(stringr)

imagine_raw <- readxl::read_xlsx(
  file.path(here::here(), "data-raw/meta/Imagine-2050-Local-Forecasts-December-2025.xlsx"),
  skip = 4,
  # force coctu_id to text so leading zeros are preserved
  col_types = c(
    "text", "text", "text", "date", "text",
    rep("numeric", 16)
  )
) %>%
  janitor::clean_names()


# Retain only rows with a valid coctu_id (drops county totals, region total,
# and footer notes)
imagine <- imagine_raw %>%
  filter(!is.na(coctu_id)) %>%
  mutate(
    # strip footnote asterisks from city names
    city_or_township = str_remove_all(city_or_township, "\\*"),
    # normalize coctu_id to 11-char zero-padded string
    coctu_id = str_pad(coctu_id, width = 11, pad = "0", side = "left"),
    # extract GNIS from coctu_id (last 8 chars)
    ctu_id_gnis = substr(coctu_id, nchar(coctu_id) - 7, nchar(coctu_id))
  )

# Pivot to long format: one row per coctu × year × variable
imagine_long <- imagine %>%
  select(
    coctu_id, ctu_id_gnis,
    matches("^(population|households|employment)_\\d{4}$")
  ) %>%
  pivot_longer(
    cols = matches("^(population|households|employment)_\\d{4}$"),
    names_to = c("imagine_variable", "emissions_year"),
    names_pattern = "^(population|households|employment)_(\\d{4})$",
    values_to = "imagine_value"
  ) %>%
  mutate(emissions_year = as.numeric(emissions_year))

# Map imagine variables to the sp_categories used in UrbanSim for matching
# population  → "population"
# households  → "total_households"
# employment  → "jobs"
imagine_long <- imagine_long %>%
  mutate(sp_categories_match = case_when(
    imagine_variable == "population" ~ "population",
    imagine_variable == "households" ~ "total_households",
    imagine_variable == "employment" ~ "jobs"
  ))

# Summarize COCTU-level data to municipality (CTU) level.
# Multi-county cities (e.g. Blaine in Anoka + Ramsey) get summed.
imagine_ctu <- imagine_long %>%
  group_by(ctu_id_gnis, emissions_year, imagine_variable, sp_categories_match) %>%
  summarize(imagine_value = sum(imagine_value), .groups = "drop") %>%
  # Filter to CTUs present in geog_index for canonical naming
  inner_join(
    geog_index %>% select(geog_id, geog_name) %>% distinct(),
    by = c("ctu_id_gnis" = "geog_id")
  ) %>%
  rename(geog_id = ctu_id_gnis)

# Check for any Imagine CTUs that didn't match geog_index
imagine_unmatched <- imagine_long %>%
  distinct(ctu_id_gnis) %>%
  anti_join(geog_index, by = c("ctu_id_gnis" = "geog_id"))

if (nrow(imagine_unmatched) > 0) {
  message(
    "Imagine 2050: ", nrow(imagine_unmatched),
    " CTUs not matched in geog_index (excluded): ",
    paste(imagine_unmatched$ctu_id_gnis, collapse = ", ")
  )
}

# Extract county totals from the raw data.
# These rows have NA coctu_id and "X County Total" in the city column.
# Build a county name → FIPS lookup from the CTU-level data we already parsed.
county_fips_lookup <- imagine %>%
  mutate(county_id_fips = str_pad(
    substr(coctu_id, 1, nchar(coctu_id) - 8),
    width = 3, pad = "0", side = "left"
  )) %>%
  distinct(county, county_id_fips)

imagine_county <- imagine_raw %>%
  filter(is.na(coctu_id), str_detect(city_or_township, "County Total")) %>%
  left_join(geog_index %>% filter(geog_level == "COUNTY"),
            by = c("county" = "geog_short_name")) %>%
  select(
    geog_id,
    geog_name,
    matches("^(population|households|employment)_\\d{4}$")
  ) %>%
  pivot_longer(
    cols = matches("^(population|households|employment)_\\d{4}$"),
    names_to = c("imagine_variable", "emissions_year"),
    names_pattern = "^(population|households|employment)_(\\d{4})$",
    values_to = "imagine_value"
  ) %>%
  mutate(
    emissions_year = as.numeric(emissions_year),
    sp_categories_match = case_when(
      imagine_variable == "population" ~ "population",
      imagine_variable == "households" ~ "total_households",
      imagine_variable == "employment" ~ "jobs"
    )
  )

# Add regional total row
imagine_region <- imagine_ctu %>%
  group_by(emissions_year, imagine_variable, sp_categories_match) %>%
  summarize(imagine_value = sum(imagine_value), .groups = "drop") %>%
  mutate(
    geog_id = "00000000",
    geog_name = "Twin Cities Region"
  )

imagine_forecasts <- bind_rows(imagine_ctu, imagine_county, imagine_region)

saveRDS(
  imagine_forecasts,
  file.path(here::here(), "data-raw/meta/imagine_2050_forecasts.RDS")
)
