# Here we're going to import the area coverage data from ghg-cprg
# Goal is to have a holistic accounting of area covered by each cover type

# Clear old variables
rm(list = ls())

# Load required packages --------------------------------------------------
# List the packages you'll need
ListOfPackages <- c("tidyverse", "plotly", "patchwork", "usethis", "readr", "shiny")

# From this list, check any that aren't currently installed
newPackages <- ListOfPackages[!(ListOfPackages %in% installed.packages()[,"Package"])]

# If any new packages are not currently loaded, load them now
if(length(newPackages)) install.packages(newPackages)
lapply(ListOfPackages, library, character.only=TRUE)

# Install the released version of councilR from GitHub.
remotes::install_github("Metropolitan-Council/councilR")
library(councilR)


inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_nature/data/"

lc_county <- readr::read_rds(paste0(inpath, "nlcd_county_landcover_allyrs.rds"))
lc_ctu <- readr::read_rds(paste0(inpath, "nlcd_ctu_landcover_allyrs.rds"))

seq_county <- readr::read_rds(paste0(inpath, "nlcd_county_landcover_sequestration_allyrs.rds"))
seq_ctu <- readr::read_rds(paste0(inpath, "nlcd_ctu_landcover_sequestration_allyrs.rds"))

waterways_county <- readr::read_rds(paste0(inpath, "nhd_county_waterways_emissions_allyrs.rds"))
waterways_ctu <- readr::read_rds(paste0(inpath, "nhd_ctu_waterways_emissions_allyrs.rds"))

land_cover_c <- readr::read_rds(paste0(inpath, "land_cover_carbon.rds"))



# Here we have land cover data for Anoka county for the years 2001 to 2022
# The cover types are: Bare, Built_Up, Cropland, Shrubland, Grassland, Tree, Wetland, Water, and Urban_Grassland and Urban_Tree

# I want to build a Shiny app that will allow me to do the following:
# - Select a county (lc_county currently has all counties of interest)
# - plot the area covered by each land cover type for the selected county for the years that are available
# - project forward changes in area for each land cover type going from 2023 to 2050

# Some details to get started
# - I will need to have a reactive data frame that will be used to plot the data (lc_county is in long format right now)
# - any changes in land area have to make sense. For example, if the area of a land cover type decreases, it should not go below 0

# Let's set out some rules
# - the area for one cover type must come from somewhere. If the area of one cover type decreases, the area of another cover type must increase.
# - some cover types cannot be converted into other types. Some examples include:
# -- Water cannot be converted into Tree
# -- Tree cannot be converted into Water
# -- Cropland can be converted into Tree or Grassland
# -- Wetland can be converted to Tree but not Grassland
# -- Grassland can be converted to Tree but not Wetland
# -- Tree, Wetland, and Grassland can be converted to Built_Up
# -- Tree, Wetland, and Grassland can be converted to Cropland
# -- Built_Up cannot be converted into any other cover type except Urban_Grassland and Urban_Tree
# -- Urban_Grassland and Urban_Tree cannot be converted into any other cover type

# With these rules in mind, I want to build a Shiny app that will allow me to project changes in land cover area for a selected county from 2023 to 2050
# (we also want to show the area from 2001 to 2022)
# To keep things simple, let's build a simple app that does the following for Anoka county:

# - plot the area covered by each land cover type for Anoka county for the years 2001 to 2022
# - project forward changes in area for each land cover type going from 2023 to 2050
# - include a slider that will allow me to adjust the afforestation factor (the percentage of available land that can be converted to forest)
# - include a slider that will allow me to adjust the time until planting begins (the number of years until afforestation begins)
# - the change over time in the plot should not be a sudden increase but a gradual increase over the number of years specified in the planting delay slider



# Final Logical Rules -----------------------------------------------------

# -	Built-Up converted to Urban_Tree (only)
#   -	Want to encourage tree planting
# -	Cropland converted to Built_Up or Tree (development vs restoration)
#   -	Two sliders
#   - Think pie chart
# -	Tree, Grassland, Wetland can all be developed
#   -	Make sure you penalize by losing C stock!
# -	Urban_Grassland can be converted to Grassland
#   -	Lawns to legumes concept






# Transform data for ggplot (long format)
inventory_data <- lc_county %>%
  ungroup() %>%
  dplyr::select(land_cover_type, inventory_year, area, county_name) %>%
  pivot_wider(names_from = land_cover_type, values_from = area) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(c(Bare, Built_Up, Urban_Grassland, Urban_Tree,
                                Cropland, Grassland, Shrubland, Tree, Water,
                                Wetland)), na.rm = T)) %>%
  ungroup()  %>%
  # replace NAs with 0
  mutate(across(everything(), ~replace_na(., 0)))


# # Transform data for ggplot (long format)
# inventory_data <- lc_county %>%
#   filter(county_name == "Anoka") %>%
#   ungroup() %>%
#   dplyr::select(land_cover_type, inventory_year, area, -county_name) %>%
#   pivot_wider(names_from = land_cover_type, values_from = area) %>%
#   rowwise() %>%
#   mutate(TOTAL = sum(c_across(c(Bare, Built_Up, Urban_Grassland, Urban_Tree,
#                               Cropland, Grassland, Shrubland, Tree, Water,
#                               Wetland)), na.rm = T)) %>%
#   ungroup()

# inventory_end <- tail(inventory_data,1)


# Define UI
ui <- fluidPage(
  # Add custom CSS for font sizes
  tags$head(
    tags$style(HTML("
      h3 { font-size: 16px; }  /* Adjust header font size */
      h4 { font-size: 14px; }  /* Adjust sub-header font size */
      label { font-size: 12px; }  /* Adjust label font size */
      .form-control { font-size: 12px; }  /* Adjust input font size */
    "))
  ),
  titlePanel("Annual Land Area (2005-2050)"),
  sidebarLayout(
    sidebarPanel(
      # Header for County Selection
      tags$h3("County Selection"),
      selectInput("selected_county", "Select County:",
                  choices = unique(lc_county$county_name),  # Populate dropdown with county names
                  selected = "Anoka"),  # Default selection

      # Header for Cropland Allocation
      tags$h3("Cropland Allocation"),
      sliderInput("restoration", "% Cropland Area Available for Restoration (Tree)",
                  min = 0, max = 100, value = 0),
      sliderInput("development", "% Cropland Area Available for Development (Built Up)",
                  min = 0, max = 100, value = 0),

      # Header for Planting Settings
      tags$h3("Planting Settings"),
      sliderInput("planting_start_year", "Afforestation Start Year",
                  min = 2023, max = 2050, value = 2023, step = 1, sep = ""),  # Use `sep = ""` to remove commas
      sliderInput("planting_time", "Afforestation Planting Time (years)",
                  min = 1, max = 20, value = 10),  # New slider for sigmoidal steepness

      # Reset Button
      actionButton("reset_sliders", "Reset Sliders")
    ),
    mainPanel(
      plotOutput("wedgePlot"),         # First plot: Land area change
      plotOutput("sequestrationPlot")  # Second plot: Carbon sequestration
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reset sliders when a new county is selected
  observeEvent(input$selected_county, {
    updateSliderInput(session, "restoration", value = 0)
    updateSliderInput(session, "development", value = 0)
    updateSliderInput(session, "planting_start_year", value = 2023)
    updateSliderInput(session, "planting_time", value = 10)
  })


  # Observer to reset sliders to default values
  observeEvent(input$reset_sliders, {
    updateSliderInput(session, "restoration", value = 0)
    updateSliderInput(session, "development", value = 0)
    updateSliderInput(session, "planting_start_year", value = 2023)
    updateSliderInput(session, "planting_time", value = 10)
  })

  # Ensure the sliders are interdependent
  observe({
    # Dynamically update the maximum value of the "development" slider
    if (input$restoration <= 100) {
      updateSliderInput(session, "development",
                        max = 100 - input$restoration)
    }

    # Dynamically update the maximum value of the "restoration" slider
    if (input$development <= 100) {
      updateSliderInput(session, "restoration",
                        max = 100 - input$development)
    }

    # Ensure the current values do not exceed the new maximums
    if (input$restoration + input$development > 100) {
      if (input$restoration > 100 - input$development) {
        updateSliderInput(session, "restoration",
                          value = 100 - input$development)
      }
      if (input$development > 100 - input$restoration) {
        updateSliderInput(session, "development",
                          value = 100 - input$restoration)
      }
    }
  })


  # Reactive data for selected county
  filtered_data <- reactive({
    inventory_data %>%
      filter(county_name == input$selected_county)
  })

  # Reactive data for projections
  projected_data <- reactive({
    future_years <- 2023:2050
    restoration_factor <- input$restoration / 100
    development_factor <- input$development / 100
    planting_start_year <- input$planting_start_year  # Selected start year
    planting_delay <- planting_start_year - 2023  # Calculate planting delay dynamically
    planting_time <- input$planting_time

    # Filter historical data for the selected county
    historical_data <- filtered_data()

    # Get the last row of the filtered data for projections
    inventory_end <- tail(historical_data, 1)

    # Total Cropland area in 2022
    total_cropland <- inventory_end$Cropland

    # Calculate areas for restoration and development
    restoration_area <- restoration_factor * total_cropland
    development_area <- development_factor * total_cropland
    remaining_cropland <- total_cropland - restoration_area - development_area

    # Adjust effective years based on planting delay
    effective_years <- length(future_years) - planting_delay

    # Scale restoration and development factors over effective years
    scaled_restoration_factor <- restoration_area / total_cropland * (effective_years / length(future_years))
    scaled_development_factor <- development_area / total_cropland * (effective_years / length(future_years))

    # Sigmoidal growth parameters
    r <- 1 / planting_time  # Growth rate is inversely proportional to planting time
    t0 <- planting_start_year  # Start year determines when the curve begins

    # Logistic growth function with normalization and constraints
    logistic_growth <- function(t, K, r, t0, start_year, end_year) {
      # Standard logistic growth
      raw_growth <- K / (1 + exp(-r * (t - t0)))
      # Normalize to ensure it starts at 0 and ends at K
      growth_start <- K / (1 + exp(-r * (start_year - t0)))
      growth_end <- K / (1 + exp(-r * (end_year - t0)))
      normalized_growth <- (raw_growth - growth_start) / (growth_end - growth_start) * K
      return(normalized_growth)
    }

    # Logistic growth function for Built_Up with a fixed midpoint
    simple_sigmoid <- function(t, K, r, midpoint) {
      # Standard logistic growth centered at the midpoint
      K / (1 + exp(-r * (t - midpoint)))
    }

    # Generate projections
    future_data <- data.frame(
      inventory_year = future_years,
      county_name = unique(historical_data$county_name),
      Tree = if (restoration_factor == 0) {
        rep(inventory_end$Tree, length(future_years))  # Keep Tree constant if no restoration
      } else {
        # Sigmoidal increase in Tree area after planting delay
        sapply(future_years, function(year) {
          if (year < planting_start_year) {
            inventory_end$Tree  # No change before planting start year
          } else {
            # Calculate the cumulative effect of the logistic growth curve
            growth = logistic_growth(
              t = year,
              K = restoration_area,  # Total area to convert to Tree
              r = 4 / planting_time,  # Growth rate (adjusted for planting time)
              t0 = planting_start_year + planting_time / 2,  # Center the curve
              start_year = planting_start_year,
              end_year = planting_start_year + planting_time
            )
            # Constrain growth to ensure it does not exceed available cropland
            min(inventory_end$Tree + growth, inventory_end$Tree + restoration_area)
          }
        })
      },
      Cropland = sapply(future_years, function(year) {
        if (year < planting_start_year) {
          inventory_end$Cropland  # No change before planting start year
        } else {
          # Calculate reductions for Tree and Built_Up
          reduction_tree <- if (restoration_factor == 0) {
            0  # No reduction for Tree if restoration area is 0
          } else {
            logistic_growth(
              t = year,
              K = restoration_area,  # Total area to convert to Tree
              r = 4 / planting_time,  # Growth rate (adjusted for planting time)
              t0 = planting_start_year + planting_time / 2,  # Center the curve
              start_year = planting_start_year,
              end_year = planting_start_year + planting_time
            )
          }
          reduction_built_up <- if (development_factor == 0) {
            0  # No reduction for Built_Up if development area is 0
          } else {
            simple_sigmoid(
              t = year,
              K = development_area,  # Total area to convert to Built_Up
              r = 0.1,  # Fixed growth rate for simplicity
              midpoint = mean(range(future_years))  # Midpoint of future_years
            )
          }
          # Total reduction is the sum of both components
          total_reduction <- reduction_tree + reduction_built_up
          # Constrain reduction to ensure it does not exceed available cropland
          max(inventory_end$Cropland - total_reduction, inventory_end$Cropland - (restoration_area + development_area))
        }
      }),
      Built_Up = if (development_factor == 0) {
        rep(inventory_end$Built_Up, length(future_years))  # Keep Built_Up constant if no development
      } else {
        # Simpler sigmoid for Built_Up area changes
        sapply(future_years, function(year) {
          # Calculate the midpoint of the future_years vector
          midpoint <- mean(range(future_years))
          # Use a fixed sigmoid curve for Built_Up
          growth = simple_sigmoid(
            t = year,
            K = development_area,  # Total area to convert to Built_Up
            r = 0.1,  # Fixed growth rate for simplicity
            midpoint = midpoint
          )
          # Constrain growth to ensure it does not exceed available cropland
          min(inventory_end$Built_Up + growth, inventory_end$Built_Up + development_area)
        })
      },
      Grassland = rep(inventory_end$Grassland, length(future_years)),
      Wetland = rep(inventory_end$Wetland, length(future_years)),
      Urban_Tree = rep(inventory_end$Urban_Tree, length(future_years)),
      Urban_Grassland = rep(inventory_end$Urban_Grassland, length(future_years)),
      Bare = rep(inventory_end$Bare, length(future_years)),
      Shrubland = rep(inventory_end$Shrubland, length(future_years)),
      Water = rep(inventory_end$Water, length(future_years))
    )

    # Apply planting delay (Tree and Cropland are already handled above)
    future_data <- future_data %>%
      rowwise() %>%
      mutate(TOTAL = sum(c_across(c(Bare, Built_Up, Urban_Grassland, Urban_Tree,
                                    Cropland, Grassland, Shrubland, Tree, Water,
                                    Wetland)), na.rm = T)) %>%
      ungroup()

    # Combine historical and projected data
    combined_data <- bind_rows(historical_data, future_data)
    combined_data
  })

  # Render Wedge Plot
  output$wedgePlot <- renderPlot({
    combined_data <- projected_data()

    # Transform data for ggplot (long format)
    plot_data <- combined_data %>%
      pivot_longer(cols = -c(inventory_year,county_name), names_to = "land_cover_type", values_to = "area") %>%
      mutate(land_cover_type = factor(land_cover_type,
                                      levels =
                                        c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
                                          "Cropland", "Shrubland", "Bare", "Built_Up", "Water")
      )) %>%
      filter(land_cover_type != "TOTAL")


    ggplot(plot_data) +
      geom_area(aes(x = inventory_year, y = area, fill = land_cover_type), alpha=0.5) +
      geom_area(data=. %>% filter(inventory_year>=2023),
                mapping = aes(x = inventory_year, y = area, fill = land_cover_type)) +
      geom_vline(xintercept = 2023, linetype = "dashed") +
      scale_fill_manual(
        values = c(
          "Tree" = "#4CAF50",
          "Grassland" = "#FFEB3B",
          "Wetland" = "#75D4D9",
          "Urban_Tree" = "#B6E39A",
          "Urban_Grassland" = "#D4CA6F",
          "Bare" = "#A9A9A9",
          "Built_Up" = "#FF5733",
          "Cropland" = "#FFD700",
          "Shrubland" = "#8B4513",
          "Water" = "#1E90FF"
        ),
        breaks = c(
          "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
          "Bare", "Built_Up", "Cropland", "Shrubland", "Water"
        ),
        labels = c(
          "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland",
          "Bare", "Built Up", "Cropland", "Shrubland", "Water"
        )
      ) +
      labs(
        title = paste("Land Cover Change Projection for", input$selected_county, " County"),
        x = "Year",
        y = "Area (sq. km)",
        fill = "Land Cover Type"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(2000, 2050, by = 5)) +
      scale_y_continuous(
        labels = scales::label_number(scale = 1e-3, suffix = "k", accuracy = 0.1)
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )



  })

  # Render Sequestration Plot
  output$sequestrationPlot <- renderPlot({
    combined_data <- projected_data()

    # Transform data for ggplot (long format)
    plot_data <- combined_data %>%
      pivot_longer(cols = -c(inventory_year,county_name), names_to = "land_cover_type", values_to = "area") %>%
      mutate(land_cover_type = factor(land_cover_type,
                                      levels =
                                        c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
                                          "Cropland", "Shrubland", "Bare", "Built_Up", "Water")
      )) %>%
      filter(land_cover_type != "TOTAL")


    # Compute C sequestration and stock potential for natural systems sectors
    plot_data <-
      plot_data %>%
      left_join(., land_cover_c, by = join_by(land_cover_type)) %>%
      filter(!is.na(seq_mtco2e_sqkm)) %>%
      mutate(
        sequestration_potential = area * seq_mtco2e_sqkm,
        stock_potential = area * stock_mtco2e_sqkm
      ) %>%
      dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm))



    ggplot(plot_data) +
      geom_area(aes(x = inventory_year, y = sequestration_potential, fill = land_cover_type), alpha=0.5) +
      geom_area(data=. %>% filter(inventory_year>=2023),
                mapping = aes(x = inventory_year, y = sequestration_potential, fill = land_cover_type)) +
      geom_vline(xintercept = 2023, linetype = "dashed") +
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
        )
      ) +
      labs(
        title = paste("Sequestration potential for", input$selected_county, " County"),
        x = "Year",
        y = expression("Metric tons"~CO[2]*e),
        fill = "Land Cover Type"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(2000, 2050, by = 5)) +
      scale_y_continuous(
        labels = scales::label_number(scale = 1e-3, suffix = "k", accuracy = 1)
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )



  })

}

# Run the app
shinyApp(ui = ui, server = server)




