# Clear old variables
rm(list = ls())

# Load required packages --------------------------------------------------
# List the packages you'll need
ListOfPackages <- c("tidyverse", "plotly", "patchwork", "usethis", "readr", "shiny",
                    "tidyr", "purrr", "scales", "shinyWidgets",
                    "bslib")

# From this list, check any that aren't currently installed
newPackages <- ListOfPackages[!(ListOfPackages %in% installed.packages()[,"Package"])]

# If any new packages are not currently loaded, load them now
if(length(newPackages)) install.packages(newPackages)
lapply(ListOfPackages, library, character.only=TRUE)

# Install the released version of councilR from GitHub.
remotes::install_github("Metropolitan-Council/councilR")
library(councilR)


# inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_nature/data/"
inpath <- "https://github.com/Metropolitan-Council/ghg-cprg/raw/main/_nature/data/"

lc_county <- readr::read_rds(paste0(inpath, "nlcd_county_landcover_allyrs.rds"))
land_cover_c <- readr::read_rds(paste0(inpath, "land_cover_carbon.rds"))


# Testing space -----------------------------------------------------------

# Transform data for ggplot (long format)
hist_data <- lc_county %>%
  ungroup() %>%
  dplyr::select(land_cover_type, inventory_year, area, county_name) %>%
  pivot_wider(names_from = land_cover_type, values_from = area) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(c(Bare, Developed_Low, Developed_Med, Developed_High,
                                Urban_Grassland, Urban_Tree,
                                Cropland, Grassland, Tree, Water,
                                Wetland)), na.rm = T)) %>%
  ungroup()  %>%
  # replace NAs with 0
  mutate(across(everything(), ~replace_na(., 0)))

start_year <- head(lc_county$inventory_year,1)
end_year <- tail(lc_county$inventory_year,1)


# UI ----------------------------------------------------------------------
# Define UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  tags$head(
    tags$style(HTML("
      h3 { font-size: 16px; }
      h4 { font-size: 14px; }
      label, .form-control { font-size: 12px; }
    "))
  ),

  titlePanel(paste0("Land Cover Change (",
                    head(lc_county$inventory_year, 1), "-2050)")),

  fluidRow(
    # Sidebar panel (left)
    column(
      width = 3,
      tags$h3("County Selection"),
      selectInput("selected_county",
                  #label="Select county from dropdown:",
                  label=NULL,
                  choices = unique(lc_county$county_name),
                  selected = "Anoka"),
      br(),
      tags$h3("Mitigation Strategies"),

      accordion(
        id = "land_cover_controls",
        open = FALSE,
        multiple = FALSE,
        accordion_panel("Urban Tree Planting",
                        fluidRow(
                          column(10, sliderInput("urbanTree_pct", "Available Urban Tree Area (%)", min = 0, max = 100, value = 0)),
                          column(2, actionButton("reset_urbanTree_pct", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("urbanTree_dep", "When to Deploy?", min = 2025, max = 2045, value = 2025, step = 1, sep = "")),
                          column(2, actionButton("reset_urbanTree_dep", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("urbanTree_comp", "Years to Complete?", min = 5, max = 30, value = 15, step = 5, sep = "")),
                          column(2, actionButton("reset_urbanTree_comp", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        actionButton("reset_urbanTree_sliders", "Reset Values", class = "btn-primary btn-sm")
        ),
        accordion_panel("Lawns to Legumes",
                        fluidRow(
                          column(10, sliderInput("lawns_legumes_pct", "Available Lawn Area (%):", min = 0, max = 100, value = 0)),
                          column(2, actionButton("reset_lawns_legumes_pct", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("lawns_legumes_dep", "When to Deploy?", min = 2025, max = 2045, value = 2025, step = 1, sep = "")),
                          column(2, actionButton("reset_lawns_legumes_dep", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("lawns_legumes_comp", "Years to Complete?", min = 5, max = 30, value = 15, step = 5, sep = "")),
                          column(2, actionButton("reset_lawns_legumes_comp", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        actionButton("reset_lawns_legumes_sliders", "Reset Values", class = "btn-primary btn-sm")
        ),
        accordion_panel("Cropland Restoration",
                        fluidRow(
                          column(10, sliderInput("cropland_restoration_pct", "Available Cropland Area (%):", min = 0, max = 100, value = 0)),
                          column(2, actionButton("reset_cropland_restoration_pct", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("sliderForest", "Portion to Forests (%)", min = 0, max = 100, value = 90)),
                          column(2, actionButton("reset_sliderForest", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, uiOutput("sliderGrassland")),
                          column(2, actionButton("reset_sliderGrassland", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("cropland_dep", "When to Deploy?", min = 2025, max = 2045, value = 2025, step = 1, sep = "")),
                          column(2, actionButton("reset_cropland_dep", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("cropland_comp", "Years to Complete?", min = 5, max = 30, value = 15, step = 5, sep = "")),
                          column(2, actionButton("reset_cropland_comp", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        actionButton("reset_cropland_sliders", "Reset Values", class = "btn-primary btn-sm")

        )
      ),
      br(),
      actionButton("reset_all_sliders", "Reset All")
    ),

    # Main panel (middle)
    column(
      width = 6,
      # tags$h3("Model Projections"),
      plotOutput("sequestrationPlot", height = "375px"),
      plotOutput("wedgePlot", height = "375px")
    ),

    # Summary panel (right)
    column(
      width = 3,
      # tags$h3("Summary Plots"),
      # plotOutput("summaryPlot1", height = "350px"),
      tags$div(
        style = "margin-top: 2em; margin-bottom: 2em; font-size: 20px; line-height: 1.5;",
        uiOutput("summaryText")
      ),
      plotOutput("summaryPlot2", height = "300px")
    )
  )
)


# Define Server
server <- function(input, output, session) {

  output$sliderGrassland <- renderUI({
    sliderInput("sliderGrassland", "Portion to Grasslands (%)", min = 0,  max = 100 - input$sliderForest, value = 10)
  })

  observeEvent(input$reset_urbanTree_pct, {
    updateSliderInput(session, "urbanTree_pct", value = 0)
  })
  observeEvent(input$reset_urbanTree_dep, {
    updateSliderInput(session, "urbanTree_dep", value = 2025)
  })
  observeEvent(input$reset_urbanTree_comp, {
    updateSliderInput(session, "urbanTree_comp", value = 15)
  })

  observeEvent(input$reset_lawns_legumes_pct, {
    updateSliderInput(session, "lawns_legumes_pct", value = 0)
  })
  observeEvent(input$reset_lawns_legumes_dep, {
    updateSliderInput(session, "lawns_legumes_dep", value = 2025)
  })
  observeEvent(input$reset_lawns_legumes_comp, {
    updateSliderInput(session, "lawns_legumes_comp", value = 15)
  })


  observeEvent(input$reset_cropland_restoration_pct, {
    updateSliderInput(session, "cropland_restoration_pct", value = 0)
  })
  observeEvent(input$reset_cropland_dep, {
    updateSliderInput(session, "cropland_dep", value = 2025)
  })
  observeEvent(input$reset_cropland_comp, {
    updateSliderInput(session, "cropland_comp", value = 15)
  })
  observeEvent(input$reset_sliderForest, {
    updateSliderInput(session, "sliderForest", value = 90)
  })
  observeEvent(input$reset_sliderGrassland, {
    updateSliderInput(session, "sliderGrassland", value = 10)
  })

  # Observer to reset sliders to default values
  observeEvent(input$reset_all_sliders, {
    updateSliderInput(session, "cropland_restoration_pct", value = 0)
    updateSliderInput(session, "cropland_comp", value = 15)
    updateSliderInput(session, "cropland_dep", value = 2025)
    updateSliderInput(session, "sliderForest", value = 90)
    updateSliderInput(session, "sliderGrassland", value = 10)

    updateSliderInput(session, "urbanTree_pct", value = 0)
    updateSliderInput(session, "urbanTree_dep", value = 2025)
    updateSliderInput(session, "urbanTree_comp", value = 15)

    updateSliderInput(session, "lawns_legumes_pct", value = 0)
    updateSliderInput(session, "lawns_legumes_dep", value = 2025)
    updateSliderInput(session, "lawns_legumes_comp", value = 15)
  })

  observeEvent(input$reset_urbanTree_sliders, {
    updateSliderInput(session, "urbanTree_pct", value = 0)
    updateSliderInput(session, "urbanTree_dep", value = 2025)
    updateSliderInput(session, "urbanTree_comp", value = 15)
  })

  observeEvent(input$reset_cropland_sliders, {
    updateSliderInput(session, "cropland_restoration_pct", value = 0)
    updateSliderInput(session, "cropland_comp", value = 15)
    updateSliderInput(session, "cropland_dep", value = 2025)
    updateSliderInput(session, "sliderForest", value = 90)
    updateSliderInput(session, "sliderGrassland", value = 10)
  })

  observeEvent(input$reset_lawns_legumes_sliders, {
    updateSliderInput(session, "lawns_legumes_pct", value = 0)
    updateSliderInput(session, "lawns_legumes_dep", value = 2025)
    updateSliderInput(session, "lawns_legumes_comp", value = 15)
  })




  future_years <- 2023:2050



  # Reactive data for selected county
  filtered_data <- reactive({
    hist_data %>%
      filter(county_name == input$selected_county)
  })


  null_data <- reactive({
    # Filter historical data for the selected county
    historical_data <- filtered_data()

    # Get the last row of the filtered data for projections
    inventory_end <- tail(historical_data, 1)

    null_df <-  tibble(data.frame(
      inventory_year = future_years,
      county_name = unique(historical_data$county_name),
      null_Bare = rep(inventory_end$Bare, length(future_years)),
      null_Cropland = rep(inventory_end$Cropland, length(future_years)),
      null_Developed_Low = rep(inventory_end$Developed_Low, length(future_years)),
      null_Developed_Med = rep(inventory_end$Developed_Med, length(future_years)),
      null_Developed_High = rep(inventory_end$Developed_High, length(future_years)),
      null_Grassland = rep(inventory_end$Grassland, length(future_years)),
      null_Tree = rep(inventory_end$Tree, length(future_years)),
      null_Urban_Grassland = rep(inventory_end$Urban_Grassland, length(future_years)),
      null_Urban_Tree = rep(inventory_end$Urban_Tree, length(future_years)),
      null_Water = rep(inventory_end$Water, length(future_years)),
      null_Wetland = rep(inventory_end$Wetland, length(future_years))
    )) %>%
      mutate(
        null_TOTAL = sum(c_across(c(null_Tree, null_Bare, null_Cropland, null_Developed_Low,
                                    null_Developed_Med, null_Developed_High, null_Grassland,
                                    null_Tree, null_Urban_Grassland, null_Urban_Tree,
                                    null_Water, null_Wetland)))
      )

    null_df
  })

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



  # Module 1 - Lawns to Legumes ---------------------------------------------
  mod1_lawnsToLegumes <- reactive({
    lawns_legumes_factor <- input$lawns_legumes_pct / 100

    lawns_legumes_start_year <- input$lawns_legumes_dep

    lawns_legumes_delay <- lawns_legumes_start_year - 2023
    lawns_legumes_time <- input$lawns_legumes_comp

    # Filter historical data for the selected county
    historical_data <- filtered_data()
    null_data <- null_data()

    # Get the last row of the filtered data for projections
    inventory_end <- tail(historical_data, 1)

    # Total Urban_Grassland area in 2022
    total_Urban_Grassland <- inventory_end$Urban_Grassland * 0.75 # adding a hard limit to how much area can be converted

    # Calculate areas for converting Urban_Grassland to Grassland
    lawns_legumes_area <- lawns_legumes_factor * total_Urban_Grassland
    remaining_Urban_Grassland <- total_Urban_Grassland - lawns_legumes_area

    # Sigmoidal growth parameters
    r <- 4 / lawns_legumes_time  # Growth rate is inversely proportional to planting time
    t0 <- lawns_legumes_start_year  # Start year determines when the curve begins


    # Generate projections
    future_data <- data.frame(
      inventory_year = future_years,
      county_name = unique(historical_data$county_name),
      Grassland = if (lawns_legumes_factor == 0) {
        rep(inventory_end$Grassland, length(future_years))  # Keep constant if no restoration
      } else {
        # Sigmoidal increase in area after delay
        sapply(future_years, function(year) {
          if (year < lawns_legumes_start_year) {
            inventory_end$Grassland  # No change before start year
          } else {
            # Calculate the cumulative effect of the logistic growth curve
            growth = logistic_growth(
              t = year,
              K = lawns_legumes_area,  # Total area to convert
              r = 4 / lawns_legumes_time,  # Growth rate (adjusted for completion time)
              t0 = lawns_legumes_start_year + lawns_legumes_time / 2,  # Center the curve
              start_year = lawns_legumes_start_year,
              end_year = lawns_legumes_start_year + lawns_legumes_time
            )
            # Constrain growth to ensure it does not exceed available area
            min(inventory_end$Grassland + growth, inventory_end$Grassland + lawns_legumes_area)
          }
        })
      },
      Urban_Grassland = sapply(future_years, function(year) {
        if (year < lawns_legumes_start_year) {
          inventory_end$Urban_Grassland  # No change before planting start year
        } else {
          # Calculate reductions for Urban_Grassland
          total_reduction <- if (lawns_legumes_factor == 0) {
            0  # No reduction for Tree if restoration area is 0
          } else {
            logistic_growth(
              t = year,
              K = lawns_legumes_area,  # Total area to convert to Tree
              r = 4 / lawns_legumes_time,  # Growth rate (adjusted for planting time)
              t0 = lawns_legumes_start_year + lawns_legumes_time / 2,  # Center the curve
              start_year = lawns_legumes_start_year,
              end_year = lawns_legumes_start_year + lawns_legumes_time
            )
          }
          # Constrain reduction to ensure it does not exceed available cropland
          max(inventory_end$Urban_Grassland - total_reduction, inventory_end$Urban_Grassland - lawns_legumes_area)
        }
      })
    )
    future_data
  })



  # Module 2 - Urban Tree Planting ---------------------------------------------
  mod2_urbanTreePlanting <- reactive({
    urbanTree_factor <- input$urbanTree_pct / 100

    urbanTree_start_year <- input$urbanTree_dep
    urbanTree_delay <- urbanTree_start_year - 2023
    urbanTree_time <- input$urbanTree_comp

    # Filter historical data for the selected county
    historical_data <- filtered_data()
    null_data <- null_data()

    # Get the last row of the filtered data for projections
    inventory_end <- tail(historical_data, 1)

    # Total Developed area in 2022
    # Want to determine how much developed area is available for tree planting based on
    # the degree of imperviousness (low, medium and high) where low is 20-49% impervious,
    # medium is 50-79% impervious and high is 80-100% impervious.
    # I think a probability matrix would be a good idea here.
    # browser()
    # Impervious fractions by class
    plantable_fraction <- c(
      Developed_Low = 0.30,  # 30% plantable area, 70% impervious
      Developed_Med = 0.15,   # 15% plantable area, 85% impervious
      Developed_High = 0.05  # 5% plantable area, 95% impervious
    )

    # Calculate tree-plantable area in each class
    available_tree_area <- sum(
      inventory_end$Developed_Low * plantable_fraction["Developed_Low"],
      inventory_end$Developed_Med * plantable_fraction["Developed_Med"],
      inventory_end$Developed_High * plantable_fraction["Developed_High"]
    )

    # Total area to be converted to Urban_Tree
    urbanTree_area <- urbanTree_factor * available_tree_area


    # Sigmoidal growth parameters
    r <- 4 / urbanTree_time  # Growth rate is inversely proportional to planting time
    t0 <- urbanTree_start_year + urbanTree_time / 2  # Center the curve

    # Total initial developed areas
    developed_vals <- c(
      Developed_Low = inventory_end$Developed_Low * as.numeric(plantable_fraction["Developed_Low"]),
      Developed_Med = inventory_end$Developed_Med * as.numeric(plantable_fraction["Developed_Med"]),
      Developed_High = inventory_end$Developed_High * as.numeric(plantable_fraction["Developed_High"])
    )

    total_plantable <- sum(developed_vals)

    # Proportional share of available area per class
    proportions <- developed_vals / total_plantable

    # Generate projections
    future_data <- data.frame(
      inventory_year = future_years,
      county_name = unique(historical_data$county_name),

      Urban_Tree = sapply(future_years, function(year) {
        if (urbanTree_factor == 0 || year < urbanTree_start_year) {
          inventory_end$Urban_Tree  # No change
        } else {
          growth <- logistic_growth(
            t = year,
            K = urbanTree_area,
            r = 4 / urbanTree_time,
            t0 = t0,
            start_year = urbanTree_start_year,
            end_year = urbanTree_start_year + urbanTree_time
          )
          min(inventory_end$Urban_Tree + growth, inventory_end$Urban_Tree + urbanTree_area)
        }
      }),

      Developed_Low = sapply(future_years, function(year) {
        if (urbanTree_factor == 0 || year < urbanTree_start_year) {
          inventory_end$Developed_Low
        } else {
          reduction <- logistic_growth(
            t = year,
            K = urbanTree_area * proportions["Developed_Low"],
            r = 4 / urbanTree_time,
            t0 = t0,
            start_year = urbanTree_start_year,
            end_year = urbanTree_start_year + urbanTree_time
          )
          max(inventory_end$Developed_Low - reduction, inventory_end$Developed_Low - developed_vals["Developed_Low"])
        }
      }),

      Developed_Med = sapply(future_years, function(year) {
        if (urbanTree_factor == 0 || year < urbanTree_start_year) {
          inventory_end$Developed_Med
        } else {
          reduction <- logistic_growth(
            t = year,
            K = urbanTree_area * proportions["Developed_Med"],
            r = 4 / urbanTree_time,
            t0 = t0,
            start_year = urbanTree_start_year,
            end_year = urbanTree_start_year + urbanTree_time
          )
          max(inventory_end$Developed_Med - reduction, inventory_end$Developed_Med - developed_vals["Developed_Med"])
        }
      }),

      Developed_High = sapply(future_years, function(year) {
        if (urbanTree_factor == 0 || year < urbanTree_start_year) {
          inventory_end$Developed_High
        } else {
          reduction <- logistic_growth(
            t = year,
            K = urbanTree_area * proportions["Developed_High"],
            r = 4 / urbanTree_time,
            t0 = t0,
            start_year = urbanTree_start_year,
            end_year = urbanTree_start_year + urbanTree_time
          )
          max(inventory_end$Developed_High - reduction, inventory_end$Developed_High - developed_vals["Developed_High"])
        }
      })
    )

    future_data
  })

  # Module 3 - Restoration ---------------------------------------------
  mod3_cropRestoration <- reactive({

    tree_share <- input$sliderForest/100
    grassland_share <- input$sliderGrassland/100
    wetland_share <- (100-input$sliderForest-input$sliderGrassland)/100

    # Inputs
    total_restore_factor <- input$cropland_restoration_pct / 100

    # Speed and delay parameters
    cropland_start_year <- input$cropland_dep

    cropland_delay <- cropland_start_year - 2023
    cropland_time <- input$cropland_comp

    # Filter historical data for the selected county
    historical_data <- filtered_data()
    null_data <- null_data()

    # Get the last row of the filtered data for projections
    inventory_end <- tail(historical_data, 1)

    # Total Cropland area in 2022
    total_cropland <- inventory_end$Cropland

    # Calculate areas for converting Urban_Grassland to Grassland
    total_restore_area <- total_restore_factor * total_cropland * 0.75 # adding a hard limit to how much area can be converted
    remaining_cropland <- total_restore_area - total_cropland

    # Calculate share of land that each cover type will gain
    restore_wetland_area <- total_restore_area * wetland_share
    restore_tree_area <- total_restore_area * tree_share
    restore_grassland_area <- total_restore_area * grassland_share



    # Sigmoidal growth parameters
    r <- 4 / cropland_time  # Growth rate is inversely proportional to planting time
    t0 <- cropland_start_year  # Start year determines when the curve begins


    future_data <- data.frame(
      inventory_year = future_years,
      county_name = unique(historical_data$county_name),
      ## FROM
      Cropland = sapply(future_years, function(year) {
        if (year < cropland_start_year) {
          inventory_end$Cropland  # No change before planting start year
        } else {
          # Calculate reductions for Cropland
          total_reduction <- if (total_restore_factor == 0) {
            0  # No reduction for Cropland if restoration area is 0
          } else {
            logistic_growth(
              t = year,
              K = total_restore_area,  # Total area to convert to Tree
              r = 4 / cropland_time,  # Growth rate (adjusted for planting time)
              t0 = cropland_start_year + cropland_time / 2,  # Center the curve
              start_year = cropland_start_year,
              end_year = cropland_start_year + cropland_time
            )
          }
          # Constrain reduction to ensure it does not exceed available cropland
          max(inventory_end$Cropland - total_reduction, inventory_end$Cropland - total_restore_area)
        }
      }),

      Tree = if (total_restore_factor == 0) {
        rep(inventory_end$Tree, length(future_years))  # Keep constant if no restoration
      } else {
        if (restore_tree_area == 0) {
          rep(inventory_end$Tree, length(future_years))  # Keep constant if no restoration
        } else {
          # Sigmoidal increase in area after delay
          sapply(future_years, function(year) {
            if (year < cropland_start_year) {
              inventory_end$Tree  # No change before start year
            } else {
              # Calculate the cumulative effect of the logistic growth curve
              growth = logistic_growth(
                t = year,
                K = restore_tree_area,  # Total area to convert
                r = 4 / cropland_time,  # Growth rate (adjusted for completion time)
                t0 = cropland_start_year + cropland_time / 2,  # Center the curve
                start_year = cropland_start_year,
                end_year = cropland_start_year + cropland_time
              )
              # Constrain growth to ensure it does not exceed available area
              min(inventory_end$Tree + growth, inventory_end$Tree + restore_tree_area)
            }
          })
        }
      },
      Grassland = if (total_restore_factor == 0) {
        rep(inventory_end$Grassland, length(future_years))  # Keep constant if no restoration
      } else {
        if (restore_grassland_area == 0) {
          rep(inventory_end$Grassland, length(future_years))  # Keep constant if no restoration
        } else {
          # Sigmoidal increase in area after delay
          sapply(future_years, function(year) {
            if (year < cropland_start_year) {
              inventory_end$Grassland  # No change before start year
            } else {
              # Calculate the cumulative effect of the logistic growth curve
              growth = logistic_growth(
                t = year,
                K = restore_grassland_area,  # Total area to convert
                r = 4 / cropland_time,  # Growth rate (adjusted for completion time)
                t0 = cropland_start_year + cropland_time / 2,  # Center the curve
                start_year = cropland_start_year,
                end_year = cropland_start_year + cropland_time
              )
              # Constrain growth to ensure it does not exceed available area
              min(inventory_end$Grassland + growth, inventory_end$Grassland + restore_grassland_area)
            }
          })
        }
      },
      Wetland = if (total_restore_factor == 0) {
        rep(inventory_end$Wetland, length(future_years))  # Keep constant if no restoration
      } else {
        if (restore_wetland_area == 0) {
          rep(inventory_end$Wetland, length(future_years))  # Keep constant if no restoration
        } else {
          # Sigmoidal increase in area after delay
          sapply(future_years, function(year) {
            if (year < cropland_start_year) {
              inventory_end$Wetland  # No change before start year
            } else {
              # Calculate the cumulative effect of the logistic growth curve
              growth = logistic_growth(
                t = year,
                K = restore_wetland_area,  # Total area to convert
                r = 4 / cropland_time,  # Growth rate (adjusted for completion time)
                t0 = cropland_start_year + cropland_time / 2,  # Center the curve
                start_year = cropland_start_year,
                end_year = cropland_start_year + cropland_time
              )
              # Constrain growth to ensure it does not exceed available area
              min(inventory_end$Wetland + growth, inventory_end$Wetland + restore_wetland_area)
            }
          })
        }
      }
    )





    future_data
  })


  # Reactive data for projections
  projected_data <- reactive({
    projected_mod1 <- mod1_lawnsToLegumes()
    projected_mod2 <- mod2_urbanTreePlanting()
    projected_mod3 <- mod3_cropRestoration()

    # Filter historical data for the selected county
    historical_data <- filtered_data()
    null_data <- null_data()


    # Get the last row of the filtered data for projections
    inventory_end <- tail(historical_data, 1)

    # Generate projections
    future_data <- data.frame(
      inventory_year = future_years,
      county_name = unique(historical_data$county_name),
      Grassland = (projected_mod1$Grassland - inventory_end$Grassland) + projected_mod3$Grassland,
      Urban_Grassland = projected_mod1$Urban_Grassland,
      Developed_Low = projected_mod2$Developed_Low,
      Developed_Med = projected_mod2$Developed_Med,
      Developed_High = projected_mod2$Developed_High,
      Urban_Tree = projected_mod2$Urban_Tree,
      Cropland = projected_mod3$Cropland,
      Tree = projected_mod3$Tree,
      Wetland = projected_mod3$Wetland,
      Bare = rep(inventory_end$Bare, length(future_years)),
      Water = rep(inventory_end$Water, length(future_years))
    )

    # Apply planting delay (Tree and Cropland are already handled above)
    future_data <- future_data %>%
      rowwise() %>%
      mutate(TOTAL = sum(c_across(c(Bare, Developed_Low, Developed_Med, Developed_High,
                                    Urban_Grassland, Urban_Tree,
                                    Cropland, Grassland, Tree, Water,
                                    Wetland)), na.rm = T)) %>%
      ungroup()

    # Combine historical and projected data
    combined_data <- bind_rows(historical_data, future_data)
    combined_data
  })




  theme_settings <- function() {
    theme(
      plot.title = element_text(size=17, face="bold"),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14)
    )
  }

  plot_colors <- c(
    "Tree" = "#4CAF50",
    "Grassland" = "#FFEB3B",
    "Wetland" = "#75D4D9",
    "Urban_Tree" = "#B6E39A",
    "Urban_Grassland" = "#D4CA6F",
    "Bare" = "#A9A9A9",
    "Developed_Low" = "#FFB9A8",
    "Developed_Med" = "#FF8E75",
    "Developed_High" = "#FF5733",
    "Cropland" = "orange",
    "Water" = "#1E90FF"
  )


  # Render Wedge Plot
  output$wedgePlot <- renderPlot({
    combined_data <- projected_data()

    # Transform data for ggplot (long format)
    plot_data <- combined_data %>%
      pivot_longer(cols = -c(inventory_year,county_name), names_to = "land_cover_type", values_to = "area") %>%
      mutate(land_cover_type = factor(land_cover_type,
                                      levels =
                                        c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
                                          "Cropland", "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Water")
      )) %>%
      filter(land_cover_type != "TOTAL")

    ggplot(plot_data) +
      geom_area(aes(x = inventory_year, y = area, fill = land_cover_type), alpha=0.5, show.legend = F) +
      geom_area(aes(x = inventory_year, y = area, color = land_cover_type), fill=NA, linewidth=1, show.legend = F) +
      geom_vline(xintercept = 2023, linetype = "dashed", alpha=0.5) +
      scale_fill_manual(
        values = plot_colors,
        breaks = c(
          "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
          "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Cropland", "Water"
        ),
        labels = c(
          "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland",
          "Bare", "Developed (Low)", "Developed (Med)", "Developed (High)", "Cropland", "Water"
        )
      ) +
      scale_color_manual(
        values = plot_colors,
        breaks = c(
          "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
          "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Cropland", "Water"
        ),
        labels = c(
          "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland",
          "Bare", "Developed (Low)", "Developed (Med)", "Developed (High)", "Cropland", "Water"
        )
      ) +
      labs(
        title = paste("Land Cover Change Projection for", input$selected_county, "County"),
        x = "Year",
        y = "Area (sq. km)",
        fill = "Land Cover Type"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(2000, 2050, by = 5)) +
      scale_y_continuous(
        labels = scales::label_number(scale = 1e-3, suffix = "k", accuracy = 0.1)
      ) +
      theme_settings()


    p1 <- ggplot(plot_data) +
      geom_area(aes(x = inventory_year, y = area, fill = land_cover_type), alpha=0.5, show.legend = F) +
      geom_area(aes(x = inventory_year, y = area, color = land_cover_type), fill=NA, linewidth=1, show.legend = F) +
      geom_vline(xintercept = 2023, linetype = "dashed", alpha=0.5) +
      scale_fill_manual(
        values = plot_colors,
        breaks = c(
          "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
          "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Cropland", "Water"
        ),
        labels = c(
          "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland",
          "Bare", "Developed (Low)", "Developed (Med)", "Developed (High)", "Cropland", "Water"
        )
      ) +
      scale_color_manual(
        values = plot_colors,
        breaks = c(
          "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
          "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Cropland", "Water"
        ),
        labels = c(
          "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland",
          "Bare", "Developed (Low)", "Developed (Med)", "Developed (High)", "Cropland", "Water"
        )
      ) +
      labs(
        title = NULL,
        x = "Year",
        y = "Area (sq. km)",
        fill = "Land Cover Type"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(2000, 2050, by = 5)) +
      scale_y_continuous(
        labels = scales::label_number(scale = 1e-3, suffix = "k", accuracy = 0.1)
      ) +
      theme_settings()



    p2 <- rbind(
      plot_data %>%
        filter(inventory_year == future_years[1]-1) %>%
        mutate(tag = factor("initial", levels=c("initial","final"))),
      plot_data %>%
        filter(inventory_year == 2050) %>%
        mutate(tag = factor("final", levels=c("initial","final")))
    ) %>% arrange(tag) %>%
      # filter(!is.na(sequestration_potential)) %>%
      mutate(inventory_year = factor(inventory_year, levels=c(as.character(future_years[1]-1), as.character(2050)))) %>%
      ggplot() +
      theme_minimal() +
      geom_col(aes(x = inventory_year, y = area, fill = land_cover_type), alpha=0.5) +
      geom_col(aes(x = inventory_year, y = area, color = land_cover_type), fill=NA, linewidth=1, show.legend = F) +
      scale_fill_manual(
        values = plot_colors,
        breaks = c(
          "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
          "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Cropland", "Water"
        ),
        labels = c(
          "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland",
          "Bare", "Developed (Low)", "Developed (Med)", "Developed (High)", "Cropland", "Water"
        )
      ) +
      scale_color_manual(
        values = plot_colors,
        breaks = c(
          "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
          "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Cropland", "Water"
        ),
        labels = c(
          "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland",
          "Bare", "Developed (Low)", "Developed (Med)", "Developed (High)", "Cropland", "Water"
        )
      ) +
      labs(
        # title = paste("Change in sequestration potential for", input$selected_county, "County"),
        x = NULL,
        y = NULL,
        fill = "Land Cover Type"
      ) +
      theme_settings() +
      theme(
        axis.text.y = element_blank()
      )




    wrap_elements(p1 + p2 + plot_layout(widths=c(9,1))) +
      labs(title = paste("Land Cover Change Projection for", input$selected_county, "County")) +
      theme_minimal() +
      theme_settings()


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
                                          "Cropland", "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Water")
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


    baseline_sequestration <- plot_data %>%
      filter(inventory_year == 2022) %>%
      ungroup() %>%
      pull(sequestration_potential) %>% sum()


    final_sequestration <- plot_data %>%
      filter(inventory_year == 2050) %>%
      ungroup() %>%
      pull(sequestration_potential) %>% sum()


    p1 <- ggplot(plot_data) +
      geom_area(aes(x = inventory_year, y = sequestration_potential, fill = land_cover_type), alpha=0.5, show.legend = F) +
      geom_area(aes(x = inventory_year, y = sequestration_potential, color = land_cover_type), fill=NA, linewidth=1, show.legend = F) +

      geom_vline(xintercept = 2023, linetype = "dashed", alpha=0.5) +
      geom_hline(yintercept = baseline_sequestration, linetype = "dashed", alpha=0.5) +
      geom_hline(yintercept = final_sequestration, linetype = "dashed", alpha=0.5) +

      scale_fill_manual(
        values = plot_colors,
        breaks =
          c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland"),
        labels =
          c("Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland")
      ) +
      scale_color_manual(
        values = plot_colors,
        breaks =
          c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland"),
        labels =
          c("Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland")
      ) +
      labs(
        title = NULL,
        x = "Year",
        y = expression("Metric tons"~CO[2]*e),
        fill = "Land Cover Type"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(2000, 2050, by = 5)) +
      scale_y_continuous(
        labels = scales::label_number(scale = 1e-3, suffix = "k", accuracy = 1)
      ) +
      theme_settings()



    p2 <- rbind(
      plot_data %>%
        filter(inventory_year == future_years[1]-1) %>%
        mutate(tag = factor("initial", levels=c("initial","final"))),
      plot_data %>%
        filter(inventory_year == 2050) %>%
        mutate(tag = factor("final", levels=c("initial","final")))
    ) %>% arrange(tag) %>%
      filter(!is.na(sequestration_potential)) %>%
      mutate(inventory_year = factor(inventory_year, levels=c(as.character(future_years[1]-1), as.character(2050)))) %>%
      ggplot() +
      theme_minimal() +
      geom_col(aes(x = inventory_year, y = sequestration_potential, fill = land_cover_type), alpha=0.5) +
      geom_col(aes(x = inventory_year, y = sequestration_potential, color = land_cover_type), fill=NA, linewidth=1, show.legend = F) +
      scale_fill_manual(
        values = plot_colors,
        breaks =
          c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland"),
        labels =
          c("Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland")
      ) +
      scale_color_manual(
        values = plot_colors,
        breaks =
          c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland"),
        labels =
          c("Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland")
      ) +
      labs(
        # title = paste("Change in sequestration potential for", input$selected_county, "County"),
        x = NULL,
        y = NULL,
        fill = "Land Cover Type"
      ) +
      theme_settings() +
      theme(
        axis.text.y = element_blank()
      )




    wrap_elements(p1 + p2 + plot_layout(widths=c(9,1))) +
      labs(title = paste("Sequestration potential for", input$selected_county, "County")) +
      theme_minimal() +
      theme_settings()

  })

  output$summaryPlot1 <- renderPlot({
    combined_data <- projected_data()

    # Transform data for ggplot (long format)
    plot_data <- combined_data %>%
      pivot_longer(cols = -c(inventory_year,county_name), names_to = "land_cover_type", values_to = "area") %>%
      mutate(land_cover_type = factor(land_cover_type,
                                      levels =
                                        c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
                                          "Cropland", "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Water")
      )) %>%
      filter(land_cover_type != "TOTAL")


    # Compute C sequestration and stock potential for natural systems sectors
    plot_data <-
      plot_data %>%
      left_join(., land_cover_c, by = join_by(land_cover_type)) %>%
      # filter(!is.na(seq_mtco2e_sqkm)) %>%
      mutate(
        sequestration_potential = area * seq_mtco2e_sqkm,
        stock_potential = area * stock_mtco2e_sqkm
      ) %>%
      dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm))


    plot_data <- rbind(
      plot_data %>%
        filter(inventory_year == future_years[1]-1) %>%
        mutate(tag = factor("initial", levels=c("initial","final"))),
      plot_data %>%
            filter(inventory_year == 2050) %>%
              mutate(tag = factor("final", levels=c("initial","final")))
    ) %>% arrange(tag)



    plot_data %>%
      filter(!is.na(sequestration_potential)) %>%
      mutate(inventory_year = factor(inventory_year, levels=c(as.character(future_years[1]-1), as.character(2050)))) %>%
      ggplot() +
      theme_minimal() +
      geom_col(aes(x = inventory_year, y = sequestration_potential, fill = land_cover_type), alpha=0.5) +
      geom_col(aes(x = inventory_year, y = sequestration_potential, color = land_cover_type), fill=NA, linewidth=1, show.legend = F) +
      scale_fill_manual(
        values = plot_colors,
        breaks =
          c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland"),
        labels =
          c("Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland")
      ) +
      scale_color_manual(
        values = plot_colors,
        breaks =
          c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland"),
        labels =
          c("Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland")
      ) +
      scale_y_continuous(
        labels = scales::label_number(scale = 1e-3, suffix = "k", accuracy = 1)
      ) +
      labs(
        title = "",
        x = "Year",
        y = NULL,
        fill = "Land Cover Type"
      ) +
      theme_settings()


      # browser()

  })


  output$summaryText <- renderUI({
    # Transform data for summary
    summary_data <- projected_data() %>%
      pivot_longer(cols = -c(inventory_year,county_name), names_to = "land_cover_type", values_to = "area") %>%
      mutate(land_cover_type = factor(land_cover_type,
                                      levels =
                                        c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
                                          "Cropland", "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Water")
      )) %>%
      filter(land_cover_type != "TOTAL")


    # Compute C sequestration and stock potential for natural systems sectors
    summary_data <-
      summary_data %>%
      left_join(., land_cover_c, by = join_by(land_cover_type)) %>%
      filter(!is.na(seq_mtco2e_sqkm)) %>%
      mutate(
        sequestration_potential = area * seq_mtco2e_sqkm,
        stock_potential = area * stock_mtco2e_sqkm
      ) %>%
      dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm))


    baseline_sequestration <- summary_data %>%
      filter(inventory_year == 2022) %>%
      ungroup() %>%
      pull(sequestration_potential) %>% sum()


    final_sequestration <- summary_data %>%
      filter(inventory_year == 2050) %>%
      ungroup() %>%
      pull(sequestration_potential) %>% sum()


    seq_change_pct <- round(((final_sequestration-baseline_sequestration)/baseline_sequestration)*100,1)
    seq_change_actual <- round(abs(final_sequestration-baseline_sequestration)/1000,0)

    county_name <- unique(summary_data$county_name)

    if (seq_change_pct > 0) {
      HTML(paste0("+",
                  seq_change_pct,
                  "% increase (+",
                  seq_change_actual,
                  "k metric tons CO<sub>2</sub>e) in C sequestered by natural systems in ",
                  county_name,
                  " County by 2050."))
    } else if (seq_change_pct < 0) {
      HTML(paste0(seq_change_pct,
                  "% decrease (-",
                  seq_change_actual,
                  "k metric tons CO<sub>2</sub>e) in C sequestered by natural systems in ",
                  county_name,
                  " County by 2050."))
    } else {
      HTML("No change in C sequestered by natural systems.")
    }


  })


}

## Run the app -----------
shinyApp(ui = ui, server = server)



















# Working code below ------------------------------------------------------
# Transform data for ggplot (long format)
hist_data <- lc_county %>%
  ungroup() %>%
  dplyr::select(land_cover_type, inventory_year, area, county_name) %>%
  pivot_wider(names_from = land_cover_type, values_from = area) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(c(Bare, Developed_Low, Developed_Med, Developed_High,
                                Urban_Grassland, Urban_Tree,
                                Cropland, Grassland, Tree, Water,
                                Wetland)), na.rm = T)) %>%
  ungroup()  %>%
  # replace NAs with 0
  mutate(across(everything(), ~replace_na(., 0)))


# UI ----------------------------------------------------------------------
# Define UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  tags$head(
    tags$style(HTML("
      h3 { font-size: 16px; }
      h4 { font-size: 14px; }
      label, .form-control { font-size: 12px; }
    "))
  ),

  titlePanel(paste0("Land Cover Change (",
                    head(lc_county$inventory_year, 1), "-2050)")),

  fluidRow(
    # Sidebar panel (left)
    column(
      width = 3,
      tags$h3("County Selection"),
      selectInput("selected_county",
                  #label="Select county from dropdown:",
                  label=NULL,
                  choices = unique(lc_county$county_name),
                  selected = "Anoka"),
      br(),
      tags$h3("Mitigation Strategies"),
      accordion(
        id = "land_cover_controls",
        open = FALSE,
        multiple = FALSE,
        accordion_panel("Urban Tree Planting",
                        fluidRow(
                          column(10, sliderInput("urbanTree_pct", "Available Urban Tree Area (%)", min = 0, max = 100, value = 0)),
                          column(2, actionButton("reset_urbanTree_pct", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("urbanTree_dep", "When to Deploy?", min = 2025, max = 2045, value = 2025, step = 1, sep = "")),
                          column(2, actionButton("reset_urbanTree_dep", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("urbanTree_comp", "Years to Complete?", min = 5, max = 30, value = 15, step = 5, sep = "")),
                          column(2, actionButton("reset_urbanTree_comp", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        )
        ),
        accordion_panel("Lawns to Legumes",
                        fluidRow(
                          column(10, sliderInput("lawns_legumes_pct", "Available Lawn Area (%):", min = 0, max = 100, value = 0)),
                          column(2, actionButton("reset_lawns_legumes_pct", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("lawns_legumes_dep", "When to Deploy?", min = 2025, max = 2045, value = 2025, step = 1, sep = "")),
                          column(2, actionButton("reset_lawns_legumes_dep", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("lawns_legumes_comp", "Years to Complete?", min = 5, max = 30, value = 15, step = 5, sep = "")),
                          column(2, actionButton("reset_lawns_legumes_comp", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        )
        ),
        accordion_panel("Cropland Restoration",
                        fluidRow(
                          column(10, sliderInput("cropland_restoration_pct", "Available Cropland Area (%):", min = 0, max = 100, value = 0)),
                          column(2, actionButton("reset_cropland_restoration_pct", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("sliderForest", "Portion to Forests (%)", min = 0, max = 100, value = 90)),
                          column(2, actionButton("reset_sliderForest", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, uiOutput("sliderGrassland")),
                          column(2, actionButton("reset_sliderGrassland", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("cropland_dep", "When to Deploy?", min = 2025, max = 2045, value = 2025, step = 1, sep = "")),
                          column(2, actionButton("reset_cropland_dep", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("cropland_comp", "Years to Complete?", min = 5, max = 30, value = 15, step = 5, sep = "")),
                          column(2, actionButton("reset_cropland_comp", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        )

        )
      ),
      br(),
      actionButton("reset_all_sliders", "Restore Defaults")
    ),

    # Main panel (middle)
    column(
      width = 6,
      # tags$h3("Model Projections"),
      plotOutput("sequestrationPlot", height = "350px"),
      plotOutput("wedgePlot", height = "350px")
    ),

    # Summary panel (right)
    column(
      width = 3,
      # tags$h3("Summary Plots"),
      # plotOutput("summaryPlot1", height = "350px"),
      tags$div(
        style = "margin-top: 2em; margin-bottom: 2em; font-size: 20px; line-height: 1.5;",
        uiOutput("summaryText")
      ),
      plotOutput("summaryPlot2", height = "300px")
    )
  )
)


# Define Server
server <- function(input, output, session) {

  output$sliderGrassland <- renderUI({
    sliderInput("sliderGrassland", "Portion to Grasslands (%)", min = 0,  max = 100 - input$sliderForest, value = 10)
  })

  observeEvent(input$reset_urbanTree_pct, {
    updateSliderInput(session, "urbanTree_pct", value = 0)
  })
  observeEvent(input$reset_urbanTree_dep, {
    updateSliderInput(session, "urbanTree_dep", value = 2025)
  })
  observeEvent(input$reset_urbanTree_comp, {
    updateSliderInput(session, "urbanTree_comp", value = 15)
  })

  observeEvent(input$reset_lawns_legumes_pct, {
    updateSliderInput(session, "lawns_legumes_pct", value = 0)
  })
  observeEvent(input$reset_lawns_legumes_dep, {
    updateSliderInput(session, "lawns_legumes_dep", value = 2025)
  })
  observeEvent(input$reset_lawns_legumes_comp, {
    updateSliderInput(session, "lawns_legumes_comp", value = 15)
  })


  observeEvent(input$reset_cropland_restoration_pct, {
    updateSliderInput(session, "cropland_restoration_pct", value = 0)
  })
  observeEvent(input$reset_cropland_dep, {
    updateSliderInput(session, "cropland_dep", value = 2025)
  })
  observeEvent(input$reset_cropland_comp, {
    updateSliderInput(session, "cropland_comp", value = 15)
  })
  observeEvent(input$reset_sliderForest, {
    updateSliderInput(session, "sliderForest", value = 90)
  })
  observeEvent(input$reset_sliderGrassland, {
    updateSliderInput(session, "sliderGrassland", value = 10)
  })

  # Observer to reset sliders to default values
  observeEvent(input$reset_all_sliders, {
    updateSliderInput(session, "cropland_restoration_pct", value = 0)
    updateSliderInput(session, "cropland_comp", value = 15)
    updateSliderInput(session, "cropland_dep", value = 2025)
    updateSliderInput(session, "sliderForest", value = 90)
    updateSliderInput(session, "sliderGrassland", value = 10)

    updateSliderInput(session, "urbanTree_pct", value = 0)
    updateSliderInput(session, "urbanTree_dep", value = 2025)
    updateSliderInput(session, "urbanTree_comp", value = 15)

    updateSliderInput(session, "lawns_legumes_pct", value = 0)
    updateSliderInput(session, "lawns_legumes_dep", value = 2025)
    updateSliderInput(session, "lawns_legumes_comp", value = 15)
  })


  future_years <- 2023:2050



  # Reactive data for selected county
  filtered_data <- reactive({
    hist_data %>%
      filter(county_name == input$selected_county)
  })


  null_data <- reactive({
    # Filter historical data for the selected county
    historical_data <- filtered_data()

    # Get the last row of the filtered data for projections
    inventory_end <- tail(historical_data, 1)

    null_df <-  tibble(data.frame(
      inventory_year = future_years,
      county_name = unique(historical_data$county_name),
      null_Bare = rep(inventory_end$Bare, length(future_years)),
      null_Cropland = rep(inventory_end$Cropland, length(future_years)),
      null_Developed_Low = rep(inventory_end$Developed_Low, length(future_years)),
      null_Developed_Med = rep(inventory_end$Developed_Med, length(future_years)),
      null_Developed_High = rep(inventory_end$Developed_High, length(future_years)),
      null_Grassland = rep(inventory_end$Grassland, length(future_years)),
      null_Tree = rep(inventory_end$Tree, length(future_years)),
      null_Urban_Grassland = rep(inventory_end$Urban_Grassland, length(future_years)),
      null_Urban_Tree = rep(inventory_end$Urban_Tree, length(future_years)),
      null_Water = rep(inventory_end$Water, length(future_years)),
      null_Wetland = rep(inventory_end$Wetland, length(future_years))
    )) %>%
      mutate(
        null_TOTAL = sum(c_across(c(null_Tree, null_Bare, null_Cropland, null_Developed_Low,
                                    null_Developed_Med, null_Developed_High, null_Grassland,
                                    null_Tree, null_Urban_Grassland, null_Urban_Tree,
                                    null_Water, null_Wetland)))
      )

    null_df
  })

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



  # Module 1 - Lawns to Legumes ---------------------------------------------
  mod1_lawnsToLegumes <- reactive({
    lawns_legumes_factor <- input$lawns_legumes_pct / 100

    lawns_legumes_start_year <- input$lawns_legumes_dep

    lawns_legumes_delay <- lawns_legumes_start_year - 2023
    lawns_legumes_time <- input$lawns_legumes_comp

    # Filter historical data for the selected county
    historical_data <- filtered_data()
    null_data <- null_data()

    # Get the last row of the filtered data for projections
    inventory_end <- tail(historical_data, 1)

    # Total Urban_Grassland area in 2022
    total_Urban_Grassland <- inventory_end$Urban_Grassland * 0.75 # adding a hard limit to how much area can be converted

    # Calculate areas for converting Urban_Grassland to Grassland
    lawns_legumes_area <- lawns_legumes_factor * total_Urban_Grassland
    remaining_Urban_Grassland <- total_Urban_Grassland - lawns_legumes_area

    # Sigmoidal growth parameters
    r <- 4 / lawns_legumes_time  # Growth rate is inversely proportional to planting time
    t0 <- lawns_legumes_start_year  # Start year determines when the curve begins


    # Generate projections
    future_data <- data.frame(
      inventory_year = future_years,
      county_name = unique(historical_data$county_name),
      Grassland = if (lawns_legumes_factor == 0) {
        rep(inventory_end$Grassland, length(future_years))  # Keep constant if no restoration
      } else {
        # Sigmoidal increase in area after delay
        sapply(future_years, function(year) {
          if (year < lawns_legumes_start_year) {
            inventory_end$Grassland  # No change before start year
          } else {
            # Calculate the cumulative effect of the logistic growth curve
            growth = logistic_growth(
              t = year,
              K = lawns_legumes_area,  # Total area to convert
              r = 4 / lawns_legumes_time,  # Growth rate (adjusted for completion time)
              t0 = lawns_legumes_start_year + lawns_legumes_time / 2,  # Center the curve
              start_year = lawns_legumes_start_year,
              end_year = lawns_legumes_start_year + lawns_legumes_time
            )
            # Constrain growth to ensure it does not exceed available area
            min(inventory_end$Grassland + growth, inventory_end$Grassland + lawns_legumes_area)
          }
        })
      },
      Urban_Grassland = sapply(future_years, function(year) {
        if (year < lawns_legumes_start_year) {
          inventory_end$Urban_Grassland  # No change before planting start year
        } else {
          # Calculate reductions for Urban_Grassland
          total_reduction <- if (lawns_legumes_factor == 0) {
            0  # No reduction for Tree if restoration area is 0
          } else {
            logistic_growth(
              t = year,
              K = lawns_legumes_area,  # Total area to convert to Tree
              r = 4 / lawns_legumes_time,  # Growth rate (adjusted for planting time)
              t0 = lawns_legumes_start_year + lawns_legumes_time / 2,  # Center the curve
              start_year = lawns_legumes_start_year,
              end_year = lawns_legumes_start_year + lawns_legumes_time
            )
          }
          # Constrain reduction to ensure it does not exceed available cropland
          max(inventory_end$Urban_Grassland - total_reduction, inventory_end$Urban_Grassland - lawns_legumes_area)
        }
      })
    )
    future_data
  })



  # Module 2 - Urban Tree Planting ---------------------------------------------
  mod2_urbanTreePlanting <- reactive({
    urbanTree_factor <- input$urbanTree_pct / 100

    urbanTree_start_year <- input$urbanTree_dep
    urbanTree_delay <- urbanTree_start_year - 2023
    urbanTree_time <- input$urbanTree_comp

    # Filter historical data for the selected county
    historical_data <- filtered_data()
    null_data <- null_data()

    # Get the last row of the filtered data for projections
    inventory_end <- tail(historical_data, 1)

    # Total Developed area in 2022
    # Want to determine how much developed area is available for tree planting based on
    # the degree of imperviousness (low, medium and high) where low is 20-49% impervious,
    # medium is 50-79% impervious and high is 80-100% impervious.
    # I think a probability matrix would be a good idea here.
    # browser()
    # Impervious fractions by class
    plantable_fraction <- c(
      Developed_Low = 0.30,  # 30% plantable area, 70% impervious
      Developed_Med = 0.15,   # 15% plantable area, 85% impervious
      Developed_High = 0.05  # 5% plantable area, 95% impervious
    )

    # Calculate tree-plantable area in each class
    available_tree_area <- sum(
      inventory_end$Developed_Low * plantable_fraction["Developed_Low"],
      inventory_end$Developed_Med * plantable_fraction["Developed_Med"],
      inventory_end$Developed_High * plantable_fraction["Developed_High"]
    )

    # Total area to be converted to Urban_Tree
    urbanTree_area <- urbanTree_factor * available_tree_area


    # Sigmoidal growth parameters
    r <- 4 / urbanTree_time  # Growth rate is inversely proportional to planting time
    t0 <- urbanTree_start_year + urbanTree_time / 2  # Center the curve

    # Total initial developed areas
    developed_vals <- c(
      Developed_Low = inventory_end$Developed_Low * as.numeric(plantable_fraction["Developed_Low"]),
      Developed_Med = inventory_end$Developed_Med * as.numeric(plantable_fraction["Developed_Med"]),
      Developed_High = inventory_end$Developed_High * as.numeric(plantable_fraction["Developed_High"])
    )

    total_plantable <- sum(developed_vals)

    # Proportional share of available area per class
    proportions <- developed_vals / total_plantable

    # Generate projections
    future_data <- data.frame(
      inventory_year = future_years,
      county_name = unique(historical_data$county_name),

      Urban_Tree = sapply(future_years, function(year) {
        if (urbanTree_factor == 0 || year < urbanTree_start_year) {
          inventory_end$Urban_Tree  # No change
        } else {
          growth <- logistic_growth(
            t = year,
            K = urbanTree_area,
            r = 4 / urbanTree_time,
            t0 = t0,
            start_year = urbanTree_start_year,
            end_year = urbanTree_start_year + urbanTree_time
          )
          min(inventory_end$Urban_Tree + growth, inventory_end$Urban_Tree + urbanTree_area)
        }
      }),

      Developed_Low = sapply(future_years, function(year) {
        if (urbanTree_factor == 0 || year < urbanTree_start_year) {
          inventory_end$Developed_Low
        } else {
          reduction <- logistic_growth(
            t = year,
            K = urbanTree_area * proportions["Developed_Low"],
            r = 4 / urbanTree_time,
            t0 = t0,
            start_year = urbanTree_start_year,
            end_year = urbanTree_start_year + urbanTree_time
          )
          max(inventory_end$Developed_Low - reduction, inventory_end$Developed_Low - developed_vals["Developed_Low"])
        }
      }),

      Developed_Med = sapply(future_years, function(year) {
        if (urbanTree_factor == 0 || year < urbanTree_start_year) {
          inventory_end$Developed_Med
        } else {
          reduction <- logistic_growth(
            t = year,
            K = urbanTree_area * proportions["Developed_Med"],
            r = 4 / urbanTree_time,
            t0 = t0,
            start_year = urbanTree_start_year,
            end_year = urbanTree_start_year + urbanTree_time
          )
          max(inventory_end$Developed_Med - reduction, inventory_end$Developed_Med - developed_vals["Developed_Med"])
        }
      }),

      Developed_High = sapply(future_years, function(year) {
        if (urbanTree_factor == 0 || year < urbanTree_start_year) {
          inventory_end$Developed_High
        } else {
          reduction <- logistic_growth(
            t = year,
            K = urbanTree_area * proportions["Developed_High"],
            r = 4 / urbanTree_time,
            t0 = t0,
            start_year = urbanTree_start_year,
            end_year = urbanTree_start_year + urbanTree_time
          )
          max(inventory_end$Developed_High - reduction, inventory_end$Developed_High - developed_vals["Developed_High"])
        }
      })
    )

    future_data
  })

  # Module 3 - Restoration ---------------------------------------------
  mod3_cropRestoration <- reactive({

    tree_share <- input$sliderForest/100
    grassland_share <- input$sliderGrassland/100
    wetland_share <- (100-input$sliderForest-input$sliderGrassland)/100

    # Inputs
    total_restore_factor <- input$cropland_restoration_pct / 100

    # Speed and delay parameters
    cropland_start_year <- input$cropland_dep

    cropland_delay <- cropland_start_year - 2023
    cropland_time <- input$cropland_comp

    # Filter historical data for the selected county
    historical_data <- filtered_data()
    null_data <- null_data()

    # Get the last row of the filtered data for projections
    inventory_end <- tail(historical_data, 1)

    # Total Cropland area in 2022
    total_cropland <- inventory_end$Cropland

    # Calculate areas for converting Urban_Grassland to Grassland
    total_restore_area <- total_restore_factor * total_cropland * 0.75 # adding a hard limit to how much area can be converted
    remaining_cropland <- total_restore_area - total_cropland

    # Calculate share of land that each cover type will gain
    restore_wetland_area <- total_restore_area * wetland_share
    restore_tree_area <- total_restore_area * tree_share
    restore_grassland_area <- total_restore_area * grassland_share



    # Sigmoidal growth parameters
    r <- 4 / cropland_time  # Growth rate is inversely proportional to planting time
    t0 <- cropland_start_year  # Start year determines when the curve begins


    future_data <- data.frame(
      inventory_year = future_years,
      county_name = unique(historical_data$county_name),
      ## FROM
      Cropland = sapply(future_years, function(year) {
        if (year < cropland_start_year) {
          inventory_end$Cropland  # No change before planting start year
        } else {
          # Calculate reductions for Cropland
          total_reduction <- if (total_restore_factor == 0) {
            0  # No reduction for Cropland if restoration area is 0
          } else {
            logistic_growth(
              t = year,
              K = total_restore_area,  # Total area to convert to Tree
              r = 4 / cropland_time,  # Growth rate (adjusted for planting time)
              t0 = cropland_start_year + cropland_time / 2,  # Center the curve
              start_year = cropland_start_year,
              end_year = cropland_start_year + cropland_time
            )
          }
          # Constrain reduction to ensure it does not exceed available cropland
          max(inventory_end$Cropland - total_reduction, inventory_end$Cropland - total_restore_area)
        }
      }),

      Tree = if (total_restore_factor == 0) {
        rep(inventory_end$Tree, length(future_years))  # Keep constant if no restoration
      } else {
        if (restore_tree_area == 0) {
          rep(inventory_end$Tree, length(future_years))  # Keep constant if no restoration
        } else {
          # Sigmoidal increase in area after delay
          sapply(future_years, function(year) {
            if (year < cropland_start_year) {
              inventory_end$Tree  # No change before start year
            } else {
              # Calculate the cumulative effect of the logistic growth curve
              growth = logistic_growth(
                t = year,
                K = restore_tree_area,  # Total area to convert
                r = 4 / cropland_time,  # Growth rate (adjusted for completion time)
                t0 = cropland_start_year + cropland_time / 2,  # Center the curve
                start_year = cropland_start_year,
                end_year = cropland_start_year + cropland_time
              )
              # Constrain growth to ensure it does not exceed available area
              min(inventory_end$Tree + growth, inventory_end$Tree + restore_tree_area)
            }
          })
        }
      },
      Grassland = if (total_restore_factor == 0) {
        rep(inventory_end$Grassland, length(future_years))  # Keep constant if no restoration
      } else {
        if (restore_grassland_area == 0) {
          rep(inventory_end$Grassland, length(future_years))  # Keep constant if no restoration
        } else {
          # Sigmoidal increase in area after delay
          sapply(future_years, function(year) {
            if (year < cropland_start_year) {
              inventory_end$Grassland  # No change before start year
            } else {
              # Calculate the cumulative effect of the logistic growth curve
              growth = logistic_growth(
                t = year,
                K = restore_grassland_area,  # Total area to convert
                r = 4 / cropland_time,  # Growth rate (adjusted for completion time)
                t0 = cropland_start_year + cropland_time / 2,  # Center the curve
                start_year = cropland_start_year,
                end_year = cropland_start_year + cropland_time
              )
              # Constrain growth to ensure it does not exceed available area
              min(inventory_end$Grassland + growth, inventory_end$Grassland + restore_grassland_area)
            }
          })
        }
      },
      Wetland = if (total_restore_factor == 0) {
        rep(inventory_end$Wetland, length(future_years))  # Keep constant if no restoration
      } else {
        if (restore_wetland_area == 0) {
          rep(inventory_end$Wetland, length(future_years))  # Keep constant if no restoration
        } else {
          # Sigmoidal increase in area after delay
          sapply(future_years, function(year) {
            if (year < cropland_start_year) {
              inventory_end$Wetland  # No change before start year
            } else {
              # Calculate the cumulative effect of the logistic growth curve
              growth = logistic_growth(
                t = year,
                K = restore_wetland_area,  # Total area to convert
                r = 4 / cropland_time,  # Growth rate (adjusted for completion time)
                t0 = cropland_start_year + cropland_time / 2,  # Center the curve
                start_year = cropland_start_year,
                end_year = cropland_start_year + cropland_time
              )
              # Constrain growth to ensure it does not exceed available area
              min(inventory_end$Wetland + growth, inventory_end$Wetland + restore_wetland_area)
            }
          })
        }
      }
    )





    future_data
  })


  # Reactive data for projections
  projected_data <- reactive({
    projected_mod1 <- mod1_lawnsToLegumes()
    projected_mod2 <- mod2_urbanTreePlanting()
    projected_mod3 <- mod3_cropRestoration()

    # Filter historical data for the selected county
    historical_data <- filtered_data()
    null_data <- null_data()


    # Get the last row of the filtered data for projections
    inventory_end <- tail(historical_data, 1)

    # Generate projections
    future_data <- data.frame(
      inventory_year = future_years,
      county_name = unique(historical_data$county_name),
      Grassland = (projected_mod1$Grassland - inventory_end$Grassland) + projected_mod3$Grassland,
      Urban_Grassland = projected_mod1$Urban_Grassland,
      Developed_Low = projected_mod2$Developed_Low,
      Developed_Med = projected_mod2$Developed_Med,
      Developed_High = projected_mod2$Developed_High,
      Urban_Tree = projected_mod2$Urban_Tree,
      Cropland = projected_mod3$Cropland,
      Tree = projected_mod3$Tree,
      Wetland = projected_mod3$Wetland,
      Bare = rep(inventory_end$Bare, length(future_years)),
      Water = rep(inventory_end$Water, length(future_years))
    )

    # Apply planting delay (Tree and Cropland are already handled above)
    future_data <- future_data %>%
      rowwise() %>%
      mutate(TOTAL = sum(c_across(c(Bare, Developed_Low, Developed_Med, Developed_High,
                                    Urban_Grassland, Urban_Tree,
                                    Cropland, Grassland, Tree, Water,
                                    Wetland)), na.rm = T)) %>%
      ungroup()

    # Combine historical and projected data
    combined_data <- bind_rows(historical_data, future_data)
    combined_data
  })




  theme_settings <- function() {
    theme(
      plot.title = element_text(size=17, face="bold"),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14)
    )
  }

  plot_colors <- c(
    "Tree" = "#4CAF50",
    "Grassland" = "#FFEB3B",
    "Wetland" = "#75D4D9",
    "Urban_Tree" = "#B6E39A",
    "Urban_Grassland" = "#D4CA6F",
    "Bare" = "#A9A9A9",
    "Developed_Low" = "#FFB9A8",
    "Developed_Med" = "#FF8E75",
    "Developed_High" = "#FF5733",
    "Cropland" = "orange",
    "Water" = "#1E90FF"
  )


  # Render Wedge Plot
  output$wedgePlot <- renderPlot({
    combined_data <- projected_data()

    # Transform data for ggplot (long format)
    plot_data <- combined_data %>%
      pivot_longer(cols = -c(inventory_year,county_name), names_to = "land_cover_type", values_to = "area") %>%
      mutate(land_cover_type = factor(land_cover_type,
                                      levels =
                                        c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
                                          "Cropland", "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Water")
      )) %>%
      filter(land_cover_type != "TOTAL")

    ggplot(plot_data) +
      geom_area(aes(x = inventory_year, y = area, fill = land_cover_type), alpha=0.5, show.legend = F) +
      geom_area(aes(x = inventory_year, y = area, color = land_cover_type), fill=NA, linewidth=1, show.legend = F) +
      geom_vline(xintercept = 2023, linetype = "dashed", alpha=0.5) +
      scale_fill_manual(
        values = plot_colors,
        breaks = c(
          "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
          "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Cropland", "Water"
        ),
        labels = c(
          "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland",
          "Bare", "Developed (Low)", "Developed (Med)", "Developed (High)", "Cropland", "Water"
        )
      ) +
      scale_color_manual(
        values = plot_colors,
        breaks = c(
          "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
          "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Cropland", "Water"
        ),
        labels = c(
          "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland",
          "Bare", "Developed (Low)", "Developed (Med)", "Developed (High)", "Cropland", "Water"
        )
      ) +
      labs(
        title = paste("Land Cover Change Projection for", input$selected_county, "County"),
        x = "Year",
        y = "Area (sq. km)",
        fill = "Land Cover Type"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(2000, 2050, by = 5)) +
      scale_y_continuous(
        labels = scales::label_number(scale = 1e-3, suffix = "k", accuracy = 0.1)
      ) +
      theme_settings()


    p1 <- ggplot(plot_data) +
      geom_area(aes(x = inventory_year, y = area, fill = land_cover_type), alpha=0.5, show.legend = F) +
      geom_area(aes(x = inventory_year, y = area, color = land_cover_type), fill=NA, linewidth=1, show.legend = F) +
      geom_vline(xintercept = 2023, linetype = "dashed", alpha=0.5) +
      scale_fill_manual(
        values = plot_colors,
        breaks = c(
          "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
          "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Cropland", "Water"
        ),
        labels = c(
          "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland",
          "Bare", "Developed (Low)", "Developed (Med)", "Developed (High)", "Cropland", "Water"
        )
      ) +
      scale_color_manual(
        values = plot_colors,
        breaks = c(
          "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
          "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Cropland", "Water"
        ),
        labels = c(
          "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland",
          "Bare", "Developed (Low)", "Developed (Med)", "Developed (High)", "Cropland", "Water"
        )
      ) +
      labs(
        title = NULL,
        x = "Year",
        y = "Area (sq. km)",
        fill = "Land Cover Type"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(2000, 2050, by = 5)) +
      scale_y_continuous(
        labels = scales::label_number(scale = 1e-3, suffix = "k", accuracy = 0.1)
      ) +
      theme_settings()



    p2 <- rbind(
      plot_data %>%
        filter(inventory_year == future_years[1]-1) %>%
        mutate(tag = factor("initial", levels=c("initial","final"))),
      plot_data %>%
        filter(inventory_year == 2050) %>%
        mutate(tag = factor("final", levels=c("initial","final")))
    ) %>% arrange(tag) %>%
      # filter(!is.na(sequestration_potential)) %>%
      mutate(inventory_year = factor(inventory_year, levels=c(as.character(future_years[1]-1), as.character(2050)))) %>%
      ggplot() +
      theme_minimal() +
      geom_col(aes(x = inventory_year, y = area, fill = land_cover_type), alpha=0.5) +
      geom_col(aes(x = inventory_year, y = area, color = land_cover_type), fill=NA, linewidth=1, show.legend = F) +
      scale_fill_manual(
        values = plot_colors,
        breaks = c(
          "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
          "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Cropland", "Water"
        ),
        labels = c(
          "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland",
          "Bare", "Developed (Low)", "Developed (Med)", "Developed (High)", "Cropland", "Water"
        )
      ) +
      scale_color_manual(
        values = plot_colors,
        breaks = c(
          "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
          "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Cropland", "Water"
        ),
        labels = c(
          "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland",
          "Bare", "Developed (Low)", "Developed (Med)", "Developed (High)", "Cropland", "Water"
        )
      ) +
      labs(
        # title = paste("Change in sequestration potential for", input$selected_county, "County"),
        x = NULL,
        y = NULL,
        fill = "Land Cover Type"
      ) +
      theme_settings() +
      theme(
        axis.text.y = element_blank()
      )




    wrap_elements(p1 + p2 + plot_layout(widths=c(9,1))) +
      labs(title = paste("Land Cover Change Projection for", input$selected_county, "County")) +
      theme_minimal() +
      theme_settings()


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
                                          "Cropland", "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Water")
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


    baseline_sequestration <- plot_data %>%
      filter(inventory_year == 2022) %>%
      ungroup() %>%
      pull(sequestration_potential) %>% sum()


    final_sequestration <- plot_data %>%
      filter(inventory_year == 2050) %>%
      ungroup() %>%
      pull(sequestration_potential) %>% sum()


    p1 <- ggplot(plot_data) +
      geom_area(aes(x = inventory_year, y = sequestration_potential, fill = land_cover_type), alpha=0.5, show.legend = F) +
      geom_area(aes(x = inventory_year, y = sequestration_potential, color = land_cover_type), fill=NA, linewidth=1, show.legend = F) +

      geom_vline(xintercept = 2023, linetype = "dashed", alpha=0.5) +
      geom_hline(yintercept = baseline_sequestration, linetype = "dashed", alpha=0.5) +
      geom_hline(yintercept = final_sequestration, linetype = "dashed", alpha=0.5) +

      scale_fill_manual(
        values = plot_colors,
        breaks =
          c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland"),
        labels =
          c("Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland")
      ) +
      scale_color_manual(
        values = plot_colors,
        breaks =
          c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland"),
        labels =
          c("Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland")
      ) +
      labs(
        title = NULL,
        x = "Year",
        y = expression("Metric tons"~CO[2]*e),
        fill = "Land Cover Type"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(2000, 2050, by = 5)) +
      scale_y_continuous(
        labels = scales::label_number(scale = 1e-3, suffix = "k", accuracy = 1)
      ) +
      theme_settings()



    p2 <- rbind(
      plot_data %>%
        filter(inventory_year == future_years[1]-1) %>%
        mutate(tag = factor("initial", levels=c("initial","final"))),
      plot_data %>%
        filter(inventory_year == 2050) %>%
        mutate(tag = factor("final", levels=c("initial","final")))
    ) %>% arrange(tag) %>%
      filter(!is.na(sequestration_potential)) %>%
      mutate(inventory_year = factor(inventory_year, levels=c(as.character(future_years[1]-1), as.character(2050)))) %>%
      ggplot() +
      theme_minimal() +
      geom_col(aes(x = inventory_year, y = sequestration_potential, fill = land_cover_type), alpha=0.5) +
      geom_col(aes(x = inventory_year, y = sequestration_potential, color = land_cover_type), fill=NA, linewidth=1, show.legend = F) +
      scale_fill_manual(
        values = plot_colors,
        breaks =
          c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland"),
        labels =
          c("Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland")
      ) +
      scale_color_manual(
        values = plot_colors,
        breaks =
          c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland"),
        labels =
          c("Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland")
      ) +
      labs(
        # title = paste("Change in sequestration potential for", input$selected_county, "County"),
        x = NULL,
        y = NULL,
        fill = "Land Cover Type"
      ) +
      theme_settings() +
      theme(
        axis.text.y = element_blank()
      )




    wrap_elements(p1 + p2 + plot_layout(widths=c(9,1))) +
      labs(title = paste("Sequestration potential for", input$selected_county, "County")) +
      theme_minimal() +
      theme_settings()

  })

  output$summaryPlot1 <- renderPlot({
    combined_data <- projected_data()

    # Transform data for ggplot (long format)
    plot_data <- combined_data %>%
      pivot_longer(cols = -c(inventory_year,county_name), names_to = "land_cover_type", values_to = "area") %>%
      mutate(land_cover_type = factor(land_cover_type,
                                      levels =
                                        c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
                                          "Cropland", "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Water")
      )) %>%
      filter(land_cover_type != "TOTAL")


    # Compute C sequestration and stock potential for natural systems sectors
    plot_data <-
      plot_data %>%
      left_join(., land_cover_c, by = join_by(land_cover_type)) %>%
      # filter(!is.na(seq_mtco2e_sqkm)) %>%
      mutate(
        sequestration_potential = area * seq_mtco2e_sqkm,
        stock_potential = area * stock_mtco2e_sqkm
      ) %>%
      dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm))


    plot_data <- rbind(
      plot_data %>%
        filter(inventory_year == future_years[1]-1) %>%
        mutate(tag = factor("initial", levels=c("initial","final"))),
      plot_data %>%
        filter(inventory_year == 2050) %>%
        mutate(tag = factor("final", levels=c("initial","final")))
    ) %>% arrange(tag)



    plot_data %>%
      filter(!is.na(sequestration_potential)) %>%
      mutate(inventory_year = factor(inventory_year, levels=c(as.character(future_years[1]-1), as.character(2050)))) %>%
      ggplot() +
      theme_minimal() +
      geom_col(aes(x = inventory_year, y = sequestration_potential, fill = land_cover_type), alpha=0.5) +
      geom_col(aes(x = inventory_year, y = sequestration_potential, color = land_cover_type), fill=NA, linewidth=1, show.legend = F) +
      scale_fill_manual(
        values = plot_colors,
        breaks =
          c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland"),
        labels =
          c("Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland")
      ) +
      scale_color_manual(
        values = plot_colors,
        breaks =
          c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland"),
        labels =
          c("Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland")
      ) +
      scale_y_continuous(
        labels = scales::label_number(scale = 1e-3, suffix = "k", accuracy = 1)
      ) +
      labs(
        title = "",
        x = "Year",
        y = NULL,
        fill = "Land Cover Type"
      ) +
      theme_settings()


    # browser()

  })


  output$summaryText <- renderUI({
    # Transform data for summary
    summary_data <- projected_data() %>%
      pivot_longer(cols = -c(inventory_year,county_name), names_to = "land_cover_type", values_to = "area") %>%
      mutate(land_cover_type = factor(land_cover_type,
                                      levels =
                                        c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
                                          "Cropland", "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Water")
      )) %>%
      filter(land_cover_type != "TOTAL")


    # Compute C sequestration and stock potential for natural systems sectors
    summary_data <-
      summary_data %>%
      left_join(., land_cover_c, by = join_by(land_cover_type)) %>%
      filter(!is.na(seq_mtco2e_sqkm)) %>%
      mutate(
        sequestration_potential = area * seq_mtco2e_sqkm,
        stock_potential = area * stock_mtco2e_sqkm
      ) %>%
      dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm))


    baseline_sequestration <- summary_data %>%
      filter(inventory_year == 2022) %>%
      ungroup() %>%
      pull(sequestration_potential) %>% sum()


    final_sequestration <- summary_data %>%
      filter(inventory_year == 2050) %>%
      ungroup() %>%
      pull(sequestration_potential) %>% sum()


    seq_change_pct <- round(((final_sequestration-baseline_sequestration)/baseline_sequestration)*100,1)
    seq_change_actual <- round(abs(final_sequestration-baseline_sequestration)/1000,0)

    county_name <- unique(summary_data$county_name)

    if (seq_change_pct > 0) {
      HTML(paste0("+",
                  seq_change_pct,
                  "% increase (+",
                  seq_change_actual,
                  "k metric tons CO<sub>2</sub>e) in C sequestered by natural systems in ",
                  county_name,
                  " County."))
    } else if (seq_change_pct < 0) {
      HTML(paste0(seq_change_pct,
                  "% decrease (",
                  seq_change_actual,
                  "k metric tons CO<sub>2</sub>e) in C sequestered by natural systems in ",
                  county_name,
                  " County."))
    } else {
      HTML("No change in C sequestered by natural systems.")
    }


    # if (seq_change_pct > 0) {
    #   paste0("By 2050, natural systems in ",
    #          county_name,
    #          " County will have sequestered +",
    #          seq_change_actual,
    #          "k metric tons CO2 (",
    #          seq_change_pct,
    #          "% increase) over 2022 estimates.")
    # } else if (seq_change_pct < 0) {
    #   paste0("By 2050, natural systems in ",
    #          county_name,
    #          " County will have lost ",
    #          seq_change_actual,
    #          "k metric tons CO2 (",
    #          seq_change_pct,
    #          "% decrease) compared with 2022 estimates.")
    # } else {
    #   paste0("No change in C sequestered by natural systems.")
    # }
    #






  })


}

## Run the app -----------
shinyApp(ui = ui, server = server)



