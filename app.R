#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(tidyr)
library(scales)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(

  theme = shinytheme("cerulean"),
  titlePanel("City of Minneapolis GHG Reduction Scenario"),
  sidebarLayout(
    sidebarPanel(
      div(style = "overflow-y: auto; max-height: 500px;", # Add custom CSS style here
      # Add "Play" button

      actionButton("play", "Run Scenario"),
      tags$p(
        "Click Run Scenario to run the scenario and view the results to the left. It takes about 30 seconds to run the scenario."
      ),
      # Add input widgets for user to provide arguments for 'myfunction'

      textInput("input_selected_ctu", "Selected City:", value = "Minneapolis"),
      tags$p("Leave as is. The selected City"),

      numericInput(
        "input_grid_decarbonization_pct",
        "Grid Decarbonization by 2040 (%):",
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.05
      ),

      tags$h3("Residential Buildings"),

      numericInput(
        "input_home_behavior_change_pct",
        "Home Behavior Change by 2040 (%):",
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.05
      ),
      tags$p("Percentage of Households that will change their behavior to reduce energy use."),

      numericInput(
        "input_new_homes_to_multifamily_pct",
        "New Single family Homes to Multifamily by 2040 (%):",
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.05
      ),
      tags$p("Percentage of all new single-family households that will be converted to multifamily."),

      numericInput(
        "input_new_homes_leed_gold_pct",
        "New Homes LEED Gold by 2040 (%):",
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.05
      ),
      tags$p(
        "Percentage of all new single-family households that will be energy efficient (LEED Gold for Reference)."
      ),

      numericInput(
        "input_new_homes_affected_pct",
        "New Homes Affected by Increased Energy Prices 2040 (%):",
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.05
      ),
      tags$p(
        "Percentage of all new single-family households that will respond to increased energy costs by decreasing home size."
      ),

      numericInput(
        "input_existing_home_retrofit_pct",
        "Existing Home Retrofit by 2040 (%):",
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.05
      ),
      tags$p(
        "Percentage of all existing single-family households that will retrofit their homes to be energy efficient."
      ),

      numericInput(
        "input_existing_home_ultra_retrofit_pct",
        "Existing Home Ultra Retrofit by 2040 (%):",
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.05
      ),
      tags$p(
        "Percentage of all existing single-family households that will retrofit their homes to be super energy efficient (Passivehouse for Reference)."
      ),

      numericInput(
        "input_additional_electrified_residential_buildings_pct",
        "Additional Electrified Residential Buildings by 2040 (%):",
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.05
      ),
      tags$p(
        "Percentage of all new single-family households that will respond to increased energy costs by decreasing home size."
      ),

      checkboxInput("input_renewable_ng_res",
                    "Renewable Natural Gas (Residential):"),
      tags$p("Shift from natural gas to renewable natural gas for residential heating."),

      tags$h3("Commercial Buildings"),

      numericInput(
        "input_existing_high_efficiency_buildings_pct",
        "Existing High Efficiency Buildings by 2040 (%):",
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.05
      ),
      tags$p(
        "Percentage of all existing commercial buildings that will retrofit their buildings to be energy efficient."
      ),

      numericInput(
        "input_electrified_buildings_pct",
        "Electrified Commercial Buildings by 2040 (%):",
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.05
      ),

      checkboxInput("input_renewable_ng_nonres",
                    "Renewable Natural Gas (Commercial):"),
      tags$p("Shift from natural gas to renewable natural gas for commercial heating."),


      tags$h3("Passenger Vehicles"),

      numericInput(
        "input_vmt_fee",
        "Vehicle Miles Traveled Fee ($/mile) by 2040:",
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.05
      ),
      tags$p("Fee on vehicle miles traveled (VMT) to reduce vehicle miles traveled (VMT)."),

      #numericInput("input_payd_fee", "Pay As You Drive Fee ($/mile) by 2040:",
      #      value = 0, min = 0, max = 1, step = 0.05),
      #tags$p("Fee on vehicle miles traveled (VMT) to reduce vehicle miles traveled (VMT). "),

      numericInput("input_cong_price", "Congestion Price ($/mile) by 2040:",
            value = 0.05, min = 0, max = 1, step = 0.00),
      tags$p("Fee on vehicle miles traveled (VMT) to reduce vehicle miles traveled (VMT)."),

      numericInput("input_gas_tax", "Gas Tax ($/mile) by 2040:",
            value = 0.05, min = 0, max = 1, step = 0.05),
      tags$p("Tax on Gasoline to reduce vehicle miles traveled (VMT)."),

      numericInput("input_parking_price", "Parking Price ($/hour) by 2040:",
            value = 1.00, min = 0, max = 10, step = 0.05),
      tags$p("Price on parking to reduce vehicle miles traveled (VMT)."),

      numericInput("input_pldv_avo_pct", "Passenger Light Duty Vehicle Average Vehicle Occupancy Increase (%) by 2040:",
            value = 0.05, min = 0, max = 1, step = 0.05),
      tags$p("Percentage increase in average vehicle occupancy for passenger light duty vehicles."),

      numericInput("input_telework_pct", "Telework Increase (%) by 2040:",
            value = 0.05, min = 0, max = 1, step = 0.05),
      tags$p("Percentage increase in telework."),

      numericInput(
        "input_bev_pct_sales",
        "Battery EV Sales Increase (%) by 2040:",
        value = 0.60,
        min = 0,
        max = 1,
        step = 0.05
      ),
      tags$p("Percentage increase in battery electric vehicle sales."),

      numericInput(
        "input_phev_pct_sales",
        "Plug-in Hybrid EV Sales Increase (%) by 2040:",
        value = 0.20,
        min = 0,
        max = 1,
        step = 0.05
      ),
      tags$p("Percentage increase in plug-in hybrid electric vehicle sales."),

      numericInput(
        "input_hev_pct_sales",
        "Hybrid EV Sales (%) by 2040:",
        value = 0.20,
        min = 0,
        max = 1,
        step = 0.05
      ),
      tags$p("Percentage increase in hybrid electric vehicle sales."),

      tags$h3("Freight Vehicles"),
      numericInput("input_freight_parking_price", "Freight Parking Price ($/mile) by 2040:",
            value = 0.05, min = 0, max = 1, step = 0.05),

      numericInput("input_freight_vmt_fee", "Freight VMT Fee ($/mile) by 2040:",
            value = 0.05, min = 0, max = 1, step = 0.05),

      tags$h3("Land Use and Transit"),
      numericInput("input_transit_avo_pct", "Transit Average Vehicle Occupancy Increase (%) by 2040:",
            value = 0.05, min = 0, max = 1, step = 0.05),

      #numericInput("input_transit_service_pct", "Transit Service (Routes and Frequency) Increase (%) by 2040:",
      #      value = 0.05, min = 0, max = 1, step = 0.05),

      numericInput(
        "input_pop_dens_pct_change",
        "Population Density Increase (%) by 2040:",
        value = 0.02,
        min = 0,
        max = 0.1,
        step = 0.02
      ),
      tags$p("Percentage increase in population density."),

      numericInput(
        "input_emp_dens_pct_change",
        "Employment Density Increase (%) by 2040:",
        value = 0.02,
        min = 0,
        max = 0.1,
        step = 0.05
      ),
      tags$p("Percentage increase in employment density."),

      numericInput(
        "input_land_use_diversity_pct_change",
        "Land Use Diversity Increase (%) by 2040:",
        value = 0.02,
        min = 0,
        max = 0.1,
        step = 0.05
      ),
      tags$p("Percentage increase in land use diversity."),

      numericInput(
        "input_intersection_design_pct_change",
        "Intersection Design Increase (%) by 2040:",
        value = 0.02,
        min = 0,
        max = 0.1,
        step = 0.05
      ),
      tags$p("Percentage increase in intersection design."),

      numericInput(
        "input_comb_5d_impact_pct_change",
        "Combined 5D Impact Increase (%) by 2040:",
        value = 0.02,
        min = 0,
        max = 0.1,
        step = 0.02
      ),
      tags$p("Percentage increase in combined 5D impact."),

      numericInput(
        "input_job_access_pct_change",
        "Job Accessibility Increase (%) by 2040:",
        value = 0.02,
        min = 0,
        max = 0.1,
        step = 0.02
      ),
      tags$p("Percentage increase in job accessibility."),

      numericInput(
        "input_transit_dist_pct_change",
        "Distance to Transit Decrease (%) by 2040:",
        value = 0.02,
        min = 0,
        max = 0.1,
        step = 0.02
      ),
      tags$p("Percentage decrease in distance to transit."),

      downloadButton("downloadCSV", "Download CSV")
    )),

    mainPanel(
      plotOutput("barChart"),
      DTOutput("table")
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


  myfunction <- function(.selected_ctu,
                          .grid_decarbonization_pct,
                          .electrified_buildings_pct,
                          .home_behavior_change_pct,
                          .existing_high_efficiency_buildings_pct,
                          .renewable_ng_res,
                          .renewable_ng_nonres,
                          .new_homes_to_multifamily_pct,
                          .new_homes_leed_gold_pct,
                          .new_homes_affected_pct,
                          .existing_home_retrofit_pct,
                          .existing_home_ultra_retrofit_pct,
                          .additional_electrified_residential_buildings_pct,
                          .transit_avo_pct,
                          .pldv_avo_pct,
                          .transit_service_pct,
                          .vmt_fee,
                          .payd_fee,
                          .gas_tax,
                          .parking_price,
                          .freight_parking_price,
                          .cong_price,
                          .freight_vmt_fee,
                          .pop_dens_pct_change,
                          .emp_dens_pct_change,
                          .land_use_diversity_pct_change,
                          .intersection_design_pct_change,
                          .job_access_pct_change,
                          .transit_dist_pct_change,
                          .comb_5d_impact_pct_change,
                          .telework_pct,
                          .bev_pct_sales,
                          .phev_pct_sales,
                          .hev_pct_sales
                         ){

    bau <- ghg.sp::run_all_modules(
      .selected_ctu = .selected_ctu,

      ## land use module parameters
      .urban_form_scenario = "bau",
      .conservation_tillage_intervention = "current_conservation_tillage",
      .tree_planting_intervention = "match_la_million_trees_goal",
      .tree_planting_per_capita = 0,
      .tree_planting_per_hectare = 0,
      .parking_lot_reduction_percentage = 0,

      ## building energy module parameters
      .grid_decarbonization_pct = 0,

      ## non residential energy parameters
      .renewable_ng_nonres = FALSE,
      .electrified_buildings_pct = 0,
      .commercial_smart_grid_pct = 1,
      .industrial_smart_grid_pct = 1,
      .smart_grid_energy_reduction_pct = 0,

      ## residential energy parameters
      .renewable_ng_res = FALSE,
      .new_homes_to_multifamily_pct = 0,
      .existing_high_efficiency_buildings_pct = 0,
      .home_behavior_change_pct = 0,
      .single_family_floor_area_growth_pct = 0.05,
      .new_homes_affected_pct = 0,
      .new_homes_leed_gold_pct = 0,
      .existing_home_retrofit_pct = 0,
      .existing_home_ultra_retrofit_pct = 0,
      .additional_electrified_residential_buildings_pct = 0,

      ## transportation parameters
      .scenario = "bau",
      .electric_scenario = "ER",
      .aeo_scenario = "REF",
      .transit_avo_pct = 0,
      .pldv_avo_pct = 0,
      .transit_service_pct = 0,
      .vmt_fee = 0,
      .payd_fee = 0,
      .gas_tax = 0,
      .parking_price = 0,
      .freight_parking_price = 0,
      .cong_price = 0,
      .freight_vmt_fee = 0,
      .drs_pct = 0,
      .av_pct = 0,
      .pop_dens_pct_change = 0,
      .emp_dens_pct_change = 0,
      .land_use_diversity_pct_change = 0,
      .intersection_design_pct_change = 0,
      .job_access_pct_change = 0,
      .transit_dist_pct_change = 0,
      .comb_5d_impact_pct_change = 0,
      .telework_pct = 0,
      .bev_pct_sales = 0,
      .phev_pct_sales = 0,
      .hev_pct_sales = 0,
      .mit_bau_summary = 0
    )

    scen <- ghg.sp::run_all_modules(
      .selected_ctu = .selected_ctu,

      ## land use module parameters
      .urban_form_scenario = "bau",
      .conservation_tillage_intervention = "current_conservation_tillage",
      .tree_planting_intervention = "match_la_million_trees_goal",
      .tree_planting_per_capita = 0.26,
      .tree_planting_per_hectare = 247,
      .parking_lot_reduction_percentage = 0.8,

      ## building energy module parameters
      .grid_decarbonization_pct = .grid_decarbonization_pct,
      .renewable_ng_nonres = .renewable_ng_nonres,

      ## non residential energy parameters
      .electrified_buildings_pct = .electrified_buildings_pct,
      .existing_high_efficiency_buildings_pct = .existing_high_efficiency_buildings_pct,
      .commercial_smart_grid_pct = 1,
      .industrial_smart_grid_pct = 1,
      .smart_grid_energy_reduction_pct = 0.11,

      ## residential energy parameters
      ### renewable natural gas
      .renewable_ng_res = .renewable_ng_res,
      ### more multifamily housing
      .new_homes_to_multifamily_pct = .new_homes_to_multifamily_pct,
      .home_behavior_change_pct = .home_behavior_change_pct,
      .single_family_floor_area_growth_pct = 0.05,
      .new_homes_affected_pct = .new_homes_affected_pct,
      .new_homes_leed_gold_pct = .new_homes_leed_gold_pct,
      .existing_home_retrofit_pct = .existing_home_retrofit_pct,
      .existing_home_ultra_retrofit_pct = .existing_home_ultra_retrofit_pct,
      .additional_electrified_residential_buildings_pct = .additional_electrified_residential_buildings_pct,

      ## transportation parameters
      .scenario = "scen",
      .electric_scenario = "ER",
      .aeo_scenario = "REF",
      .transit_avo_pct = .transit_avo_pct ,
      .pldv_avo_pct = .pldv_avo_pct,
      .transit_service_pct = 0,
      .vmt_fee = .vmt_fee,
      .payd_fee = 0,
      .gas_tax =.gas_tax,
      .parking_price = .parking_price,
      .freight_parking_price = .freight_parking_price,
      .cong_price = .cong_price,
      .freight_vmt_fee = .freight_vmt_fee,
      .pop_dens_pct_change = .pop_dens_pct_change,
      .emp_dens_pct_change = .emp_dens_pct_change,
      .land_use_diversity_pct_change = .land_use_diversity_pct_change,
      .intersection_design_pct_change = .intersection_design_pct_change,
      .job_access_pct_change = .job_access_pct_change,
      .transit_dist_pct_change = .transit_dist_pct_change,
      .comb_5d_impact_pct_change = .comb_5d_impact_pct_change,
      .telework_pct = .telework_pct,
      .bev_pct_sales = .bev_pct_sales,
      .phev_pct_sales = .phev_pct_sales,
      .hev_pct_sales = .hev_pct_sales,
      .mit_bau_summary = 0
    )

    buildings_bau <- bau$buildings %>%
      # Select the variables you want to keep
      select(year, scen, var, value) %>%
      filter(scen == "bau") %>%
      # Filter out the variables you don't want
      filter(var %in% c("residential_electricity_emissions_kg_co",
                        "commercial_electricity_emissions_kg_co",
                        "industrial_electricity_emissions_kg_co",
                        "residential_natural_gas_emissions_kg_co",
                        "commercial_natural_gas_emissions_kg_co",
                        "industrial_natural_gas_emissions_kg_co")) %>%
      # Rename the variables
      mutate(var = if_else(var == "residential_electricity_emissions_kg_co",
                           "Residential Buildings Electricity", var)) %>%
      mutate(var = if_else(var == "commercial_electricity_emissions_kg_co",
                           "Commercial Buildings Electricity", var)) %>%
      mutate(var = if_else(var == "industrial_electricity_emissions_kg_co",
                           "Industrial Buildings Electricity", var)) %>%
      mutate(var = if_else(var == "residential_natural_gas_emissions_kg_co",
                           "Residential Buildings Natural Gas", var)) %>%
      mutate(var = if_else(var == "commercial_natural_gas_emissions_kg_co",
                           "Commercial Buildings Natural Gas", var)) %>%
      mutate(var = if_else(var == "industrial_natural_gas_emissions_kg_co",
                           "Industrial Buildings Natural Gas", var)) %>%
      # Convert the units
      mutate(value = value/1000) %>%
      rename(tonnes_co2_per_year = value) %>%
      arrange(year)

    buildings_scen <- scen$buildings %>%
      # Select the variables you want to keep
      select(year, scen, var, value) %>%
      filter(scen == "scen",
             year == 2040) %>%
      # Filter out the variables you don't want
      filter(var %in% c("residential_electricity_emissions_kg_co",
                        "commercial_electricity_emissions_kg_co",
                        "industrial_electricity_emissions_kg_co",
                        "residential_natural_gas_emissions_kg_co",
                        "commercial_natural_gas_emissions_kg_co",
                        "industrial_natural_gas_emissions_kg_co")) %>%
      # Rename the variables
      mutate(var = if_else(var == "residential_electricity_emissions_kg_co",
                           "Residential Buildings Electricity", var)) %>%
      mutate(var = if_else(var == "commercial_electricity_emissions_kg_co",
                           "Commercial Buildings Electricity", var)) %>%
      mutate(var = if_else(var == "industrial_electricity_emissions_kg_co",
                           "Industrial Buildings Electricity", var)) %>%
      mutate(var = if_else(var == "residential_natural_gas_emissions_kg_co",
                           "Residential Buildings Natural Gas", var)) %>%
      mutate(var = if_else(var == "commercial_natural_gas_emissions_kg_co",
                           "Commercial Buildings Natural Gas", var)) %>%
      mutate(var = if_else(var == "industrial_natural_gas_emissions_kg_co",
                           "Industrial Buildings Natural Gas", var)) %>%
      # Convert the units
      mutate(value = value/1000) %>%
      rename(tonnes_co2_per_year = value) %>%
      arrange(year)

    buildings <- rbind(buildings_bau, buildings_scen)  %>%
      mutate(module = "Buildings")

    landuse <- scen$land_use %>%
      mutate(scen = "scen",
             year = as.character(year)) %>%
      select(year, scen, var, value) %>%
      rename(tonnes_co2_per_year = value) %>%
      mutate(var = if_else(var == "sequestration_tonnes_co2e_per_year",
                           "Sequestration", var),
             var = if_else(var == "stock_tonnes_co2e_per_year (land conversion emissions)",
                           "Stock", var)) %>%
      mutate(module = "Land Use")

    transportation_bau <- bau$transp$app_data %>%
      ungroup() %>%
      filter(name == "direct") %>%
      select(year, scenario, submodule, mode, metric, name, value) %>%
      group_by(year, scenario, submodule) %>%
      rename(scen = scenario) %>%
      summarise(value = sum(value)) %>%
      mutate(value = value) %>%
      rename(tonnes_co2_per_year = value,
             var = submodule) %>%
      mutate(var = if_else(var == "people", "Passenger Transportation", var),
             var = if_else(var == "freight", "Freight Transportation", var))

    transportation_scen <- scen$transp$app_data %>%
      ungroup() %>%
      filter(name == "direct",
             year == 2040) %>%
      select(year, scenario, submodule, mode, metric, name, value) %>%
      group_by(year, scenario, submodule) %>%
      rename(scen = scenario) %>%
      summarise(value = sum(value)) %>%
      mutate(value = value) %>%
      rename(tonnes_co2_per_year = value,
             var = submodule) %>%
      mutate(var = if_else(var == "people", "Passenger Transportation", var),
             var = if_else(var == "freight", "Freight Transportation", var))

    transportation <- rbind(transportation_bau, transportation_scen) %>%
      mutate(module = "Transportation")

    summary <- rbind(buildings, landuse, transportation) %>%
      arrange(scen, year)

    return(summary)

  }

  # Create a reactive expression to call 'myfunction' with user inputs
  dataset <- eventReactive(input$play, {
    myfunction(.selected_ctu = input$input_selected_ctu,
               .grid_decarbonization_pct = input$input_grid_decarbonization_pct,
               .home_behavior_change_pct = input$input_home_behavior_change_pct,
               .electrified_buildings_pct = input$input_electrified_buildings_pct,
               .existing_high_efficiency_buildings_pct = input$input_existing_high_efficiency_buildings_pct,
               .renewable_ng_res = input$input_renewable_ng_res,
               .new_homes_to_multifamily_pct = input$input_new_homes_to_multifamily_pct,
               .new_homes_leed_gold_pct = input$input_new_homes_leed_gold_pct,
               .new_homes_affected_pct = input$input_new_homes_affected_pct,
               .existing_home_retrofit_pct = input$input_existing_home_retrofit_pct,
               .existing_home_ultra_retrofit_pct = input$input_existing_home_ultra_retrofit_pct,
               .additional_electrified_residential_buildings_pct = input$input_additional_electrified_residential_buildings_pct,
               .renewable_ng_nonres = input$input_renewable_ng_nonres,
               .transit_avo_pct = input$input_transit_avo_pct,
               .pldv_avo_pct = input$input_pldv_avo_pct,
               #.transit_service_pct = input$input_transit_service_pct,
               .vmt_fee = input$input_vmt_fee,
               .payd_fee = input$input_payd_fee,
               .gas_tax = input$input_gas_tax,
               .parking_price = input$input_parking_price,
               .telework_pct = input$input_telework_pct,
               .cong_price = input$input_cong_price,
               .freight_parking_price = input$input_freight_parking_price,
               .freight_vmt_fee = input$input_freight_vmt_fee,
               .pop_dens_pct_change = input$input_pop_dens_pct_change,
               .emp_dens_pct_change = input$input_emp_dens_pct_change,
               .land_use_diversity_pct_change = input$input_land_use_diversity_pct_change,
               .intersection_design_pct_change = input$input_intersection_design_pct_change,
               .job_access_pct_change = input$input_job_access_pct_change,
               .transit_dist_pct_change = input$input_transit_dist_pct_change,
               .comb_5d_impact_pct_change = input$input_comb_5d_impact_pct_change,
               .bev_pct_sales = input$input_bev_pct_sales,
               .phev_pct_sales = input$input_phev_pct_sales,
               .hev_pct_sales = input$input_hev_pct_sales
               )
  })

  # Create the stacked bar chart
  output$barChart <- renderPlot({
    # Check if the 'Play' button has been clicked
    if (input$play == 0) {
      return(NULL)
    }

  data <- dataset()

    ggplot(data, aes(x = year, y = tonnes_co2_per_year, fill = var)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~scen) +
      labs(
        title = "Stacked Bar Chart",
        x = "Year",
        y = "Tonnes CO2 per Year"
      ) +
      theme_minimal() +
    scale_y_continuous(labels = comma)

  })

  # Render the data table
  output$table <- renderDT({
    datatable(dataset())
  })

  data_csv <- reactive({
    as.data.frame(dataset())
    })
  # Handle the download action
  output$downloadCSV <- downloadHandler(

    filename = function() {
      "table.csv"
    },
    content = function(file) {
      write.csv(data_csv(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
}
# Run the application
shinyApp(ui = ui, server = server)
