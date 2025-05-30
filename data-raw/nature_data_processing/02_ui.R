ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  tags$head(
    tags$style(HTML("
      h3 { font-size: 16px; }
      h4 { font-size: 14px; }
      h5 { font-size: 12px; }
      label, .form-control { font-size: 12px; }
    "))
  ),

  titlePanel(paste0("Land Cover Change (",
                    head(lc_county$inventory_year, 1), "-2050)")),
  br(),
  fluidRow(
    # Sidebar panel (left)
    column(
      width = 4,
      tags$h3("Select Area of Interest"),
      tabsetPanel(
        id = "selected_tab", # <---- ID to track the selected tab
        tabPanel("County",
                 # tags$h3("County Selection"),
                 selectInput("selected_county",
                             label="Choose a county:",
                             # label=NULL,
                             choices = unique(lc_county$county_name),
                             selected = "Anoka")
        ),
        tabPanel("CTU",
                 # tags$h3("CTU Selection"),
                 selectInput("selected_ctu",
                             label="Choose a CTU:",
                             # label=NULL,
                             choices = unique(lc_ctu$ctu_name),
                             selected = "Afton")
        )
      ),


      br(),
      tags$h3("Mitigation Strategies"),
      accordion(
        id = "land_cover_controls",
        open = FALSE,
        multiple = FALSE,
        accordion_panel("Urban Tree Planting",
                        fluidRow(
                          column(10, sliderInput("urbanTreePlanting_start_yr", "When to Deploy?", min = 2025, max = 2045, value = 2025, step = 1, sep = "", ticks=FALSE)),
                          column(2, actionButton("reset_urbanTreePlanting_start_yr", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("urbanTreePlanting_comp_time", "Years to Complete?", min = 5, max = 30, value = 15, step = 1, sep = "", ticks=FALSE)),
                          column(2, actionButton("reset_urbanTreePlanting_comp_time", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("urbanTreePlanting_area_pct", "Available Urban Tree Area (%)", min = 0, max = 100, value = 0, ticks=FALSE)),
                          column(2, actionButton("reset_urbanTreePlanting_area_pct", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10,
                                 actionButton("reset_urbanTreePlanting", "Reset")
                          )
                        )

        ),
        accordion_panel("Lawns to Legumes",
                        fluidRow(
                          column(10, sliderInput("lawnsToLegumes_start_yr", "When to Deploy?", min = 2025, max = 2045, value = 2025, step = 1, sep = "", ticks=FALSE)),
                          column(2, actionButton("reset_lawnsToLegumes_start_yr", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("lawnsToLegumes_comp_time", "Years to Complete?", min = 5, max = 30, value = 15, step = 1, sep = "", ticks=FALSE)),
                          column(2, actionButton("reset_lawnsToLegumes_comp_time", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("lawnsToLegumes_area_pct", "Available Lawn Area (%):", min = 0, max = 100, value = 0, ticks=FALSE)),
                          column(2, actionButton("reset_lawnsToLegumes_area_pct", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10,
                                 actionButton("reset_lawnsToLegumes", "Reset")
                          )
                        )

        ),
        accordion_panel("Cropland Restoration",
                        fluidRow(
                          column(10, sliderInput("cropRestoration_start_yr", "When to Deploy?", min = 2025, max = 2045, value = 2025, step = 1, sep = "", ticks=FALSE)),
                          column(2, actionButton("reset_cropRestoration_start_yr", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("cropRestoration_comp_time", "Years to Complete?", min = 5, max = 30, value = 15, step = 1, sep = "", ticks=FALSE)),
                          column(2, actionButton("reset_cropRestoration_comp_time", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10, sliderInput("cropRestoration_area_pct", "Available Cropland Area (%):", min = 0, max = 100, value = 0, ticks=FALSE)),
                          column(2, actionButton("reset_cropRestoration_area_pct", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        br(),

                        fluidRow(
                          column(10,
                                 tags$h5("Target Land Cover (%)"),
                                 uiOutput("percentages_table"),
                                 br()
                          )
                        ),

                        fluidRow(
                          column(10, sliderInput("cropRestoration_lc_props", "Adjust Land Cover Proportions", min = 0, max = 100, value = c(33, 66), step = 1,
                                                 ticks=FALSE)),
                          column(2, actionButton("reset_cropRestoration_lc_props", NULL, icon = icon("rotate-left"),
                                                 class = "btn-sm btn-outline-secondary", style = "margin-top: 25px;"))
                        ),
                        fluidRow(
                          column(10,
                                 actionButton("reset_cropRestoration", "Reset")
                          )
                        )

        )
      ),
      br(),
      actionButton("reset_all_sliders", "Restore Defaults")
    ),

    # Main panel (middle)
    column(
      width = 8,
      # tags$h3("Model Projections"),
      plotOutput("sequestrationPlot", height = "360px"),
      plotOutput("wedgePlot", height = "360px"),
      tags$div(
        style = "margin-top: 1em; margin-bottom: 1em; font-size: 20px; line-height: 1.5;",
        uiOutput("summaryText")
      )
    )
  )
)
