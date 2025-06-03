server <- function(input, output, session) {
  observe({
    tree_percentage <- input$cropRestoration_lc_props[1]
    wetland_percentage <- 100 - input$cropRestoration_lc_props[2]
    grassland_percentage <- 100 - (tree_percentage + wetland_percentage)

    if (wetland_percentage < 0) {
      grassland_percentage <- 100 - tree_percentage
      wetland_percentage <- 0
    }

    output$percentages_table <- renderUI({
      tags$table(
        style = "width: 100%; font-size: 12px;",
        tags$tbody(
          tags$tr(
            tags$td(tags$strong("Tree:"), style = "text-align: left; padding-right: 10px;"),
            tags$td(tree_percentage, style = "text-align: right;")
          ),
          tags$tr(
            tags$td(tags$strong("Grassland:"), style = "text-align: left; padding-right: 10px;"),
            tags$td(grassland_percentage, style = "text-align: right;")
          ),
          tags$tr(
            tags$td(tags$strong("Wetland:"), style = "text-align: left; padding-right: 10px;"),
            tags$td(wetland_percentage, style = "text-align: right;")
          )
        )
      )
    })
  })





  observeEvent(input$reset_urbanTreePlanting_area_pct, {
    updateSliderInput(session, "urbanTreePlanting_area_pct", value = 0)
  })
  observeEvent(input$reset_urbanTreePlanting_start_yr, {
    updateSliderInput(session, "urbanTreePlanting_start_yr", value = 2025)
  })
  observeEvent(input$reset_urbanTreePlanting_comp_time, {
    updateSliderInput(session, "urbanTreePlanting_comp_time", value = 15)
  })

  observeEvent(input$reset_lawnsToLegumes_area_pct, {
    updateSliderInput(session, "lawnsToLegumes_area_pct", value = 0)
  })
  observeEvent(input$reset_lawnsToLegumes_start_yr, {
    updateSliderInput(session, "lawnsToLegumes_start_yr", value = 2025)
  })
  observeEvent(input$reset_lawnsToLegumes_comp_time, {
    updateSliderInput(session, "lawnsToLegumes_comp_time", value = 15)
  })


  observeEvent(input$reset_cropRestoration_area_pct, {
    updateSliderInput(session, "cropRestoration_area_pct", value = 0)
  })
  observeEvent(input$reset_cropRestoration_start_yr, {
    updateSliderInput(session, "cropRestoration_start_yr", value = 2025)
  })
  observeEvent(input$reset_cropRestoration_comp_time, {
    updateSliderInput(session, "cropRestoration_comp_time", value = 15)
  })
  observeEvent(input$reset_cropRestoration_lc_props, {
    updateSliderInput(session, "cropRestoration_lc_props", value = c(33, 66))
  })


  # Observer to reset sliders to default values
  observeEvent(input$reset_all_sliders, {
    updateSliderInput(session, "cropRestoration_area_pct", value = 0)
    updateSliderInput(session, "cropRestoration_comp_time", value = 15)
    updateSliderInput(session, "cropRestoration_start_yr", value = 2025)
    updateSliderInput(session, "cropRestoration_lc_props", value = c(33, 66))

    updateSliderInput(session, "urbanTreePlanting_area_pct", value = 0)
    updateSliderInput(session, "urbanTreePlanting_start_yr", value = 2025)
    updateSliderInput(session, "urbanTreePlanting_comp_time", value = 15)

    updateSliderInput(session, "lawnsToLegumes_area_pct", value = 0)
    updateSliderInput(session, "lawnsToLegumes_start_yr", value = 2025)
    updateSliderInput(session, "lawnsToLegumes_comp_time", value = 15)
  })


  # Observer to reset sliders to default values
  observeEvent(input$reset_cropRestoration, {
    updateSliderInput(session, "cropRestoration_area_pct", value = 0)
    updateSliderInput(session, "cropRestoration_comp_time", value = 15)
    updateSliderInput(session, "cropRestoration_start_yr", value = 2025)
    updateSliderInput(session, "cropRestoration_lc_props", value = c(33, 66))
  })


  # Observer to reset sliders to default values
  observeEvent(input$reset_urbanTreePlanting, {
    updateSliderInput(session, "urbanTreePlanting_area_pct", value = 0)
    updateSliderInput(session, "urbanTreePlanting_start_yr", value = 2025)
    updateSliderInput(session, "urbanTreePlanting_comp_time", value = 15)
  })


  # Observer to reset sliders to default values
  observeEvent(input$reset_lawnsToLegumes, {
    updateSliderInput(session, "lawnsToLegumes_area_pct", value = 0)
    updateSliderInput(session, "lawnsToLegumes_start_yr", value = 2025)
    updateSliderInput(session, "lawnsToLegumes_comp_time", value = 15)
  })




  future_years <- 2023:2050


  filtered_data <- reactiveVal(NULL) # container for dynamically loaded dataset
  null_data <- reactiveVal(NULL) # container for dynamically loaded dataset


  observe({
    if (input$selected_tab == "County") {
      filtered_data(filter(hist_data_county, county_name == input$selected_county))
      null_data(filter(null_data_county, county_name == input$selected_county))
    } else if (input$selected_tab == "CTU") {
      filtered_data(filter(hist_data_ctu, ctu_name == input$selected_ctu))
      null_data(filter(null_data_ctu, ctu_name == input$selected_ctu))
    }
  })




  # # Reactive data for selected county
  # filtered_data <- reactive({
  #   hist_data_county %>%
  #     filter(county_name == input$selected_county)
  # })
  #
  #
  # null_data <- reactive({
  #   null_data_county %>%
  #     filter(county_name == input$selected_county)
  # })


  # Module 1 - Lawns to Legumes ---------------------------------------------
  mod1_lawnsToLegumes_results <- reactive({
    mod1_lawnsToLegumes(
      df_hist = filtered_data(),
      df_null = null_data(),
      start_yr = input$lawnsToLegumes_start_yr,
      comp_time = input$lawnsToLegumes_comp_time,
      area_pct = input$lawnsToLegumes_area_pct
    )
  })

  # Module 2 - Urban Tree Planting ---------------------------------------------
  mod2_urbanTreePlanting_results <- reactive({
    mod2_urbanTreePlanting(
      df_hist = filtered_data(),
      df_null = null_data(),
      start_yr = input$urbanTreePlanting_start_yr,
      comp_time = input$urbanTreePlanting_comp_time,
      area_pct = input$urbanTreePlanting_area_pct
    )
  })

  # Module 3 - Restoration ---------------------------------------------
  mod3_cropRestoration_results <- reactive({
    tree_pct <- input$cropRestoration_lc_props[1]
    wetland_pct <- 100 - input$cropRestoration_lc_props[2]
    grass_pct <- 100 - (tree_pct + wetland_pct)

    if (wetland_pct < 0) {
      grass_pct <- 100 - tree_pct
      wetland_pct <- 0
    }

    mod3_cropRestoration(
      df_hist = filtered_data(),
      df_null = null_data(),
      start_yr = input$cropRestoration_start_yr,
      comp_time = input$cropRestoration_comp_time,
      area_pct = input$cropRestoration_area_pct,
      tree_pct = tree_pct,
      grass_pct = grass_pct,
      wetland_pct = wetland_pct
    )
  })

  # Reactive data for projections
  projected_data <- reactive({
    all_model_results <-
      combine_module_deltas(
        mod1_output = mod1_lawnsToLegumes_results(),
        mod2_output = mod2_urbanTreePlanting_results(),
        mod3_output = mod3_cropRestoration_results()
      )

    df_projections <- null_data() %>%
      left_join(all_model_results, by = "inventory_year") %>%
      mutate(across(starts_with("delta_"), ~ replace_na(., 0))) %>%
      mutate(
        Grassland = Grassland + delta_Grassland,
        Tree = Tree + delta_Tree,
        Urban_Tree = Urban_Tree + delta_Urban_Tree,
        Cropland = Cropland + delta_Cropland,
        Wetland = Wetland + delta_Wetland,
        Urban_Grassland = Urban_Grassland + delta_Urban_Grassland,
        Developed_Low = Developed_Low + delta_Developed_Low,
        Developed_Med = Developed_Med + delta_Developed_Med,
        Developed_High = Developed_High + delta_Developed_High
      ) %>%
      select(-starts_with("delta_")) %>% # optional: remove delta columns
      mutate(newTOTAL = rowSums(across(c(
        Bare, Developed_Low, Developed_Med, Developed_High,
        Urban_Grassland, Urban_Tree,
        Cropland, Grassland, Tree, Water,
        Wetland
      ))))

    combined_data <- rbind(
      filtered_data(),
      df_projections %>% select(-TOTAL) %>% rename(TOTAL = newTOTAL)
    )

    combined_data
  })




  theme_settings <- function() {
    theme(
      plot.title = element_text(size = 17, face = "bold"),
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
      pivot_longer(cols = c(
        Bare, Developed_Low, Developed_Med, Developed_High,
        Urban_Grassland, Urban_Tree,
        Cropland, Grassland, Tree, Water,
        Wetland
      ), names_to = "land_cover_type", values_to = "area") %>%
      mutate(land_cover_type = factor(land_cover_type,
        levels =
          c(
            "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
            "Cropland", "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Water"
          )
      )) %>%
      filter(land_cover_type != "TOTAL")



    lc_brks <- c(
      "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
      "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Cropland", "Water"
    )
    lc_labs <- c(
      "Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland",
      "Bare", "Developed (Low)", "Developed (Med)", "Developed (High)", "Cropland", "Water"
    )


    p1 <- ggplot(plot_data) +
      geom_area(aes(x = inventory_year, y = area, fill = land_cover_type), alpha = 0.5, show.legend = F) +
      geom_area(aes(x = inventory_year, y = area, color = land_cover_type), fill = NA, linewidth = 1, show.legend = F) +
      geom_vline(xintercept = 2023, linetype = "dashed", alpha = 0.5) +
      scale_fill_manual(values = plot_colors, breaks = lc_brks, labels = lc_labs) +
      scale_color_manual(values = plot_colors, breaks = lc_brks, labels = lc_labs) +
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
        filter(inventory_year == future_years[1] - 1) %>%
        mutate(tag = factor("initial", levels = c("initial", "final"))),
      plot_data %>%
        filter(inventory_year == 2050) %>%
        mutate(tag = factor("final", levels = c("initial", "final")))
    ) %>%
      arrange(tag) %>%
      # filter(!is.na(sequestration_potential)) %>%
      mutate(inventory_year = factor(inventory_year, levels = c(as.character(future_years[1] - 1), as.character(2050)))) %>%
      ggplot() +
      theme_minimal() +
      geom_col(aes(x = inventory_year, y = area, fill = land_cover_type), alpha = 0.5) +
      geom_col(aes(x = inventory_year, y = area, color = land_cover_type), fill = NA, linewidth = 1, show.legend = F) +
      scale_fill_manual(values = plot_colors, breaks = lc_brks, labels = lc_labs) +
      scale_color_manual(values = plot_colors, breaks = lc_brks, labels = lc_labs) +
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


    plot_lab <- if (input$selected_tab == "County") {
      paste(input$selected_county, "County")
    } else {
      paste(input$selected_ctu)
    }


    wrap_elements(p1 + p2 + plot_layout(widths = c(9, 1))) +
      labs(title = paste("Land Cover Change Projection for", plot_lab)) +
      theme_minimal() +
      theme_settings()
  })

  # Render Sequestration Plot
  output$sequestrationPlot <- renderPlot({
    combined_data <- projected_data()

    # Transform data for ggplot (long format)
    plot_data <- combined_data %>%
      pivot_longer(cols = c(
        Bare, Developed_Low, Developed_Med, Developed_High,
        Urban_Grassland, Urban_Tree,
        Cropland, Grassland, Tree, Water,
        Wetland
      ), names_to = "land_cover_type", values_to = "area") %>%
      mutate(land_cover_type = factor(land_cover_type,
        levels =
          c(
            "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
            "Cropland", "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Water"
          )
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
      pull(sequestration_potential) %>%
      sum()


    final_sequestration <- plot_data %>%
      filter(inventory_year == 2050) %>%
      ungroup() %>%
      pull(sequestration_potential) %>%
      sum()

    seq_brks <- c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland")
    seq_labs <- c("Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland")

    p1 <- ggplot(plot_data) +
      geom_area(aes(x = inventory_year, y = sequestration_potential, fill = land_cover_type), alpha = 0.5, show.legend = F) +
      geom_area(aes(x = inventory_year, y = sequestration_potential, color = land_cover_type), fill = NA, linewidth = 1, show.legend = F) +
      geom_vline(xintercept = 2023, linetype = "dashed", alpha = 0.5) +
      geom_hline(yintercept = baseline_sequestration, linetype = "dashed", alpha = 0.5) +
      geom_hline(yintercept = final_sequestration, linetype = "dashed", alpha = 0.5) +
      scale_fill_manual(values = plot_colors, breaks = seq_brks, labels = seq_labs) +
      scale_color_manual(values = plot_colors, breaks = seq_brks, labels = seq_labs) +
      labs(
        title = NULL,
        x = "Year",
        y = expression("Metric tons" ~ CO[2] * e),
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
        filter(inventory_year == future_years[1] - 1) %>%
        mutate(tag = factor("initial", levels = c("initial", "final"))),
      plot_data %>%
        filter(inventory_year == 2050) %>%
        mutate(tag = factor("final", levels = c("initial", "final")))
    ) %>%
      arrange(tag) %>%
      filter(!is.na(sequestration_potential)) %>%
      mutate(inventory_year = factor(inventory_year, levels = c(as.character(future_years[1] - 1), as.character(2050)))) %>%
      ggplot() +
      theme_minimal() +
      geom_col(aes(x = inventory_year, y = sequestration_potential, fill = land_cover_type), alpha = 0.5) +
      geom_col(aes(x = inventory_year, y = sequestration_potential, color = land_cover_type), fill = NA, linewidth = 1, show.legend = F) +
      scale_fill_manual(values = plot_colors, breaks = seq_brks, labels = seq_labs) +
      scale_color_manual(values = plot_colors, breaks = seq_brks, labels = seq_labs) +
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

    plot_lab <- if (input$selected_tab == "County") {
      paste(input$selected_county, "County")
    } else {
      paste(input$selected_ctu)
    }

    wrap_elements(p1 + p2 + plot_layout(widths = c(9, 1))) +
      labs(title = paste("Sequestration potential for", plot_lab)) +
      theme_minimal() +
      theme_settings()
  })

  output$summaryPlot1 <- renderPlot({
    combined_data <- projected_data()

    # Transform data for ggplot (long format)
    plot_data <- combined_data %>%
      pivot_longer(cols = c(
        Bare, Developed_Low, Developed_Med, Developed_High,
        Urban_Grassland, Urban_Tree,
        Cropland, Grassland, Tree, Water,
        Wetland
      ), names_to = "land_cover_type", values_to = "area") %>%
      mutate(land_cover_type = factor(land_cover_type,
        levels =
          c(
            "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
            "Cropland", "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Water"
          )
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
        filter(inventory_year == future_years[1] - 1) %>%
        mutate(tag = factor("initial", levels = c("initial", "final"))),
      plot_data %>%
        filter(inventory_year == 2050) %>%
        mutate(tag = factor("final", levels = c("initial", "final")))
    ) %>% arrange(tag)

    seq_brks <- c("Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland")
    seq_labs <- c("Tree", "Grassland", "Wetland", "Urban Tree", "Urban Grassland")

    plot_data %>%
      filter(!is.na(sequestration_potential)) %>%
      mutate(inventory_year = factor(inventory_year, levels = c(as.character(future_years[1] - 1), as.character(2050)))) %>%
      ggplot() +
      theme_minimal() +
      geom_col(aes(x = inventory_year, y = sequestration_potential, fill = land_cover_type), alpha = 0.5) +
      geom_col(aes(x = inventory_year, y = sequestration_potential, color = land_cover_type), fill = NA, linewidth = 1, show.legend = F) +
      scale_fill_manual(values = plot_colors, breaks = seq_brks, labels = seq_labs) +
      scale_color_manual(values = plot_colors, breaks = seq_brks, labels = seq_labs) +
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
  })


  output$summaryText <- renderUI({
    # Transform data for summary
    summary_data <- projected_data() %>%
      pivot_longer(cols = c(
        Bare, Developed_Low, Developed_Med, Developed_High,
        Urban_Grassland, Urban_Tree,
        Cropland, Grassland, Tree, Water,
        Wetland
      ), names_to = "land_cover_type", values_to = "area") %>%
      mutate(land_cover_type = factor(land_cover_type,
        levels =
          c(
            "Tree", "Grassland", "Wetland", "Urban_Tree", "Urban_Grassland",
            "Cropland", "Bare", "Developed_Low", "Developed_Med", "Developed_High", "Water"
          )
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
      pull(sequestration_potential) %>%
      sum()


    final_sequestration <- summary_data %>%
      filter(inventory_year == 2050) %>%
      ungroup() %>%
      pull(sequestration_potential) %>%
      sum()


    seq_change_pct <- round(((final_sequestration - baseline_sequestration) / baseline_sequestration) * 100, 1)
    seq_change_actual <- round(abs(final_sequestration - baseline_sequestration) / 1000, 0)



    geog_name <- if (input$selected_tab == "County") {
      paste(input$selected_county, "County")
    } else {
      paste(input$selected_ctu)
    }


    if (seq_change_pct > 0) {
      HTML(paste0(
        "+",
        seq_change_pct,
        "% increase (+",
        seq_change_actual,
        "k metric tons CO<sub>2</sub>e) in C sequestered by natural systems in ",
        geog_name,
        "."
      ))
    } else if (seq_change_pct < 0) {
      HTML(paste0(
        seq_change_pct,
        "% decrease (",
        seq_change_actual,
        "k metric tons CO<sub>2</sub>e) in C sequestered by natural systems in ",
        geog_name,
        "."
      ))
    } else {
      HTML("No change in C sequestered by natural systems.")
    }
  })
}
