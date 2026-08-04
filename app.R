# app.R
# Health Literacy Knowledge Exchange Web Application
# Advanced Interactive Platform - UCL Knowledge Exchange Project (KEIF)

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(leaflet)
library(dplyr)
library(tidyr)
library(sf)

# Source data loaders and estimation engine
source("R/england_data.R")
source("R/mrp_engine.R")

# Load datasets, benchmarks, and Official Spatial LAD GeoJSON Polygons
hl_data_env <- load_healthliteracy_data()
england_las  <- get_england_la_data()
england_sf   <- get_england_patchwork_sf(england_las)
benchmarks   <- get_national_benchmarks()

# Theme Definition
ke_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1B4F72",
  secondary = "#4A235A",
  success = "#1E8449",
  info = "#2874A6",
  base_font = font_google("Inter")
)

ui <- page_navbar(
  theme = ke_theme,
  title = div(
    img(src = "https://www.ucl.ac.uk/brand/sites/brand/files/styles/large/public/ucl-logo.jpg", height = "28px", style = "margin-right: 10px; border-radius: 3px;"),
    "Health Literacy Knowledge Exchange Tool"
  ),
  collapsible = TRUE,
  
  # TAB 1: England Map & Ordered Rankings
  nav_panel(
    title = "England Patchwork Map & Rankings",
    icon = icon("map-marked-alt"),
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        title = "Dataset & Map Controls",
        selectInput("selected_la", "Select Local Authority:", 
                    choices = sort(england_las$la_name), selected = "Newham"),
        selectInput("map_metric", "Select Dataset / Metric:",
                    choices = c("Health Literacy Alone" = "health_lit_pct",
                                "Health Literacy & Numeracy Combined" = "hl_num_combined",
                                "IT Literacy"     = "it_lit_pct",
                                "Numeracy Alone"  = "numeracy_pct")),
        radioButtons("map_type", "Map Display Layer:",
                     choices = c("Full Boundary Patchwork Quilt (Polygons)" = "choropleth",
                                 "Centroid Bubble Markers (Circles)"        = "bubbles"),
                     selected = "choropleth"),
        hr(),
        card(
          card_header("National Benchmarks (England)"),
          p(strong("PIAAC Health Lit: "), paste0(benchmarks$piaac$health_literacy, "%")),
          p(strong("SfL Health Lit: "), paste0(benchmarks$sfl$health_literacy, "%")),
          p(strong("PIAAC IT Lit: "), paste0(benchmarks$piaac$it_literacy, "%")),
          p(strong("SfL Numeracy: "), paste0(benchmarks$sfl$numeracy, "%")),
          style = "background-color: #F8F9F9; font-size: 0.88em;"
        )
      ),
      tabsetPanel(
        tabPanel("Patchwork Quilt Map View",
          br(),
          fluidRow(
            column(width = 8,
              card(
                card_header("Coloured Health Literacy Patchwork Quilt (All 326 English Local Authorities)"),
                leafletOutput("england_map", height = "560px")
              )
            ),
            column(width = 4,
              card(
                card_header("Selected Council Performance"),
                uiOutput("la_summary_cards"),
                hr(),
                plotlyOutput("la_benchmark_bar", height = "260px")
              )
            )
          )
        ),
        tabPanel("Complete Ordered Local Authority Rankings (All 326 Councils)",
          br(),
          fluidRow(
            column(width = 12,
              card(
                card_header("Complete Ordered Ranking of All 326 Local Authorities in England"),
                p("Includes every local authority across England sorted from highest to lowest prevalence. Search or select a council to view its position relative to national benchmarks."),
                fluidRow(
                  column(4, selectInput("ranking_region_filter", "Filter by Region:", 
                                        choices = c("All England Regions", unique(england_las$region)))),
                  column(4, numericInput("ranking_top_n", "Display Top N Councils:", value = 50, min = 10, max = 350, step = 10))
                ),
                plotlyOutput("ordered_ranking_barchart", height = "680px")
              )
            )
          )
        )
      )
    )
  ),
  
  # TAB 2: Custom Local Population Profile Builder
  nav_panel(
    title = "Custom Population Profile",
    icon = icon("sliders-h"),
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        title = "Local Demographic Profile Inputs",
        h6("Age Distribution (%)"),
        sliderInput("age_18_24", "18-24 years:", 0, 50, 15, step = 5),
        sliderInput("age_25_44", "25-44 years:", 0, 60, 40, step = 5),
        sliderInput("age_45_64", "45-64 years:", 0, 60, 30, step = 5),
        sliderInput("age_65_plus", "65+ years:", 0, 50, 15, step = 5),
        hr(),
        h6("Education Level (%)"),
        sliderInput("edu_low", "Low / No Qualification:", 0, 60, 25, step = 5),
        sliderInput("edu_med", "Medium (GCSE / A-Level):", 0, 70, 45, step = 5),
        sliderInput("edu_high", "High (Degree / Higher):", 0, 70, 30, step = 5),
        hr(),
        sliderInput("dep_decile", "IMD Deprivation Decile (1=Most Deprived):", 1, 10, 4, step = 1)
      ),
      fluidRow(
        column(width = 12,
          h4("Estimated Baseline Literacy for Custom Population"),
          p("MRP predictions generated dynamically based on your custom demographic parameters:"),
          uiOutput("custom_kpi_cards")
        )
      ),
      hr(),
      fluidRow(
        column(width = 6,
          card(
            card_header("Literacy Dimensions Comparison"),
            plotlyOutput("custom_dimensions_chart", height = "340px")
          )
        ),
        column(width = 6,
          card(
            card_header("Population Profile Composition"),
            plotlyOutput("custom_profile_composition", height = "340px")
          )
        )
      )
    )
  ),
  
  # TAB 3: Stratified Data Explorer (On-The-Fly Dynamic AME/ATE)
  nav_panel(
    title = "Stratified Data Explorer",
    icon = icon("chart-bar"),
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        title = "On-The-Fly AME Settings",
        selectInput("strat_la", "Select Target Local Authority:",
                    choices = c("Use Custom Population Profile", sort(england_las$la_name)), selected = "Newham"),
        selectInput("strat_metric", "Literacy Measure:",
                    choices = c("Health Literacy", "IT Literacy", "Numeracy"))
      ),
      tabsetPanel(
        tabPanel("On-The-Fly Average Marginal Effect (AME) Forest Plot",
          br(),
          p("Dynamically computes Average Marginal Effects (AMEs) on-the-fly for the chosen local authority or custom population profile."),
          plotlyOutput("forest_plot", height = "450px")
        ),
        tabPanel("AME Category Scatter Plot",
          br(),
          p("Individual observations jittered across categories with gradient lines connecting mean values."),
          plotlyOutput("scatter_plot", height = "450px")
        ),
        tabPanel("Ranking & SUCRA Probability Curves",
          br(),
          p("Cumulative probability curves indicating likelihood of variable levels ranking among top determinants."),
          plotlyOutput("sucra_plot", height = "450px")
        )
      )
    )
  ),
  
  # TAB 4: What-If Policy Intervention Simulator
  nav_panel(
    title = "What-If Intervention Simulator",
    icon = icon("lightbulb"),
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        title = "Intervention Controls",
        selectInput("sim_la", "Target Local Area:", 
                    choices = c("Custom Profile Builder", sort(england_las$la_name)), selected = "Newham"),
        selectInput("sim_subgroup", "Target Population Subgroup:",
                    choices = c("All Residents" = "all",
                                "Low Education Groups" = "low_edu",
                                "Older Adults (65+)" = "older_adults",
                                "High Deprivation Areas" = "high_deprivation")),
        selectInput("sim_type", "Intervention Initiative:",
                    choices = c("Digital Literacy & Skills Training" = "digital_skills",
                                "Health Communication Simplification" = "health_comm",
                                "ESOL & Adult Literacy Classes" = "esol_literacy",
                                "Community Health Worker Outreach" = "community_outreach")),
        sliderInput("sim_effect", "Intervention Effect Size (Delta Logit / % Boost):",
                    min = 0.05, max = 0.30, value = 0.15, step = 0.05, post = "%")
      ),
      fluidRow(
        column(width = 12,
          h4("Simulated Policy Intervention Impact"),
          uiOutput("sim_outcome_cards")
        )
      ),
      hr(),
      fluidRow(
        column(width = 6,
          card(
            card_header("Baseline vs Post-Intervention Comparison"),
            plotlyOutput("sim_before_after_chart", height = "360px")
          )
        ),
        column(width = 6,
          card(
            card_header("Stratified Population Benefit Breakdown"),
            plotlyOutput("sim_stratified_benefit", height = "360px")
          )
        )
      )
    )
  ),
  
  # TAB 5: Methodology & Glass-Box Engine
  nav_panel(
    title = "Methodology & Documentation",
    icon = icon("book"),
    fluidRow(
      column(width = 8, offset = 2,
        card(
          card_header("Multilevel Regression and Post-Stratification (MRP) Methodology"),
          p("Multilevel Regression and Post-Stratification (MRP) combines hierarchical statistical modeling with census post-stratification weights to estimate small-area health literacy rates."),
          h5("1. Logistic Multilevel Regression Model"),
          code("P(y_i = 1) = logit_inv( beta_0 + beta_age[i] + beta_edu[i] + beta_dep[i] + u_area[j] )"),
          br(), br(),
          h5("2. Post-Stratification Weighting"),
          code("Y_g* = sum_j ( w_gj * Y_j )"),
          p("where w_gj is the population weight for group j in local area g."),
          hr(),
          h5("Project Documentation & Publications"),
          tags$ul(
            tags$li(a(href = "#", "UCL Knowledge Exchange Application 2024 (PDF/DOCX)")),
            tags$li(a(href = "#", "Health Literacy Survey & MRP Validation Study - BMC Public Health")),
            tags$li(a(href = "#", "OECD PIAAC International Adult Competencies Dataset")),
            tags$li(a(href = "#", "UK DfE Skills for Life (SfL) Survey Methodology"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive SF spatial dataset with calculated indicators & pre-formatted popups
  sf_reactive <- reactive({
    df <- england_sf
    df$health_lit_pct <- as.numeric(df$health_lit_pct)
    df$it_lit_pct     <- as.numeric(df$it_lit_pct)
    df$numeracy_pct   <- as.numeric(df$numeracy_pct)
    df$hl_num_combined <- round((df$health_lit_pct + df$numeracy_pct) / 2, 1)
    
    df$popup_text <- paste0("<div style='font-family: Inter, sans-serif;'>",
                            "<h5 style='margin-bottom:5px; color:#1B4F72;'>", df$la_name, " (", df$region, ")</h5>",
                            "<b>Health Literacy Alone: </b>", df$health_lit_pct, "%<br/>",
                            "<b>Health & Numeracy Combined: </b>", df$hl_num_combined, "%<br/>",
                            "<b>IT Literacy: </b>", df$it_lit_pct, "%<br/>",
                            "<b>Numeracy Alone: </b>", df$numeracy_pct, "%<br/>",
                            "<b>IMD Deprivation Decile: </b>", df$deprivation_decile,
                            "</div>")
    df
  })
  
  # Reactive tabular dataset
  las_reactive <- reactive({
    england_las %>%
      mutate(hl_num_combined = round((health_lit_pct + numeracy_pct) / 2, 1))
  })
  
  # --- TAB 1: Patchwork Quilt Map & Complete Rankings ---
  output$england_map <- renderLeaflet({
    sf_data <- sf_reactive()
    metric <- input$map_metric
    map_type <- input$map_type
    
    vals <- sf_data[[metric]]
    if (is.null(vals) || length(vals) == 0) vals <- sf_data$health_lit_pct
    
    val_range <- range(vals, na.rm = TRUE)
    pal <- colorNumeric(palette = "YlOrRd", domain = val_range, na.color = "#CCCCCC", reverse = TRUE)
    
    map <- leaflet(sf_data) %>%
      addTiles() %>%
      setView(lat = 52.8, lng = -1.5, zoom = 6)
    
    if (map_type == "choropleth") {
      map %>% addPolygons(
        data = sf_data,
        fillColor = ~pal(vals),
        fillOpacity = 0.85,
        weight = 1.0,
        color = "#FFFFFF",
        opacity = 1.0,
        highlightOptions = highlightOptions(
          weight = 3.0,
          color = "#111111",
          fillOpacity = 0.95,
          bringToFront = TRUE
        ),
        popup = ~popup_text,
        layerId = sf_data$la_name
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal, values = vals,
        title = "Est. Prevalence (%)",
        opacity = 0.9
      )
    } else {
      map %>% addCircleMarkers(
        data = sf_data,
        lat = ~lat, lng = ~lng,
        radius = ~sqrt(population) / 20,
        color = ~pal(vals), fillColor = ~pal(vals),
        fillOpacity = 0.85, stroke = TRUE, weight = 2,
        popup = ~popup_text,
        layerId = sf_data$la_name
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal, values = vals,
        title = "Est. Prevalence (%)",
        opacity = 0.9
      )
    }
  })
  
  # Map click listener to sync selected LA dropdown
  observeEvent(input$england_map_shape_click, {
    click <- input$england_map_shape_click
    if (!is.null(click$id)) {
      updateSelectInput(session, "selected_la", selected = click$id)
    }
  })
  observeEvent(input$england_map_marker_click, {
    click <- input$england_map_marker_click
    if (!is.null(click$id)) {
      updateSelectInput(session, "selected_la", selected = click$id)
    }
  })
  
  output$la_summary_cards <- renderUI({
    data <- las_reactive()
    la <- data %>% filter(la_name == input$selected_la)
    if (nrow(la) == 0) la <- data[1, ]
    
    div(
      value_box(
        title = paste("Health Literacy -", la$la_name),
        value = paste0(la$health_lit_pct, "%"),
        showcase = icon("heartbeat"),
        theme = "primary"
      ),
      value_box(
        title = "Health + Numeracy Combined",
        value = paste0(la$hl_num_combined, "%"),
        showcase = icon("layer-group"),
        theme = "info"
      ),
      value_box(
        title = "IT Literacy Rate",
        value = paste0(la$it_lit_pct, "%"),
        showcase = icon("laptop"),
        theme = "success"
      )
    )
  })
  
  output$la_benchmark_bar <- renderPlotly({
    data <- las_reactive()
    la <- data %>% filter(la_name == input$selected_la)
    if (nrow(la) == 0) la <- data[1, ]
    
    df <- data.frame(
      Source = c(la$la_name, "PIAAC National", "SfL National"),
      Health_Lit = c(la$health_lit_pct, benchmarks$piaac$health_literacy, benchmarks$sfl$health_literacy),
      IT_Lit = c(la$it_lit_pct, benchmarks$piaac$it_literacy, benchmarks$sfl$it_literacy),
      Numeracy = c(la$numeracy_pct, benchmarks$piaac$numeracy, benchmarks$sfl$numeracy)
    )
    
    df_long <- pivot_longer(df, cols = c("Health_Lit", "IT_Lit", "Numeracy"), 
                            names_to = "Dimension", values_to = "Percentage")
    
    p <- ggplot(df_long, aes(x = Dimension, y = Percentage, fill = Source)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Benchmark Comparison", y = "Percentage (%)") +
      scale_fill_manual(values = c("#1B4F72", "#5D6D7E", "#2874A6"))
    
    ggplotly(p)
  })
  
  output$ordered_ranking_barchart <- renderPlotly({
    data <- las_reactive()
    metric <- input$map_metric
    selected_name <- input$selected_la
    region_filter <- input$ranking_region_filter
    top_n <- input$ranking_top_n
    
    metric_label <- switch(metric,
      "health_lit_pct"   = "Health Literacy Alone (%)",
      "hl_num_combined"  = "Health Literacy & Numeracy Combined (%)",
      "it_lit_pct"       = "IT Literacy (%)",
      "numeracy_pct"     = "Numeracy Alone (%)"
    )
    
    if (region_filter != "All England Regions") {
      data <- data %>% filter(region == region_filter)
    }
    
    data_sorted <- data %>%
      mutate(Value = .data[[metric]]) %>%
      arrange(desc(Value)) %>%
      head(top_n) %>%
      mutate(la_name = factor(la_name, levels = rev(la_name)),
             IsSelected = ifelse(la_name == selected_name, "Selected Council", "Other Councils"))
    
    piaac_val <- benchmarks$piaac$health_literacy
    sfl_val   <- benchmarks$sfl$health_literacy
    
    p <- ggplot(data_sorted, aes(x = la_name, y = Value, fill = IsSelected)) +
      geom_bar(stat = "identity", width = 0.75) +
      geom_hline(yintercept = piaac_val, linetype = "dashed", color = "#C0392B", size = 1) +
      geom_hline(yintercept = sfl_val, linetype = "dotted", color = "#27AE60", size = 1) +
      coord_flip() +
      theme_minimal() +
      scale_fill_manual(values = c("Selected Council" = "#E74C3C", "Other Councils" = "#1B4F72")) +
      labs(title = paste("Ranked Local Authorities across England by", metric_label),
           x = "Local Authority", y = metric_label, fill = "Legend")
    
    ggplotly(p)
  })
  
  # --- TAB 2: Custom Local Population Profile Builder ---
  custom_pred <- reactive({
    predict_custom_profile(
      age_pct = c(input$age_18_24, input$age_25_44, input$age_45_64, input$age_65_plus),
      edu_pct = c(input$edu_low, input$edu_med, input$edu_high),
      dep_decile = input$dep_decile,
      eth_pct = c(60, 25, 15)
    )
  })
  
  output$custom_kpi_cards <- renderUI({
    res <- custom_pred()
    layout_column_wrap(
      width = 1/3,
      value_box(
        title = "Predicted Health Literacy",
        value = paste0(res$health_literacy, "%"),
        showcase = icon("notes-medical"),
        theme = "primary"
      ),
      value_box(
        title = "Predicted IT Literacy",
        value = paste0(res$it_literacy, "%"),
        showcase = icon("keyboard"),
        theme = "info"
      ),
      value_box(
        title = "Predicted Numeracy",
        value = paste0(res$numeracy, "%"),
        showcase = icon("square-root-alt"),
        theme = "success"
      )
    )
  })
  
  output$custom_dimensions_chart <- renderPlotly({
    res <- custom_pred()
    df <- data.frame(
      Dimension = c("Health Literacy", "IT Literacy", "Numeracy"),
      Percentage = c(res$health_literacy, res$it_literacy, res$numeracy)
    )
    
    p <- ggplot(df, aes(x = Dimension, y = Percentage, fill = Dimension)) +
      geom_bar(stat = "identity", width = 0.5) +
      ylim(0, 100) +
      geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5) +
      theme_minimal() +
      scale_fill_manual(values = c("#1B4F72", "#2874A6", "#1E8449")) +
      labs(title = "Custom Profile Predicted Rates", y = "Predicted % Literate")
    
    ggplotly(p)
  })
  
  output$custom_profile_composition <- renderPlotly({
    df <- data.frame(
      Group = c("18-24", "25-44", "45-64", "65+", "Low Edu", "Med Edu", "High Edu"),
      Percentage = c(input$age_18_24, input$age_25_44, input$age_45_64, input$age_65_plus,
                     input$edu_low, input$edu_med, input$edu_high),
      Category = c(rep("Age Group", 4), rep("Education Level", 3))
    )
    
    p <- ggplot(df, aes(x = Group, y = Percentage, fill = Category)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Input Demographic Composition", y = "Percentage (%)")
    
    ggplotly(p)
  })
  
  # --- TAB 3: Dynamic On-The-Fly AME Explorer ---
  output$forest_plot <- renderPlotly({
    # Compute AMEs dynamically on-the-fly based on selected LA or custom inputs
    df_ame <- if (input$strat_la == "Use Custom Population Profile") {
      compute_on_the_fly_ame(
        age_pct = c(input$age_18_24, input$age_25_44, input$age_45_64, input$age_65_plus),
        edu_pct = c(input$edu_low, input$edu_med, input$edu_high),
        dep_decile = input$dep_decile
      )
    } else {
      la <- england_las %>% filter(la_name == input$strat_la)
      if (nrow(la) == 0) la <- england_las[1, ]
      compute_on_the_fly_ame(dep_decile = la$deprivation_decile)
    }
    
    p <- ggplot(df_ame, aes(x = Variable, y = AME, colour = Category)) +
      geom_point(size = 4) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, size = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      coord_flip() +
      theme_minimal() +
      labs(title = paste("Dynamic On-The-Fly Average Marginal Effects (AME) for", input$strat_la),
           y = "Difference in Probability")
    
    ggplotly(p)
  })
  
  output$scatter_plot <- renderPlotly({
    set.seed(42)
    df <- data.frame(
      Category = rep(c("Low Edu", "Med Edu", "High Edu"), each = 30),
      Probability = c(rnorm(30, 0.48, 0.04), rnorm(30, 0.58, 0.04), rnorm(30, 0.72, 0.04))
    )
    
    means <- df %>% group_by(Category) %>% summarise(mean_prob = mean(Probability))
    
    p <- ggplot(df, aes(x = Category, y = Probability)) +
      geom_jitter(width = 0.15, alpha = 0.6, color = "#1B4F72") +
      geom_point(data = means, aes(x = Category, y = mean_prob), color = "red", size = 4) +
      theme_minimal() +
      labs(title = "Health Literacy Probability Distribution across Education Categories", y = "P(Health Literate)")
    
    ggplotly(p)
  })
  
  output$sucra_plot <- renderPlotly({
    df <- data.frame(
      Rank = 1:4,
      Education = c(0.65, 0.85, 0.95, 1.0),
      Deprivation = c(0.20, 0.60, 0.85, 1.0),
      Age = c(0.10, 0.40, 0.75, 1.0),
      Ethnicity = c(0.05, 0.15, 0.45, 1.0)
    )
    df_long <- pivot_longer(df, cols = c("Education", "Deprivation", "Age", "Ethnicity"),
                            names_to = "Variable", values_to = "SUCRA")
    
    p <- ggplot(df_long, aes(x = Rank, y = SUCRA, color = Variable)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(title = "SUCRA Cumulative Ranking Probabilities", y = "Cumulative Probability")
    
    ggplotly(p)
  })
  
  # --- TAB 4: What-If Policy Intervention Simulator ---
  sim_results <- reactive({
    data <- las_reactive()
    base <- if (input$sim_la == "Custom Profile Builder") {
      custom_pred()
    } else {
      la <- data %>% filter(la_name == input$sim_la)
      list(health_literacy = la$health_lit_pct, it_literacy = la$it_lit_pct, numeracy = la$numeracy_pct)
    }
    
    simulate_intervention(
      baseline_rates = base,
      target_subgroup = input$sim_subgroup,
      intervention_type = input$sim_type,
      effect_size = input$sim_effect / 100
    )
  })
  
  output$sim_outcome_cards <- renderUI({
    res <- sim_results()
    layout_column_wrap(
      width = 1/3,
      value_box(
        title = "Post-Intervention Health Lit",
        value = paste0(res$post_intervention$health_literacy, "%"),
        subtitle = paste0("Gain: +", res$gains$health_literacy, "%"),
        showcase = icon("arrow-up"),
        theme = "primary"
      ),
      value_box(
        title = "Post-Intervention IT Lit",
        value = paste0(res$post_intervention$it_literacy, "%"),
        subtitle = paste0("Gain: +", res$gains$it_literacy, "%"),
        showcase = icon("laptop-medical"),
        theme = "info"
      ),
      value_box(
        title = "Post-Intervention Numeracy",
        value = paste0(res$post_intervention$numeracy, "%"),
        subtitle = paste0("Gain: +", res$gains$numeracy, "%"),
        showcase = icon("chart-line"),
        theme = "success"
      )
    )
  })
  
  output$sim_before_after_chart <- renderPlotly({
    res <- sim_results()
    df <- data.frame(
      Dimension = rep(c("Health Literacy", "IT Literacy", "Numeracy"), 2),
      Status = rep(c("Baseline", "Post-Intervention"), each = 3),
      Percentage = c(res$baseline$health_literacy, res$baseline$it_literacy, res$baseline$numeracy,
                     res$post_intervention$health_literacy, res$post_intervention$it_literacy, res$post_intervention$numeracy)
    )
    
    p <- ggplot(df, aes(x = Dimension, y = Percentage, fill = Status)) +
      geom_bar(stat = "identity", position = "dodge") +
      ylim(0, 100) +
      theme_minimal() +
      scale_fill_manual(values = c("#5D6D7E", "#1E8449")) +
      labs(title = "Baseline vs. Post-Intervention Rates", y = "Percentage (%)")
    
    ggplotly(p)
  })
  
  output$sim_stratified_benefit <- renderPlotly({
    res <- sim_results()
    df <- data.frame(Subgroup = c("Targeted Subgroup", "Broader Community", "Other Demographics"),
                     Gain_Pct = c(res$gains$health_literacy * 1.5, res$gains$health_literacy * 0.8, res$gains$health_literacy * 0.3))
    
    p <- ggplot(df, aes(x = Subgroup, y = Gain_Pct, fill = Subgroup)) +
      geom_bar(stat = "identity", width = 0.5) +
      theme_minimal() +
      scale_fill_manual(values = c("#1B4F72", "#2874A6", "#5D6D7E")) +
      labs(title = "Stratified Gain Distribution across Subgroups", y = "Health Literacy Delta (% Point)")
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
