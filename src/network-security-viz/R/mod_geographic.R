# Geographic Module - Simplified
# Network Security Visualization Dashboard
# Replaces complex mod_geo_3d.R with simple country-level choropleth

# UI function
geographicUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Control panel
      column(3,
        wellPanel(
          h4("Geographic Controls", style = "color: #00d4ff;"),
          
          # Metric selector
          selectInput(
            ns("metric"),
            "Display Metric",
            choices = c(
              "Attack Count" = "count",
              "Threat Score" = "threat_score",
              "Data Volume" = "volume"
            ),
            selected = "count"
          ),
          
          # Color palette
          selectInput(
            ns("color_palette"),
            "Color Palette",
            choices = c(
              "Threat (Plasma)" = "plasma",
              "Geographic (Viridis)" = "viridis",
              "Classic Security" = "security"
            ),
            selected = "plasma"
          ),
          
          # Filter controls
          hr(),
          h5("Filters", style = "color: #00d4ff;"),
          sliderInput(
            ns("min_attacks"),
            "Minimum Attacks",
            min = 1, max = 100, value = 1
          )
        )
      ),
      
      # Main visualization
      column(9,
        tabsetPanel(
          tabPanel(
            "World Map",
            br(),
            plotlyOutput(ns("world_map"), height = "600px")
          ),
          tabPanel(
            "Country Statistics", 
            br(),
            DT::dataTableOutput(ns("country_table"))
          )
        )
      )
    ),
    
    # Summary row
    fluidRow(
      column(3,
        valueBoxOutput(ns("total_countries"), width = NULL)
      ),
      column(3,
        valueBoxOutput(ns("top_threat_country"), width = NULL)
      ),
      column(3,
        valueBoxOutput(ns("total_attacks"), width = NULL)
      ),
      column(3,
        valueBoxOutput(ns("avg_threat"), width = NULL)
      )
    )
  )
}

# Server function
geographicServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Process geographic data
    geo_data <- reactive({
      req(data())
      
      df <- data() %>%
        filter(!is.na(source_country)) %>%
        group_by(source_country) %>%
        summarise(
          count = n(),
          threat_score = mean(threat_score, na.rm = TRUE),
          volume = sum(length, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        filter(count >= input$min_attacks)
      
      return(df)
    })
    
    # World map visualization
    output$world_map <- renderPlotly({
      req(geo_data())
      
      # Get color palette
      palette_name <- switch(input$color_palette,
        "plasma" = "plasma",
        "viridis" = "viridis", 
        "security" = "plasma"
      )
      
      # Create choropleth
      metric_col <- input$metric
      
      p <- plot_ly(
        geo_data(),
        type = "choropleth",
        locations = ~source_country,
        locationmode = "country names",
        z = ~get(metric_col),
        colorscale = palette_name,
        hovertemplate = paste(
          "<b>%{location}</b><br>",
          "Attacks: %{customdata[0]}<br>",
          "Avg Threat: %{customdata[1]:.2f}<br>",
          "Volume: %{customdata[2]}<br>",
          "<extra></extra>"
        ),
        customdata = ~cbind(count, threat_score, volume)
      ) %>%
        layout(
          title = list(
            text = paste("Global Attack Distribution by", 
                        switch(input$metric,
                          "count" = "Attack Count",
                          "threat_score" = "Threat Score", 
                          "volume" = "Data Volume")),
            font = list(color = "#00d4ff")
          ),
          geo = list(
            projection = list(type = "natural earth"),
            showcoastlines = TRUE,
            coastlinecolor = "#2a2f4a",
            showland = TRUE,
            landcolor = "#1a1f3a",
            showocean = TRUE,
            oceancolor = "#0a0e27"
          ),
          paper_bgcolor = "#0a0e27",
          plot_bgcolor = "#0a0e27",
          font = list(color = "#e0e0e0")
        )
      
      # Apply enhanced theme
      p <- apply_viridis_plotly_theme(p, palette_context = "geographic")
      
      return(p)
    })
    
    # Country statistics table
    output$country_table <- DT::renderDataTable({
      req(geo_data())
      
      geo_data() %>%
        arrange(desc(get(input$metric))) %>%
        mutate(
          threat_score = round(threat_score, 2),
          volume = scales::comma(volume)
        ) %>%
        rename(
          Country = source_country,
          Attacks = count,
          `Avg Threat` = threat_score,
          `Data Volume` = volume
        )
    }, 
    options = list(
      pageLength = 15,
      dom = 'frtip',
      scrollY = "400px",
      scrollCollapse = TRUE
    ),
    style = "bootstrap",
    class = "table-bordered table-striped"
    )
    
    # Value boxes
    output$total_countries <- renderValueBox({
      req(geo_data())
      valueBox(
        value = nrow(geo_data()),
        subtitle = "Countries with Attacks",
        icon = icon("globe"),
        color = "blue"
      )
    })
    
    output$top_threat_country <- renderValueBox({
      req(geo_data())
      top_country <- geo_data() %>%
        arrange(desc(threat_score)) %>%
        slice(1) %>%
        pull(source_country)
      
      valueBox(
        value = top_country,
        subtitle = "Highest Threat Country",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    })
    
    output$total_attacks <- renderValueBox({
      req(geo_data())
      total <- sum(geo_data()$count)
      valueBox(
        value = scales::comma(total),
        subtitle = "Total Attacks",
        icon = icon("crosshairs"),
        color = "orange"
      )
    })
    
    output$avg_threat <- renderValueBox({
      req(geo_data())
      avg_threat <- mean(geo_data()$threat_score, na.rm = TRUE)
      valueBox(
        value = round(avg_threat, 2),
        subtitle = "Average Threat Score",
        icon = icon("shield-alt"),
        color = if(avg_threat > 5) "red" else "green"
      )
    })
    
    
  })
}