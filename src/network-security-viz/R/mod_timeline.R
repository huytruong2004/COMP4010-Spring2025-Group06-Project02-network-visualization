# Timeline Module - Simplified
# Network Security Visualization Dashboard
# Removed: 3D surface plots, comparison views, complex aggregations

# UI function
timelineUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Control panel
      column(3,
        wellPanel(
          h4("Timeline Controls", style = "color: #00d4ff;"),
          
          # Date range selector
          dateRangeInput(
            ns("date_range"),
            "Date Range",
            start = Sys.Date() - 7,
            end = Sys.Date(),
            max = Sys.Date()
          ),
          
          # Time interval
          selectInput(
            ns("time_interval"),
            "Aggregation",
            choices = c("Hour" = "hour", "Day" = "day"),
            selected = "hour"
          ),
          
          # Metric selector
          selectInput(
            ns("metric"),
            "Metric",
            choices = c(
              "Attack Count" = "packet_count",
              "Data Volume" = "total_bytes",
              "Unique IPs" = "unique_ips",
              "Threat Score" = "avg_threat_score"
            ),
            selected = "packet_count"
          ),
          
          # Protocol filter
          selectInput(
            ns("protocol_filter"),
            "Protocol",
            choices = c("All" = "all", "TCP" = "TCP", "UDP" = "UDP"),
            selected = "all"
          ),
          
          # Threat level filter
          sliderInput(
            ns("threat_threshold"),
            "Min Threat Score",
            min = 0, max = 10, value = 0, step = 1
          )
        )
      ),
      
      # Main visualization area
      column(9,
        # Time series plot
        box(
          title = "Network Traffic Timeline",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          dygraphOutput(ns("timeline_chart"), height = "400px")
        )
      )
    ),
    
    # Summary statistics
    fluidRow(
      column(3,
        valueBoxOutput(ns("total_packets"), width = NULL)
      ),
      column(3,
        valueBoxOutput(ns("unique_attackers"), width = NULL)
      ),
      column(3,
        valueBoxOutput(ns("avg_threat_level"), width = NULL)
      ),
      column(3,
        valueBoxOutput(ns("time_span"), width = NULL)
      )
    ),
    
    # Detailed data table
    fluidRow(
      column(12,
        box(
          title = "Timeline Data",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          
          DT::dataTableOutput(ns("timeline_table"))
        )
      )
    )
  )
}

# Server function
timelineServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive data filtering
    filtered_data <- reactive({
      req(data())
      
      df <- data()
      
      # Date range filter
      df <- filter_by_date(df, input$date_range[1], input$date_range[2])
      
      # Protocol filter
      if (input$protocol_filter != "all") {
        df <- df[protocol == input$protocol_filter]
      }
      
      # Threat score filter
      df <- df[threat_score >= input$threat_threshold]
      
      return(df)
    })
    
    # Aggregated time series data
    timeline_data <- reactive({
      req(filtered_data())
      aggregate_timeseries(filtered_data(), input$time_interval)
    })
    
    # Timeline chart
    output$timeline_chart <- renderDygraph({
      req(timeline_data())
      
      ts_data <- timeline_data()
      
      # Check if we have data
      if (nrow(ts_data) == 0) {
        return(NULL)
      }
      
      metric_col <- input$metric
      metric_name <- switch(input$metric,
        "packet_count" = "Attack Count",
        "total_bytes" = "Data Volume (bytes)",
        "unique_ips" = "Unique IPs",
        "avg_threat_score" = "Average Threat Score"
      )
      
      # Convert to time series format for dygraph
      ts_matrix <- data.frame(
        datetime = ts_data$datetime,
        value = ts_data[[metric_col]]
      )
      
      # Create dygraph
      dygraph(ts_matrix, main = paste("Timeline:", metric_name)) %>%
        dyOptions(
          fillGraph = TRUE,
          fillAlpha = 0.3,
          drawGrid = TRUE,
          colors = "#00d4ff",
          gridLineColor = "#2a2f4a",
          axisLineColor = "#e0e0e0",
          axisLabelColor = "#a0a0a0"
        ) %>%
        dyRangeSelector(
          fillColor = "#1a1f3a",
          strokeColor = "#00d4ff",
          height = 40
        ) %>%
        dyHighlight(
          highlightCircleSize = 5,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = TRUE
        ) %>%
        dyLegend(show = "follow") %>%
        dyAxis("y", label = metric_name) %>%
        dyAxis("x", label = "Time")
    })
    
    # Value boxes
    output$total_packets <- renderValueBox({
      req(filtered_data())
      total <- nrow(filtered_data())
      valueBox(
        value = scales::comma(total),
        subtitle = "Total Attacks",
        icon = icon("crosshairs"),
        color = "blue"
      )
    })
    
    output$unique_attackers <- renderValueBox({
      req(filtered_data())
      unique_ips <- filtered_data()[, uniqueN(source_ip)]
      valueBox(
        value = scales::comma(unique_ips),
        subtitle = "Unique Attackers",
        icon = icon("users"),
        color = "orange"
      )
    })
    
    output$avg_threat_level <- renderValueBox({
      req(filtered_data())
      avg_threat <- mean(filtered_data()$threat_score, na.rm = TRUE)
      valueBox(
        value = round(avg_threat, 2),
        subtitle = "Avg Threat Score",
        icon = icon("shield-alt"),
        color = if(avg_threat > 5) "red" else "green"
      )
    })
    
    output$time_span <- renderValueBox({
      req(filtered_data())
      time_span <- as.numeric(difftime(max(filtered_data()$datetime), 
                                      min(filtered_data()$datetime), 
                                      units = "hours"))
      valueBox(
        value = paste(round(time_span, 1), "hrs"),
        subtitle = "Time Span",
        icon = icon("clock"),
        color = "purple"
      )
    })
    
    # Data table
    output$timeline_table <- DT::renderDataTable({
      req(timeline_data())
      
      timeline_data() %>%
        mutate(
          datetime = format(datetime, "%Y-%m-%d %H:%M"),
          total_bytes = scales::comma(total_bytes),
          avg_threat_score = round(avg_threat_score, 2)
        ) %>%
        rename(
          DateTime = datetime,
          `Attack Count` = packet_count,
          `Data Volume` = total_bytes,
          `Unique IPs` = unique_ips,
          `Avg Threat` = avg_threat_score
        )
    }, 
    options = list(
      pageLength = 15,
      dom = 'frtip',
      scrollY = "300px",
      scrollCollapse = TRUE,
      order = list(list(0, 'desc'))
    ),
    style = "bootstrap",
    class = "table-bordered table-striped"
    )
    
    
  })
}