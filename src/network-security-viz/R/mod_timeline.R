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
          
          # Time interval
          selectInput(
            ns("time_interval"),
            "Aggregation",
            choices = c("Minute" = "minute", "Hour" = "hour", "Day" = "day"),
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
            choices = c("All" = "all"),
            selected = "all"
          ),
          
          # Threat level filter
          sliderInput(
            ns("threat_threshold"),
            "Min Threat Score",
            min = 0, max = 10, value = 0, step = 1
          ),
          
          # Gap handling option
          checkboxInput(
            ns("fill_gaps"),
            "Fill data gaps with zeros",
            value = TRUE
          ),
          
          # Data quality info
          br(),
          div(
            style = "background-color: #2a2f4a; padding: 10px; border-radius: 5px; border-left: 3px solid #f39c12;",
            h5("Data Quality Legend", style = "color: #f39c12; margin-top: 0;"),
            p(style = "color: #e0e0e0; margin: 5px 0;", 
              tags$span("•", style = "color: #00d4ff;"), " Blue: Actual data points"),
            p(style = "color: #e0e0e0; margin: 5px 0;", 
              tags$span("•", style = "color: #ff6b6b;"), " Red: Filled gaps (no data collected)")
          ),
          
          # Threat score explanation
          br(),
          actionButton(
            ns("threat_info"),
            "How is Threat Score calculated?",
            icon = icon("info-circle"),
            style = "width: 100%; background-color: #2a2f4a; border-color: #00d4ff;"
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
          
          dygraphOutput(ns("timeline_chart"), height = "560px")
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
    
    # Update protocol choices based on data
    observe({
      req(data())
      protocols <- unique(data()$protocol)
      protocols <- sort(protocols[!is.na(protocols)])
      
      updateSelectInput(
        session,
        "protocol_filter",
        choices = c("All" = "all", setNames(protocols, protocols)),
        selected = input$protocol_filter
      )
    })
    
    # Reactive data filtering
    filtered_data <- reactive({
      req(data())
      
      df <- data()
      
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
      aggregate_timeseries(filtered_data(), input$time_interval, fill_gaps = input$fill_gaps)
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
        datetime = ts_data$datetime
      )
      # Add column with dynamic name
      ts_matrix[[metric_name]] <- ts_data[[metric_col]]
      
      # Add data quality indicator if available
      if ("data_available" %in% names(ts_data)) {
        # Create separate series for filled gaps (when data_available = FALSE)
        ts_matrix[[paste(metric_name, "(No Data)")]] <- ifelse(
          ts_data$data_available == FALSE & input$fill_gaps, 
          ts_data[[metric_col]], 
          NA
        )
      }
      
      # Create dygraph
      dygraph(ts_matrix, main = paste("Timeline:", metric_name)) %>%
        dyOptions(
          fillGraph = TRUE,
          fillAlpha = 0.3,
          drawGrid = TRUE,
          colors = if ("data_available" %in% names(ts_data)) c("#00d4ff", "#ff6b6b") else "#00d4ff",
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
        color = if(total == 0) "black" else "blue"
      )
    })
    
    output$unique_attackers <- renderValueBox({
      req(filtered_data())
      if (nrow(filtered_data()) == 0) {
        unique_ips <- 0
      } else {
        unique_ips <- filtered_data()[, uniqueN(source_ip)]
      }
      valueBox(
        value = scales::comma(unique_ips),
        subtitle = "Unique Attackers",
        icon = icon("users"),
        color = if(unique_ips == 0) "black" else "orange"
      )
    })
    
    output$avg_threat_level <- renderValueBox({
      req(filtered_data())
      if (nrow(filtered_data()) == 0) {
        valueBox(
          value = "N/A",
          subtitle = "Avg Threat Score",
          icon = icon("shield-alt"),
          color = "black"
        )
      } else {
        avg_threat <- mean(filtered_data()$threat_score, na.rm = TRUE)
        valueBox(
          value = round(avg_threat, 2),
          subtitle = "Avg Threat Score",
          icon = icon("shield-alt"),
          color = if(is.finite(avg_threat) && avg_threat > 5) "red" else "green"
        )
      }
    })
    
    output$time_span <- renderValueBox({
      req(filtered_data())
      if (nrow(filtered_data()) == 0) {
        valueBox(
          value = "N/A",
          subtitle = "Time Span",
          icon = icon("clock"),
          color = "black"
        )
      } else {
        time_span <- as.numeric(difftime(max(filtered_data()$datetime), 
                                        min(filtered_data()$datetime), 
                                        units = "hours"))
        valueBox(
          value = paste(round(time_span, 1), "hrs"),
          subtitle = "Time Span",
          icon = icon("clock"),
          color = "purple"
        )
      }
    })
    
    # Data table
    output$timeline_table <- DT::renderDataTable({
      req(timeline_data())
      
      table_data <- timeline_data()
      
      # Format data for display
      if ("data_available" %in% names(table_data)) {
        table_data %>%
          mutate(
            datetime = format(datetime, "%Y-%m-%d %H:%M"),
            total_bytes = scales::comma(total_bytes),
            avg_threat_score = round(avg_threat_score, 2),
            data_status = ifelse(data_available, "✓ Data", "⚠ Filled")
          ) %>%
          rename(
            DateTime = datetime,
            `Attack Count` = packet_count,
            `Data Volume` = total_bytes,
            `Unique IPs` = unique_ips,
            `Avg Threat` = avg_threat_score,
            `Status` = data_status
          )
      } else {
        table_data %>%
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
      }
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
    
    # Threat score explanation modal
    observeEvent(input$threat_info, {
      showModal(modalDialog(
        title = div("Threat Score Calculation", style = "color: #00d4ff;"),
        size = "l",
        tags$style("
          .modal-content {
            background-color: #1a1f3a !important;
            border: 1px solid #00d4ff;
          }
          .modal-header {
            background-color: #0a0e27 !important;
            border-bottom: 1px solid #2a2f4a;
          }
          .modal-body {
            background-color: #1a1f3a !important;
            color: #e0e0e0 !important;
            max-height: 70vh !important;
            overflow-y: auto !important;
          }
          .modal-footer {
            background-color: #0a0e27 !important;
            border-top: 1px solid #2a2f4a;
          }
          .close {
            color: #00d4ff !important;
            opacity: 0.8;
          }
          .close:hover {
            opacity: 1;
          }
          .btn-default {
            background-color: #2a2f4a !important;
            color: #00d4ff !important;
            border: 1px solid #00d4ff !important;
          }
          .btn-default:hover {
            background-color: #00d4ff !important;
            color: #0a0e27 !important;
          }
        "),
        div(style = "color: #e0e0e0;",
          p("Threat scores range from 0 (low) to 10 (high) based on:"),
          tags$ul(style = "color: #e0e0e0;",
            tags$li(tags$span("High-Risk Ports (+3 points):", style = "color: #00d4ff; font-weight: bold;"), 
                   " SSH (22), Telnet (23), SMB (445), RDP (3389), SQL databases (1433, 3306, 5432)"),
            tags$li(tags$span("Unusual Packet Sizes:", style = "color: #00d4ff; font-weight: bold;"), 
                   " Large packets >10KB (+2), Very small packets <20 bytes (+1)"),
            tags$li(tags$span("Protocol & Port:", style = "color: #00d4ff; font-weight: bold;"), 
                   " TCP on privileged ports <1024 (+1)")
          ),
          br(),
          p("Examples:", style = "color: #00d4ff; font-weight: bold;"),
          tags$ul(style = "color: #e0e0e0;",
            tags$li("Attack on SSH port (22) with large packet: 3 + 2 = ", 
                   tags$span("5 points", style="color: #f39c12; font-weight: bold;"))
          ),
          br(),
          p("How Threat Scores are Used:", style = "color: #00d4ff; font-weight: bold;"),
          tags$ul(style = "color: #e0e0e0;",
            tags$li(tags$span("IP Threat Scores:", style = "color: #00d4ff;"), 
                   " Show the average threat level of all attacks from that IP address (e.g., 'This attacker typically targets high-risk services')"),
            tags$li(tags$span("Port Threat Scores:", style = "color: #00d4ff;"), 
                   " Show the average threat level of all attacks on that port (e.g., 'Attacks on this port tend to be more sophisticated')")
          ),
          br()
        ),
        footer = tagList(
          modalButton(
            "Close",
            icon = NULL
          )
        )
      ))
    })
    
  })
}