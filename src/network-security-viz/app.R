# Network Security Visualization Dashboard - MVP
# Main Shiny Application - Simplified

# Source global settings
source("global.R")

# Define UI
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = tags$span(
      tags$img(src = "data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMjQiIGhlaWdodD0iMjQiIHZpZXdCb3g9IjAgMCAyNCAyNCIgZmlsbD0ibm9uZSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj4KPHBhdGggZD0iTTEyIDJMMiA3VjEyQzIgMTYuNSA0LjggMjAuNyAxMiAyMkMxOS4yIDIwLjcgMjIgMTYuNSAyMiAxMlY3TDEyIDJaIiBzdHJva2U9IiMwMGQ0ZmYiIHN0cm9rZS13aWR0aD0iMiIgZmlsbD0ibm9uZSIvPgo8Y2lyY2xlIGN4PSIxMiIgY3k9IjEyIiByPSIzIiBmaWxsPSIjMDBkNGZmIi8+CjwvcMSdPg==",
             height = "30px", style = "margin-right: 10px;"),
      "VPS Security Monitor"
    ),
    titleWidth = 300
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar",
      
      menuItem(
        "Dashboard Overview",
        tabName = "overview",
        icon = icon("tachometer-alt")
      ),
      
      menuItem(
        "Timeline Analysis",
        tabName = "timeline",
        icon = icon("chart-line")
      ),
      
      menuItem(
        "Network Graph",
        tabName = "network",
        icon = icon("project-diagram")
      ),
      
      menuItem(
        "Geographic Analysis",
        tabName = "geographic",
        icon = icon("globe")
      ),
      
      hr(),
      
      # Global filters
      tags$div(
        style = "padding: 10px;",
        h5("Global Filters", style = "color: #00d4ff;"),
        
        selectInput(
          "global_time_range",
          "Time Range",
          choices = c(
            "Last 24 Hours" = "1d",
            "Last 7 Days" = "7d",
            "Last 30 Days" = "30d",
            "All Time" = "all"
          ),
          selected = "7d"
        )
      )
    )
  ),
  
  # Body
  dashboardBody(
    # Include custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;700&display=swap"
      )
    ),
    
    # Tab content
    tabItems(
      # Overview tab
      tabItem(
        tabName = "overview",
        h2("Network Security Dashboard", style = "color: #00d4ff;"),
        
        # Summary boxes
        fluidRow(
          valueBoxOutput("overview_total_attacks"),
          valueBoxOutput("overview_unique_ips"),
          valueBoxOutput("overview_threat_level")
        ),
        
        fluidRow(
          valueBoxOutput("overview_top_country"),
          valueBoxOutput("overview_top_port"),
          valueBoxOutput("overview_data_volume")
        ),
        
        # Quick charts
        fluidRow(
          box(
            title = "Recent Attack Trends",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotOutput("overview_trend_chart", height = "300px")
          ),
          
          box(
            title = "Top Attackers",
            status = "danger",
            solidHeader = TRUE,
            width = 4,
            DT::dataTableOutput("overview_top_attackers")
          )
        ),
        
        # System status (simplified)
        fluidRow(
          box(
            title = "Data Summary",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(3,
                h4("Data Status:", style = "color: #00d4ff;"),
                textOutput("data_status")
              ),
              column(3,
                h4("Total Records:", style = "color: #00d4ff;"),
                textOutput("total_records")
              ),
              column(3,
                h4("Date Range:", style = "color: #00d4ff;"),
                textOutput("date_range")
              ),
              column(3,
                h4("Last Update:", style = "color: #00d4ff;"),
                textOutput("last_update")
              )
            )
          )
        )
      ),
      
      # Timeline tab
      tabItem(
        tabName = "timeline",
        timelineUI("timeline")
      ),
      
      # Network tab
      tabItem(
        tabName = "network",
        networkUI("network")
      ),
      
      # Geographic tab
      tabItem(
        tabName = "geographic",
        geographicUI("geographic")
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    data = NULL,
    last_updated = NULL
  )
  
  # Load initial data
  observe({
    tryCatch({
      # Load default data
      values$data <- load_network_data("../parser/output.csv")
      values$last_updated <- Sys.time()
      
      showNotification(
        paste("Loaded", scales::comma(nrow(values$data)), "attack records"),
        type = "message",
        duration = 3
      )
    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message),
        type = "error",
        duration = 10
      )
      # Create sample data for demo
      values$data <- create_sample_data()
      values$last_updated <- Sys.time()
    })
  })
  
  
  # Filter data based on global time range
  filtered_data <- reactive({
    req(values$data)
    
    df <- values$data
    
    # Apply global time filter
    if (input$global_time_range != "all") {
      days_back <- switch(input$global_time_range,
        "1d" = 1,
        "7d" = 7,
        "30d" = 30
      )
      cutoff_date <- Sys.time() - as.difftime(days_back, units = "days")
      df <- df[datetime >= cutoff_date]
    }
    
    return(df)
  })
  
  # Overview tab outputs
  output$overview_total_attacks <- renderValueBox({
    req(filtered_data())
    valueBox(
      value = scales::comma(nrow(filtered_data())),
      subtitle = "Total Attacks",
      icon = icon("crosshairs"),
      color = "red"
    )
  })
  
  output$overview_unique_ips <- renderValueBox({
    req(filtered_data())
    valueBox(
      value = scales::comma(filtered_data()[, uniqueN(source_ip)]),
      subtitle = "Unique Attackers",
      icon = icon("users"),
      color = "orange"
    )
  })
  
  output$overview_threat_level <- renderValueBox({
    req(filtered_data())
    avg_threat <- mean(filtered_data()$threat_score, na.rm = TRUE)
    valueBox(
      value = round(avg_threat, 1),
      subtitle = "Avg Threat Score",
      icon = icon("shield-alt"),
      color = if(avg_threat > 5) "red" else "green"
    )
  })
  
  output$overview_top_country <- renderValueBox({
    req(filtered_data())
    top_country <- filtered_data()[, .N, by = source_country][order(-N)][1, source_country]
    valueBox(
      value = top_country,
      subtitle = "Top Attack Origin",
      icon = icon("flag"),
      color = "blue"
    )
  })
  
  output$overview_top_port <- renderValueBox({
    req(filtered_data())
    top_port <- filtered_data()[, .N, by = destination_port][order(-N)][1, destination_port]
    valueBox(
      value = top_port,
      subtitle = "Most Targeted Port",
      icon = icon("bullseye"),
      color = "purple"
    )
  })
  
  output$overview_data_volume <- renderValueBox({
    req(filtered_data())
    total_bytes <- sum(filtered_data()$length, na.rm = TRUE)
    valueBox(
      value = scales::comma(total_bytes),
      subtitle = "Total Data Volume (bytes)",
      icon = icon("hdd"),
      color = "green"
    )
  })
  
  # Overview trend chart
  output$overview_trend_chart <- renderPlot({
    req(filtered_data())
    
    trend_data <- aggregate_timeseries(filtered_data(), "hour")
    
    ggplot(trend_data, aes(x = datetime, y = packet_count)) +
      geom_line(color = "#00d4ff", linewidth = 1.2) +
      geom_area(fill = "#00d4ff", alpha = 0.3) +
      labs(
        title = "Attack Frequency Over Time",
        x = "Time",
        y = "Attack Count"
      ) +
      theme_security_enhanced(palette_context = "temporal")
  })
  
  # Top attackers table
  output$overview_top_attackers <- DT::renderDataTable({
    req(filtered_data())
    
    filtered_data()[, .(
      Attacks = .N,
      `Avg Threat` = round(mean(threat_score), 2),
      Country = first(source_country)
    ), by = .(IP = source_ip)][order(-Attacks)][1:10]
  }, 
  options = list(pageLength = 10, dom = 't'),
  style = "bootstrap",
  class = "table-striped"
  )
  
  # Status outputs
  output$data_status <- renderText({
    if (is.null(values$data)) "No Data" else "Data Loaded"
  })
  
  output$total_records <- renderText({
    if (is.null(values$data)) "0" else scales::comma(nrow(values$data))
  })
  
  output$date_range <- renderText({
    if (is.null(values$data)) {
      "N/A"
    } else {
      paste(
        format(min(values$data$datetime), "%Y-%m-%d"),
        "to",
        format(max(values$data$datetime), "%Y-%m-%d")
      )
    }
  })
  
  output$last_update <- renderText({
    if (is.null(values$last_updated)) "Never" else format(values$last_updated, "%Y-%m-%d %H:%M:%S")
  })
  
  # Module servers
  timelineServer("timeline", filtered_data)
  networkServer("network", filtered_data)
  geographicServer("geographic", filtered_data)
}

# Create sample data function for demo
create_sample_data <- function() {
  set.seed(42)
  n <- 1000
  
  dt <- data.table(
    timestamp = as.integer(seq(
      from = as.POSIXct("2024-01-01"),
      to = Sys.time(),
      length.out = n
    )),
    source_ip = sample(c("192.168.1.1", "10.0.0.1", "172.16.0.1", "203.0.113.1", "198.51.100.1"), n, replace = TRUE),
    source_country = sample(c("United States", "China", "Russia", "Germany", "Brazil"), n, replace = TRUE),
    destination_port = sample(c(22, 80, 443, 3389, 1433, 3306), n, replace = TRUE),
    protocol = sample(c("TCP", "UDP"), n, replace = TRUE, prob = c(0.8, 0.2)),
    length = sample(50:2000, n, replace = TRUE)
  )
  
  dt[, datetime := as.POSIXct(timestamp, origin = "1970-01-01")]
  dt[, hour := as.POSIXlt(datetime)$hour]  
  dt[, date := as.Date(datetime)]
  dt[, threat_score := calculate_threat_score(destination_port, protocol, length)]
  
  return(dt)
}

# Run the application
shinyApp(ui = ui, server = server)