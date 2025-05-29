# Network Module - Simplified
# Network Security Visualization Dashboard  
# Removed: complex layouts, advanced physics, complex clustering

# UI function
networkUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Control panel
      column(3,
        wellPanel(
          h4("Network Controls", style = "color: #00d4ff;"),
          
          # Node filtering
          sliderInput(
            ns("min_connections"),
            "Min Connections",
            min = 1, max = 50, value = 5, step = 1
          ),
          
          sliderInput(
            ns("max_nodes"),
            "Max Nodes",
            min = 50, max = 200, value = 100, step = 10
          ),
          
          # Simple clustering
          selectInput(
            ns("cluster_by"),
            "Group By",
            choices = c(
              "None" = "none",
              "Country" = "country",
              "Threat Level" = "threat"
            ),
            selected = "country"
          ),
          
          # Display options
          checkboxInput(
            ns("show_labels"),
            "Show Labels",
            value = TRUE
          ),
          
          # Physics controls (simplified)
          hr(),
          h5("Physics", style = "color: #00d4ff;"),
          checkboxInput(
            ns("physics_enabled"),
            "Enable Physics",
            value = TRUE
          ),
          
          conditionalPanel(
            condition = paste0("input['", ns("physics_enabled"), "']"),
            sliderInput(
              ns("repulsion"),
              "Repulsion",
              min = -500, max = -50, value = -150, step = 10
            )
          )
        )
      ),
      
      # Main network visualization
      column(9,
        box(
          title = "Attack Network",
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          
          visNetworkOutput(ns("network_plot"), height = "500px")
        )
      )
    ),
    
    # Summary statistics
    fluidRow(
      column(3,
        valueBoxOutput(ns("total_nodes"), width = NULL)
      ),
      column(3,
        valueBoxOutput(ns("total_edges"), width = NULL)
      ),
      column(3,
        valueBoxOutput(ns("top_attacker"), width = NULL)
      ),
      column(3,
        valueBoxOutput(ns("network_density"), width = NULL)
      )
    ),
    
    # Node details (simplified)
    fluidRow(
      column(12,
        box(
          title = "Selected Node Details",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          
          uiOutput(ns("node_details"))
        )
      )
    )
  )
}

# Server function
networkServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values
    values <- reactiveValues(
      selected_node = NULL,
      network_data = NULL
    )
    
    # Process network data
    network_data <- reactive({
      req(data())
      
      # Filter by minimum connections and limit nodes
      filtered_data <- data()[
        source_ip %in% data()[, .N, by = source_ip][N >= input$min_connections, source_ip]
      ]
      
      # Create network structure with node limit
      net_data <- create_network_data(filtered_data, max_nodes = input$max_nodes)
      
      # Apply simple clustering
      if (input$cluster_by != "none") {
        net_data$nodes <- apply_simple_clustering(net_data$nodes, filtered_data, input$cluster_by)
      }
      
      return(net_data)
    })
    
    # Network visualization
    output$network_plot <- renderVisNetwork({
      req(network_data())
      
      nodes <- network_data()$nodes
      edges <- network_data()$edges
      
      # Create network
      vis <- visNetwork(nodes, edges) %>%
        visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1),
          selectedBy = "group",
          nodesIdSelection = TRUE
        ) %>%
        visInteraction(
          navigationButtons = TRUE,
          hover = TRUE,
          zoomView = TRUE,
          dragView = TRUE
        ) %>%
        visEvents(
          select = sprintf("function(params) {
            Shiny.setInputValue('%s', params.nodes[0]);
          }", session$ns("selected_node"))
        )
      
      # Configure physics
      if (input$physics_enabled) {
        vis <- vis %>%
          visPhysics(
            enabled = TRUE,
            barnesHut = list(
              gravitationalConstant = input$repulsion,
              springLength = 200,
              damping = 0.09
            ),
            stabilization = list(iterations = 50)
          )
      } else {
        vis <- vis %>% visPhysics(enabled = FALSE)
      }
      
      # Configure labels
      if (!input$show_labels) {
        vis <- vis %>% visNodes(font = list(size = 0))
      }
      
      # Apply theme
      vis %>%
        visNodes(
          shadow = list(enabled = TRUE, size = 8),
          borderWidth = 2,
          borderWidthSelected = 4
        ) %>%
        visEdges(
          smooth = list(type = "dynamic"),
          shadow = FALSE,
          color = list(color = "#95a5a6", opacity = 0.7)
        )
    })
    
    # Value boxes
    output$total_nodes <- renderValueBox({
      req(network_data())
      valueBox(
        value = nrow(network_data()$nodes),
        subtitle = "Network Nodes",
        icon = icon("circle"),
        color = "blue"
      )
    })
    
    output$total_edges <- renderValueBox({
      req(network_data())
      valueBox(
        value = nrow(network_data()$edges),
        subtitle = "Connections",
        icon = icon("link"),
        color = "green"
      )
    })
    
    output$top_attacker <- renderValueBox({
      req(data())
      top_ip <- data()[, .N, by = source_ip][order(-N)][1, source_ip]
      valueBox(
        value = substr(top_ip, 1, 12),
        subtitle = "Top Attacker IP",
        icon = icon("crosshairs"),
        color = "red"
      )
    })
    
    output$network_density <- renderValueBox({
      req(network_data())
      nodes_count <- nrow(network_data()$nodes)
      edges_count <- nrow(network_data()$edges)
      max_edges <- nodes_count * (nodes_count - 1)
      density <- round((edges_count / max_edges) * 100, 1)
      
      valueBox(
        value = paste0(density, "%"),
        subtitle = "Network Density",
        icon = icon("project-diagram"),
        color = "purple"
      )
    })
    
    # Node details
    output$node_details <- renderUI({
      if (is.null(input$selected_node)) {
        return(p("Click on a node to see details", style = "color: #a0a0a0;"))
      }
      
      # Get node information
      node_info <- network_data()$nodes[network_data()$nodes$id == input$selected_node, ]
      
      if (nrow(node_info) == 0) {
        return(p("Node information not available", style = "color: #a0a0a0;"))
      }
      
      tagList(
        h5(paste("Node:", node_info$label), style = "color: #00d4ff;"),
        p(paste("Group:", node_info$group)),
        p(paste("Value:", node_info$value)),
        
        # Show related traffic if it's an IP node
        if (startsWith(node_info$label, "1") || startsWith(node_info$label, "2")) {
          req(data())
          related_traffic <- data()[source_ip == node_info$label]
          
          if (nrow(related_traffic) > 0) {
            tagList(
              h6("Recent Activity:", style = "color: #00d4ff;"),
              p(paste("Total Attacks:", nrow(related_traffic))),
              p(paste("Countries:", paste(unique(related_traffic$source_country), collapse = ", "))),
              p(paste("Avg Threat Score:", round(mean(related_traffic$threat_score), 2)))
            )
          }
        }
      )
    })
    
    
  })
}

# Helper function for simple clustering
apply_simple_clustering <- function(nodes, data, cluster_by) {
  if (cluster_by == "country") {
    # Group nodes by country for IP nodes
    for (i in 1:nrow(nodes)) {
      if (startsWith(nodes$label[i], "1") || startsWith(nodes$label[i], "2")) {
        ip_country <- data[source_ip == nodes$label[i], unique(source_country)][1]
        if (!is.na(ip_country)) {
          nodes$group[i] <- ip_country
          # Assign colors based on country
          nodes$color[i] <- switch(substr(ip_country, 1, 2),
            "US" = "#3498db",
            "CN" = "#e74c3c", 
            "RU" = "#f39c12",
            "DE" = "#27ae60",
            "#9b59b6"
          )
        }
      }
    }
  } else if (cluster_by == "threat") {
    # Group nodes by threat level for IP nodes
    for (i in 1:nrow(nodes)) {
      if (startsWith(nodes$label[i], "1") || startsWith(nodes$label[i], "2")) {
        ip_threat <- data[source_ip == nodes$label[i], mean(threat_score)]
        if (!is.na(ip_threat)) {
          if (ip_threat > 7) {
            nodes$group[i] <- "high_threat"
            nodes$color[i] <- "#e74c3c"
          } else if (ip_threat > 4) {
            nodes$group[i] <- "medium_threat"
            nodes$color[i] <- "#f39c12"
          } else {
            nodes$group[i] <- "low_threat"
            nodes$color[i] <- "#27ae60"
          }
        }
      }
    }
  }
  
  return(nodes)
}