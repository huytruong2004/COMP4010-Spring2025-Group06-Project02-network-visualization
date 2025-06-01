# Network Module - Simplified
# Network Security Visualization Dashboard

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
          selectInput(
            ns("port_selection"),
            "Show Ports",
            choices = c(
              "Top 5 ports" = 5,
              "Top 10 ports" = 10,
              "Top 20 ports" = 20
            ),
            selected = 10
          ),
          
          hr(),
          
          # Smart search
          h5("Smart Search", style = "color: #00d4ff;"),
          textInput(
            ns("node_search"),
            NULL,
            placeholder = "Search IP, Port, or Country..."
          ),
          
          # Filter chips
          h6("Node Type", style = "color: #00d4ff; margin-top: 15px;"),
          uiOutput(ns("node_type_filters")),
          
          h6("Threat Level", style = "color: #00d4ff; margin-top: 15px;"),
          uiOutput(ns("threat_level_filters")),
          
          h6("Countries", style = "color: #00d4ff; margin-top: 15px;"),
          selectizeInput(
            ns("country_filter"),
            NULL,
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            width = "100%",
            options = list(
              placeholder = "Select countries to filter...",
              maxOptions = 1000
            )
          ),
          
          br(),
          actionButton(
            ns("clear_filters"),
            "Clear All Filters",
            class = "btn-sm btn-warning",
            style = "width: 100%;"
          ),
          
          # Display options
          checkboxInput(
            ns("show_labels"),
            "Show Labels",
            value = TRUE
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
    
    # Node details
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
      network_data = NULL,
      search_term = NULL,
      filters = list(
        node_types = c("ip", "port"),  # Both active by default
        threat_levels = c("high", "medium", "low")  # All active by default
      )
    )
    
    # Process network data
    network_data <- reactive({
      req(data())
      
      # Create network structure showing selected number of top ports
      net_data <- create_network_data(data(), max_nodes = NULL, top_ports_count = as.integer(input$port_selection))
      
      # Apply filters
      if (!is.null(net_data$nodes) && nrow(net_data$nodes) > 0) {
        # Filter by node type
        type_filter <- rep(FALSE, nrow(net_data$nodes))
        if ("ip" %in% values$filters$node_types) {
          type_filter <- type_filter | net_data$nodes$group == "ip"
        }
        if ("port" %in% values$filters$node_types) {
          type_filter <- type_filter | net_data$nodes$group == "port"
        }
        
        # Filter by threat level (only for IP nodes)
        threat_filter <- rep(TRUE, nrow(net_data$nodes))
        for (i in 1:nrow(net_data$nodes)) {
          if (net_data$nodes$group[i] == "ip") {
            ip_threat <- data()[source_ip == net_data$nodes$label[i], mean(threat_score)]
            threat_level <- if (ip_threat > 7) "high" else if (ip_threat > 4) "medium" else "low"
            threat_filter[i] <- threat_level %in% values$filters$threat_levels
          }
        }
        
        # Filter by country (only for IP nodes)
        country_filter <- rep(TRUE, nrow(net_data$nodes))
        if (!is.null(input$country_filter) && length(input$country_filter) > 0) {
          for (i in 1:nrow(net_data$nodes)) {
            if (net_data$nodes$group[i] == "ip") {
              ip_country <- data()[source_ip == net_data$nodes$label[i], unique(source_country)][1]
              country_filter[i] <- ip_country %in% input$country_filter
            }
          }
        }
        
        # Apply all filters
        keep_nodes <- type_filter & threat_filter & country_filter
        filtered_node_ids <- net_data$nodes$id[keep_nodes]
        
        # Filter nodes
        net_data$nodes <- net_data$nodes[keep_nodes, ]
        
        # Filter edges to only include those between visible nodes
        net_data$edges <- net_data$edges[
          net_data$edges$from %in% filtered_node_ids & 
          net_data$edges$to %in% filtered_node_ids, 
        ]
      }
      
      # Apply coloring based on threat level
      net_data$nodes <- apply_simple_clustering(net_data$nodes, data(), "threat")
      
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
          highlightNearest = list(enabled = TRUE, degree = 1)
        ) %>%
        visInteraction(
          navigationButtons = FALSE,
          hover = TRUE,
          zoomView = TRUE,
          dragView = TRUE
        ) %>%
        visEvents(
          select = sprintf("function(params) {
            Shiny.setInputValue('%s', params.nodes[0]);
          }", session$ns("selected_node"))
        )
      
      # Configure physics with fixed repulsion
      vis <- vis %>%
        visPhysics(
          enabled = TRUE,
          barnesHut = list(
            gravitationalConstant = -500,
            springLength = 200,
            damping = 0.09
          ),
          stabilization = list(iterations = 50)
        )
      
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
    
    # Generate node type filter buttons
    output$node_type_filters <- renderUI({
      div(
        style = "display: flex; gap: 5px; flex-wrap: wrap;",
        actionButton(
          session$ns("filter_ip"), "IP",
          class = paste("btn-xs", if("ip" %in% values$filters$node_types) "active" else ""),
          style = paste0(
            "background-color: ", if("ip" %in% values$filters$node_types) "#c0392b" else "#e74c3c", "; ",
            "color: white; border: ", if("ip" %in% values$filters$node_types) "2px solid #fff" else "none", "; ",
            "opacity: ", if("ip" %in% values$filters$node_types) "1" else "0.6", ";"
          )
        ),
        actionButton(
          session$ns("filter_port"), "Port",
          class = paste("btn-xs", if("port" %in% values$filters$node_types) "active" else ""),
          style = paste0(
            "background-color: ", if("port" %in% values$filters$node_types) "#2980b9" else "#3498db", "; ",
            "color: white; border: ", if("port" %in% values$filters$node_types) "2px solid #fff" else "none", "; ",
            "opacity: ", if("port" %in% values$filters$node_types) "1" else "0.6", ";"
          )
        )
      )
    })
    
    # Generate threat level filter buttons
    output$threat_level_filters <- renderUI({
      div(
        style = "display: flex; gap: 5px; flex-wrap: wrap;",
        actionButton(
          session$ns("filter_high"), "High",
          class = paste("btn-xs", if("high" %in% values$filters$threat_levels) "active" else ""),
          style = paste0(
            "background-color: ", if("high" %in% values$filters$threat_levels) "#c0392b" else "#e74c3c", "; ",
            "color: white; border: ", if("high" %in% values$filters$threat_levels) "2px solid #fff" else "none", "; ",
            "opacity: ", if("high" %in% values$filters$threat_levels) "1" else "0.6", ";"
          )
        ),
        actionButton(
          session$ns("filter_medium"), "Medium",
          class = paste("btn-xs", if("medium" %in% values$filters$threat_levels) "active" else ""),
          style = paste0(
            "background-color: ", if("medium" %in% values$filters$threat_levels) "#d68910" else "#f39c12", "; ",
            "color: white; border: ", if("medium" %in% values$filters$threat_levels) "2px solid #fff" else "none", "; ",
            "opacity: ", if("medium" %in% values$filters$threat_levels) "1" else "0.6", ";"
          )
        ),
        actionButton(
          session$ns("filter_low"), "Low",
          class = paste("btn-xs", if("low" %in% values$filters$threat_levels) "active" else ""),
          style = paste0(
            "background-color: ", if("low" %in% values$filters$threat_levels) "#1e8449" else "#27ae60", "; ",
            "color: white; border: ", if("low" %in% values$filters$threat_levels) "2px solid #fff" else "none", "; ",
            "opacity: ", if("low" %in% values$filters$threat_levels) "1" else "0.6", ";"
          )
        )
      )
    })
    
    # Populate country dropdown
    observe({
      req(data())
      
      # Get all countries sorted by attack count
      all_countries <- data()[, .(count = .N), by = source_country][order(-count)]
      
      # Handle empty data case
      if (nrow(all_countries) == 0) {
        country_choices <- character(0)
      } else {
        # Create choices with attack counts
        country_choices <- setNames(
          all_countries$source_country,
          paste0(all_countries$source_country, " (", scales::comma(all_countries$count), ")")
        )
      }
      
      updateSelectizeInput(
        session,
        "country_filter",
        choices = country_choices,
        selected = character(0),
        server = TRUE
      )
    })
    
    # Node type filters
    observeEvent(input$filter_ip, {
      if ("ip" %in% values$filters$node_types) {
        values$filters$node_types <- setdiff(values$filters$node_types, "ip")
      } else {
        values$filters$node_types <- c(values$filters$node_types, "ip")
      }
    })
    
    observeEvent(input$filter_port, {
      if ("port" %in% values$filters$node_types) {
        values$filters$node_types <- setdiff(values$filters$node_types, "port")
      } else {
        values$filters$node_types <- c(values$filters$node_types, "port")
      }
    })
    
    # Threat level filters
    observeEvent(input$filter_high, {
      if ("high" %in% values$filters$threat_levels) {
        values$filters$threat_levels <- setdiff(values$filters$threat_levels, "high")
      } else {
        values$filters$threat_levels <- c(values$filters$threat_levels, "high")
      }
    })
    
    observeEvent(input$filter_medium, {
      if ("medium" %in% values$filters$threat_levels) {
        values$filters$threat_levels <- setdiff(values$filters$threat_levels, "medium")
      } else {
        values$filters$threat_levels <- c(values$filters$threat_levels, "medium")
      }
    })
    
    observeEvent(input$filter_low, {
      if ("low" %in% values$filters$threat_levels) {
        values$filters$threat_levels <- setdiff(values$filters$threat_levels, "low")
      } else {
        values$filters$threat_levels <- c(values$filters$threat_levels, "low")
      }
    })
    
    # Clear all filters
    observeEvent(input$clear_filters, {
      values$filters$node_types <- c("ip", "port")
      values$filters$threat_levels <- c("high", "medium", "low")
      updateSelectizeInput(session, "country_filter", selected = character(0))
      updateTextInput(session, "node_search", value = "")
    })
    
    # Enhanced search functionality
    observeEvent(input$node_search, {
      if (nzchar(input$node_search)) {
        search_term <- trimws(input$node_search)
        
        # Get network data
        net_data <- network_data()
        if (!is.null(net_data)) {
          # Search in labels and apply as additional filter
          matching_nodes <- net_data$nodes[
            grepl(search_term, net_data$nodes$label, ignore.case = TRUE) |
            grepl(search_term, net_data$nodes$group, ignore.case = TRUE), 
            "id"
          ]
          
          if (length(matching_nodes) > 0) {
            # Select and focus on the first matching node
            visNetworkProxy(session$ns("network_plot")) %>%
              visSelectNodes(id = matching_nodes[1]) %>%
              visFocus(id = matching_nodes[1], scale = 1)
          }
        }
      }
    })
    
    # Value boxes
    output$total_nodes <- renderValueBox({
      net_data <- network_data()
      if (is.null(net_data) || nrow(net_data$nodes) == 0) {
        node_count <- 0
      } else {
        node_count <- nrow(net_data$nodes)
      }
      valueBox(
        value = node_count,
        subtitle = "Network Nodes",
        icon = icon("circle"),
        color = if(node_count == 0) "black" else "blue"
      )
    })
    
    output$total_edges <- renderValueBox({
      net_data <- network_data()
      if (is.null(net_data) || nrow(net_data$edges) == 0) {
        edge_count <- 0
      } else {
        edge_count <- nrow(net_data$edges)
      }
      valueBox(
        value = edge_count,
        subtitle = "Connections",
        icon = icon("link"),
        color = if(edge_count == 0) "black" else "green"
      )
    })
    
    output$top_attacker <- renderValueBox({
      req(data())
      top_ip <- data()[, .N, by = source_ip][order(-N)][1, source_ip]
      valueBox(
        value = top_ip,
        subtitle = "Top Attacker IP",
        icon = icon("crosshairs"),
        color = "red"
      )
    })
    
    output$network_density <- renderValueBox({
      net_data <- network_data()
      if (is.null(net_data) || nrow(net_data$nodes) == 0) {
        density_text <- "N/A"
        color <- "black"
      } else {
        nodes_count <- nrow(net_data$nodes)
        edges_count <- nrow(net_data$edges)
        max_edges <- nodes_count * (nodes_count - 1)
        
        if (max_edges == 0) {
          density_text <- "N/A"
          color <- "black"
        } else {
          density <- round((edges_count / max_edges) * 100, 1)
          density_text <- paste0(density, "%")
          color <- "purple"
        }
      }
      
      valueBox(
        value = density_text,
        subtitle = "Network Density",
        icon = icon("project-diagram"),
        color = color
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
      
      # Get the actual attack count from the data
      attack_count <- if (node_info$group == "ip") {
        nrow(data()[source_ip == node_info$label])
      } else {
        # For port nodes
        nrow(data()[destination_port == as.integer(gsub("Port ", "", node_info$label))])
      }
      
      tagList(
        h5(paste("Node:", node_info$label), style = "color: #00d4ff;"),
        p(paste("Group:", node_info$group)),
        p(paste("Attacks:", attack_count)),
        
        # Show related traffic if it's an IP node
        if (node_info$group == "ip") {
          req(data())
          related_traffic <- data()[source_ip == node_info$label]
          
          if (nrow(related_traffic) > 0) {
            tagList(
              h6("Attack Details:", style = "color: #00d4ff;"),
              p(paste("Origin Country:", paste(unique(related_traffic$source_country), collapse = ", "))),
              p(paste("Avg Threat Score:", round(mean(related_traffic$threat_score), 2))),
              p(paste("Data Volume:", scales::comma(sum(related_traffic$length)), "bytes"))
            )
          }
        } else if (node_info$group == "port") {
          # Show details for port nodes
          req(data())
          port_num <- as.integer(gsub("Port ", "", node_info$label))
          port_traffic <- data()[destination_port == port_num]
          
          if (nrow(port_traffic) > 0) {
            tagList(
              h6("Port Details:", style = "color: #00d4ff;"),
              p(paste("Unique Attackers:", uniqueN(port_traffic$source_ip))),
              p(paste("Protocols:", paste(unique(port_traffic$protocol), collapse = ", "))),
              p(paste("Avg Threat Score:", round(mean(port_traffic$threat_score), 2)))
            )
          }
        }
      )
    })
    
    
  })
}

# Helper function for simple clustering
apply_simple_clustering <- function(nodes, data, cluster_by) {
  # Handle empty nodes
  if (nrow(nodes) == 0) {
    return(nodes)
  }
  
  if (cluster_by == "none") {
    # Default coloring by node type
    for (i in 1:nrow(nodes)) {
      if (nodes$group[i] == "port") {
        nodes$color[i] <- "#3498db"  # Blue for ports
      } else {
        nodes$color[i] <- "#e74c3c"  # Red for IPs
      }
    }
  } else if (cluster_by == "country") {
    # Group nodes by country for IP nodes
    for (i in 1:nrow(nodes)) {
      if (nodes$group[i] == "ip") {
        ip_country <- data[source_ip == nodes$label[i], unique(source_country)][1]
        if (!is.na(ip_country)) {
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
      if (nodes$group[i] == "ip") {
        ip_threat <- data[source_ip == nodes$label[i], mean(threat_score)]
        if (!is.na(ip_threat) && length(ip_threat) > 0) {
          if (ip_threat > 7) {
            nodes$color[i] <- "#e74c3c"  # High threat - red
          } else if (ip_threat > 4) {
            nodes$color[i] <- "#f39c12"  # Medium threat - orange
          } else {
            nodes$color[i] <- "#27ae60"  # Low threat - green
          }
        }
      }
    }
  }
  
  return(nodes)
}