# Data Processing Utilities - Simplified
# Network Security Visualization Dashboard
# Consolidated from utils_data.R and utils_network.R

# DATA LOADING AND VALIDATION

# Load and validate network data (simplified - no caching)
load_network_data <- function(file_path) {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  tryCatch({
    # Read CSV with proper column types
    data <- data.table::fread(
      file_path,
      colClasses = c(
        timestamp = "integer",
        source_ip = "character",
        source_country = "character",
        destination_port = "integer",
        protocol = "character",
        length = "integer"
      )
    )
    
    # Validate required columns
    required_cols <- c("timestamp", "source_ip", "source_country", 
                      "destination_port", "protocol", "length")
    missing_cols <- setdiff(required_cols, names(data))
    
    if (length(missing_cols) > 0) {
      stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    }
    
    # Convert timestamp to datetime
    data[, datetime := as.POSIXct(timestamp, origin = "1970-01-01")]
    
    # Add derived columns
    data[, `:=`(
      hour = as.POSIXlt(datetime)$hour,
      date = as.Date(datetime),
      threat_score = calculate_threat_score(destination_port, protocol, length)
    )]
    
    return(data)
    
  }, error = function(e) {
    stop(paste("Error loading data:", e$message))
  })
}

# Calculate threat score based on packet characteristics
calculate_threat_score <- function(port, protocol, length) {
  score <- 0
  
  # High-risk ports
  high_risk_ports <- c(22, 23, 445, 3389, 1433, 3306, 5432)
  score <- ifelse(port %in% high_risk_ports, score + 3, score)
  
  # Unusual packet sizes
  score <- ifelse(length > 10000, score + 2, score)
  score <- ifelse(length < 20, score + 1, score)
  
  # Protocol weighting  
  score <- ifelse(protocol == "TCP" & port < 1024, score + 1, score)
  
  return(pmin(score, 10))  # Cap at 10
}

# DATA AGGREGATION (SIMPLIFIED)

# Aggregate data for time series with complete time sequence
aggregate_timeseries <- function(data, interval = "hour", fill_gaps = TRUE) {
  # Handle empty data
  if (nrow(data) == 0) {
    return(data.table(
      datetime = as.POSIXct(character(0)),
      packet_count = integer(0),
      total_bytes = integer(0),
      unique_ips = integer(0),
      avg_threat_score = numeric(0),
      data_available = logical(0)
    ))
  }
  
  # Aggregate existing data
  result <- switch(interval,
    "hour" = data[, .(
      packet_count = .N,
      total_bytes = sum(length),
      unique_ips = uniqueN(source_ip),
      avg_threat_score = mean(threat_score),
      data_available = TRUE
    ), by = .(datetime = as.POSIXct(format(datetime, "%Y-%m-%d %H:00:00")))],
    
    "day" = data[, .(
      packet_count = .N,
      total_bytes = sum(length),
      unique_ips = uniqueN(source_ip),
      avg_threat_score = mean(threat_score),
      data_available = TRUE
    ), by = .(datetime = as.Date(datetime))],
    
    "minute" = data[, .(
      packet_count = .N,
      total_bytes = sum(length),
      unique_ips = uniqueN(source_ip),
      avg_threat_score = mean(threat_score),
      data_available = TRUE
    ), by = .(datetime = as.POSIXct(format(datetime, "%Y-%m-%d %H:%M:00")))]
  )
  
  # Fill gaps if requested
  if (fill_gaps && nrow(result) > 0) {
    # Create complete time sequence
    min_time <- min(result$datetime)
    max_time <- max(result$datetime)
    
    complete_sequence <- switch(interval,
      "hour" = seq(from = min_time, to = max_time, by = "hour"),
      "day" = seq(from = min_time, to = max_time, by = "day"),
      "minute" = seq(from = min_time, to = max_time, by = "min")
    )
    
    # Convert to data.table for day interval
    if (interval == "day") {
      complete_dt <- data.table(datetime = as.Date(complete_sequence))
    } else {
      complete_dt <- data.table(datetime = complete_sequence)
    }
    
    # Left join with aggregated data
    result <- merge(complete_dt, result, by = "datetime", all.x = TRUE)
    
    # Fill NAs with appropriate values
    result[is.na(packet_count), `:=`(
      packet_count = 0,
      total_bytes = 0,
      unique_ips = 0,
      avg_threat_score = NA_real_,  # Keep NA for periods with no data
      data_available = FALSE
    )]
  }
  
  return(result[order(datetime)])
}

# Aggregate by country for geographic visualization
aggregate_geographic <- function(data) {
  data[!is.na(source_country), .(
    attack_count = .N,
    threat_score = mean(threat_score, na.rm = TRUE),
    total_bytes = sum(length, na.rm = TRUE),
    unique_ips = uniqueN(source_ip)
  ), by = source_country][order(-attack_count)]
}

# NETWORK ANALYSIS FUNCTIONS

# Convert flat packet data to network structure
create_network_data <- function(packet_data, max_nodes = NULL, top_ports_count = 20) {
  # Handle empty data case
  if (nrow(packet_data) == 0) {
    return(list(
      nodes = data.frame(id = integer(0), label = character(0), group = character(0), 
                        value = numeric(0), color = character(0)),
      edges = data.frame(from = integer(0), to = integer(0), width = numeric(0), color = character(0))
    ))
  }
  
  # Get top N ports by frequency
  top_ports <- packet_data[, .N, by = destination_port][order(-N)][1:min(top_ports_count, .N)]
  
  # Filter data to only include traffic to these top ports
  filtered_data <- packet_data[destination_port %in% top_ports$destination_port]
  
  # Handle case where filtered data is empty
  if (nrow(filtered_data) == 0) {
    return(list(
      nodes = data.frame(id = integer(0), label = character(0), group = character(0), 
                        value = numeric(0), color = character(0)),
      edges = data.frame(from = integer(0), to = integer(0), width = numeric(0), color = character(0))
    ))
  }
  
  # Get all unique IPs that connect to these ports
  unique_ips <- unique(filtered_data$source_ip)
  
  # Create IP nodes
  ip_nodes <- filtered_data[, .(
    attacks = .N,
    total_bytes = sum(length),
    countries = paste(unique(source_country), collapse = ", "),
    avg_threat_score = mean(threat_score)
  ), by = source_ip]
  
  ip_nodes[, `:=`(
    id = .I,
    label = source_ip,
    group = "ip",  # Default group for IP nodes
    value = log10(attacks + 1) * 10,
    color = "#e74c3c"  # Default color for IP nodes
  )]
  
  # Create port nodes (already filtered to top N ports)
  port_nodes <- filtered_data[, .(
    connections = .N,
    unique_ips = uniqueN(source_ip)
  ), by = destination_port]
  
  port_nodes[, `:=`(
    id = destination_port + max(ip_nodes$id),
    label = paste("Port", destination_port),
    group = "port",
    value = log10(connections + 1) * 5,
    color = "#3498db"
  )]
  
  # Combine nodes
  nodes <- rbind(
    ip_nodes[, .(id, label, group, value, color)],
    port_nodes[, .(id, label, group, value, color)]
  )
  
  # Create edges
  edges <- filtered_data[, .N, by = .(source_ip, destination_port)]
  edges <- merge(edges, ip_nodes[, .(source_ip, from = id)], by = "source_ip")
  edges[, to := destination_port + max(ip_nodes$id)]
  
  edges[, `:=`(
    width = pmin(log10(N) * 2, 10),
    color = "#95a5a6"
  )]
  
  return(list(
    nodes = as.data.frame(nodes),
    edges = as.data.frame(edges[, .(from, to, width, color)])
  ))
}

# Simple clustering based on attack patterns
cluster_by_country <- function(packet_data) {
  # Group IPs by country for simple clustering
  country_clusters <- packet_data[, .(
    ips = list(unique(source_ip)),
    attack_count = .N,
    avg_threat = mean(threat_score)
  ), by = source_country]
  
  return(country_clusters)
}

# FILTERING FUNCTIONS
# Filter data by date range
filter_by_date <- function(data, start_date = NULL, end_date = NULL) {
  if (!is.null(start_date)) {
    data <- data[datetime >= as.POSIXct(start_date)]
  }
  if (!is.null(end_date)) {
    data <- data[datetime <= as.POSIXct(end_date)]
  }
  return(data)
}

# Filter by threat score threshold
filter_by_threat <- function(data, min_threat = 0, max_threat = 10) {
  data[threat_score >= min_threat & threat_score <= max_threat]
}

# Filter by protocol
filter_by_protocol <- function(data, protocols = NULL) {
  if (!is.null(protocols) && length(protocols) > 0) {
    data[protocol %in% protocols]
  } else {
    data
  }
}

# Filter by country
filter_by_country <- function(data, countries = NULL) {
  if (!is.null(countries) && length(countries) > 0) {
    data[source_country %in% countries]
  } else {
    data
  }
}