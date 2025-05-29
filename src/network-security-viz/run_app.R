#!/usr/bin/env Rscript

# Network Security Visualization Dashboard
# Run this script to start the application

cat("
╔══════════════════════════════════════════════════════════╗
║        Network Security Visualization Dashboard          ║
║              VPS Threat Monitoring System                ║
╚══════════════════════════════════════════════════════════╝
")

# Check if required packages are installed
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "bslib",
  "tidyverse", "data.table", "lubridate",
  "visNetwork", "dygraphs", "plotly", "crosstalk",
  "DT", "viridis", "shinycssloaders"
)

missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if (length(missing_packages) > 0) {
  cat("\nMissing required packages:", paste(missing_packages, collapse = ", "), "\n")
  cat("Install them with: install.packages(c(", paste0('"', missing_packages, '"', collapse = ", "), "))\n\n")
  stop("Please install missing packages before running the app.")
}

cat("\nStarting application...\n")
cat("Navigate to http://127.0.0.1:8080 in your browser\n\n")

# Run the app
shiny::runApp("app.R", host = "127.0.0.1", port = 8080, launch.browser = TRUE)