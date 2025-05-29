# Global settings and package loading
# Network Security Visualization Dashboard

# Load required packages (simplified for MVP)
suppressPackageStartupMessages({
  # Core framework
  library(shiny)
  library(shinydashboard)
  
  # Data processing
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(scales)
  
  # Visualization libraries
  library(visNetwork)     # Network graphs
  library(dygraphs)       # Interactive time series
  library(plotly)         # Interactive plots
  library(DT)             # Interactive tables
  library(viridis)        # Color palettes
})

# Set global options
options(
  shiny.maxRequestSize = 30*1024^2   # 30MB max file size
)

# Source utility functions (consolidated)
source("R/utils_data.R")
# Source enhanced theme system (single theme system)
source("R/theme_enhanced.R")

# Source Shiny modules (simplified)
source("R/mod_timeline.R")
source("R/mod_network.R")
source("R/mod_geographic.R")