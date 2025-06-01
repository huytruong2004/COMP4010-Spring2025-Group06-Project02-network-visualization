#!/usr/bin/env Rscript

# Installation script for Network Security Visualization Dashboard
# This script installs all required R packages

cat("Installing required packages for Network Security Visualization Dashboard...\n\n")

# Define required packages
required_packages <- c(
  # Shiny ecosystem
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "bslib",
  
  # Data manipulation
  "tidyverse",
  "data.table",
  "lubridate",
  
  # Advanced visualizations
  "visNetwork",     # Network graphs
  "dygraphs",       # Interactive time series
  "plotly",         # 3D plots
  "crosstalk",      # Inter-widget communication
  # "threejs",      # 3D WebGL - Not on CRAN, will use plotly 3D instead
  
  # Supporting packages
  "DT",             # Interactive tables
  "viridis",        # Color palettes
  "shinycssloaders", # Loading spinners
  "pryr",           # Memory profiling
  "xts",            # Time series objects (for dygraphs)
  "igraph",         # Network analysis
  "digest",         # Hashing for caching
  "testthat"        # Testing framework
)

# Function to install packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste("Installing", pkg, "...\n"))
    install.packages(pkg, dependencies = TRUE, repos = "https://cran.r-project.org")
  } else {
    cat(paste("✓", pkg, "already installed\n"))
  }
}

# Install each package
for (pkg in required_packages) {
  tryCatch({
    install_if_missing(pkg)
  }, error = function(e) {
    cat(paste("ERROR installing", pkg, ":", e$message, "\n"))
  })
}

cat("\n\nChecking installation status...\n")

# Check which packages are installed
installed <- required_packages[required_packages %in% installed.packages()[,"Package"]]
missing <- setdiff(required_packages, installed)

if (length(missing) == 0) {
  cat("\n✅ All packages successfully installed!\n")
  cat("\nYou can now run the application with:\n")
  cat("  Rscript run_app.R\n")
  cat("\nOr from R console:\n")
  cat("  shiny::runApp('app.R')\n")
} else {
  cat("\n❌ The following packages could not be installed:\n")
  cat(paste("  -", missing, collapse = "\n"), "\n")
  cat("\nTry installing them manually with:\n")
  cat(paste0('install.packages(c(', paste0('"', missing, '"', collapse = ", "), '))\n'))
}

# Test critical packages
cat("\n\nTesting critical package imports...\n")
critical_packages <- c("shiny", "shinydashboard", "visNetwork", "dygraphs")

for (pkg in critical_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat(paste("✓", pkg, "loaded successfully\n"))
  }, error = function(e) {
    cat(paste("✗", pkg, "failed to load:", e$message, "\n"))
  })
}