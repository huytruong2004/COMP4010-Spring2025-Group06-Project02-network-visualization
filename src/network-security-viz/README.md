# Network Security Visualization Dashboard - MVP

A simplified R Shiny dashboard for visualizing VPS security threats with enhanced theming capabilities.

## Overview

This dashboard provides three core visualization modules for analyzing network security data:
- **Timeline Analysis**: Interactive time-series charts showing attack patterns over time
- **Network Graph**: Force-directed visualization of IP-to-port attack relationships  
- **Geographic Analysis**: Country-level choropleth maps showing attack origins

## Key Features

### 🎨 Enhanced Theme System (Learning Element)
- **Viridis Color Integration**: Perceptually uniform color palettes optimized for cybersecurity visualization
- **Multi-Library Support**: Consistent theming across ggplot2, plotly, visNetwork, and DT
- **Context-Aware Palettes**: Automatic color selection based on visualization type (temporal, geographic, network, threat)
- **Accessibility Features**: Colorblind-safe options with proper contrast ratios

### 📊 Core Visualizations
- **Interactive Timeline**: Dygraphs-based time series with date range selection
- **Network Graph**: visNetwork force-directed layout with country/threat clustering
- **Geographic Heatmap**: Plotly choropleth maps with attack distribution
- **Dashboard Overview**: Summary statistics and trend analysis

### 🔧 Simplified Architecture
- **Single Theme System**: Consolidated `theme_enhanced.R` with viridis integration
- **Streamlined Modules**: Three focused visualization modules instead of complex enterprise features
- **Direct CSV Loading**: Simple data pipeline without complex caching
- **Minimal Dependencies**: 8 essential packages instead of 20+ complex dependencies

## Installation

1. **Clone the repository**
```bash
git clone <repository-url>
cd src/network-security-viz
```

2. **Install required R packages**
```r
# Essential packages only
install.packages(c(
  "shiny",        # Core framework
  "shinydashboard", # Dashboard layout
  "data.table",   # Fast data processing
  "visNetwork",   # Network graphs
  "dygraphs",     # Time series
  "plotly",       # Interactive plots
  "DT",           # Data tables
  "viridis",      # Color palettes
  "ggplot2",      # Static plots
  "scales"        # Number formatting
))
```

3. **Run the application**
```r
# From the network-security-viz directory
shiny::runApp("app.R")
```

## Data Format

The application expects CSV data with the following columns:
- `timestamp`: UNIX timestamp
- `source_ip`: IPv4 address of attacker
- `source_country`: Country name of attack origin
- `destination_port`: Target port number (1-65535)
- `protocol`: Network protocol (TCP/UDP)
- `length`: Packet size in bytes

Sample data is automatically generated if the default file is not found.

## Usage

1. **Upload Data**: Use the sidebar file input to upload your own CSV data
2. **Global Filters**: Apply time range filters that affect all visualizations
3. **Timeline Analysis**: View attack patterns over time with interactive charts
4. **Network Analysis**: Explore IP-to-port relationships with clustering options
5. **Geographic View**: Analyze attack origins by country with choropleth maps
6. **Export Data**: Download processed data from each visualization module

## Enhanced Theme System

The key learning element of this project is the enhanced theme system that demonstrates:

### Viridis Integration
```r
# Context-aware palette selection
get_threat_palette(context = "temporal", n_colors = 5)
get_threat_palette(context = "geographic", n_colors = 8) 
get_threat_palette(context = "network", n_colors = 6)
```

### Multi-Library Theming
```r
# Consistent themes across visualization libraries
theme_security_enhanced(palette_context = "temporal")    # ggplot2
apply_viridis_plotly_theme(plot, palette_context = "geographic")  # plotly
enhance_visnetwork_theme()  # visNetwork
enhance_dt_theme()         # DT tables
```

### Accessibility Features
- WCAG-compliant color contrast ratios
- Colorblind-safe palette options
- Screen reader compatible markup
- Keyboard navigation support

## Project Structure

```
network-security-viz/
├── app.R                 # Main Shiny application
├── global.R              # Package loading and configuration
├── R/
│   ├── mod_timeline.R    # Timeline visualization module
│   ├── mod_network.R     # Network graph module
│   ├── mod_geographic.R  # Geographic visualization module
│   ├── utils_data.R      # Data processing utilities
│   └── theme_enhanced.R  # Enhanced theme system
├── www/
│   └── custom.css        # Custom styling
└── README.md            # This file
```

## Course Project Context

This project fulfills the course requirements by:
- **Self-Directed Learning**: Enhanced viridis theme system integration across multiple R visualization libraries
- **Reproducible Development**: Simple setup with minimal dependencies
- **Interactive Visualizations**: Three distinct visualization paradigms (temporal, network, geographic)
- **Clean Architecture**: Modular Shiny design with separation of concerns

## Performance

- **Load Time**: ~2-3 seconds for 10K records
- **Memory Usage**: ~50-100MB typical
- **Recommended Limits**: Up to 100K records, 200 network nodes
- **Browser Support**: Modern browsers with JavaScript enabled

## Troubleshooting

**Data Loading Issues**: 
- Ensure CSV file has required columns
- Check file path is accessible
- Sample data will load automatically if default file missing

**Package Installation**: 
- Use `install.packages()` for CRAN packages
- Restart R session after installation
- Check R version compatibility (≥4.0 recommended)

**Performance**: 
- Reduce data size for better performance
- Use time filtering to limit displayed data
- Adjust network node limits in controls

## License

This project is created for educational purposes as part of a university course assignment.