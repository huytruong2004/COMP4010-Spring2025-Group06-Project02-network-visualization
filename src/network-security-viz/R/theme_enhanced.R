# Enhanced Security Theme System
# Network Security Visualization Dashboard
# Option A: Hybrid Enhancement with bslib + Viridis Integration
# 
# This file provides enhanced theming capabilities while maintaining
# compatibility with the existing shinydashboard structure.

# =============================================================================
# ENHANCED COLOR SYSTEM WITH VIRIDIS INTEGRATION
# =============================================================================

#' Create Security-Themed Viridis Color Palettes
#' 
#' Provides specialized color palettes optimized for different aspects
#' of security visualization using viridis color theory.
#' 
#' @return List of named color palettes
#' @export
create_security_viridis_palettes <- function() {
  list(
    # Threat-focused palettes for attack visualization
    threat_plasma = viridis::plasma(10, begin = 0.1, end = 0.9),      # Hot threats
    threat_inferno = viridis::inferno(10, begin = 0.2, end = 0.95),   # Network attacks
    
    # Geographic and temporal palettes  
    geo_viridis = viridis::viridis(10, begin = 0.1, end = 0.8),       # Cool geographic
    time_cividis = viridis::cividis(10, begin = 0.1, end = 0.9),      # Perceptually uniform
    
    # Accessibility variants
    high_contrast = c("#000000", "#FFFFFF", "#FF0000", "#00FF00", "#0000FF"),
    colorblind_safe = viridis::mako(8, begin = 0.2, end = 0.9),
    
    # Network analysis specific
    network_turbo = viridis::turbo(8, begin = 0.1, end = 0.9),        # High contrast networks
    
    # Traditional security colors (preserved for compatibility)
    security_classic = c("#00d4ff", "#e74c3c", "#f39c12", "#27ae60", "#8e44ad")
  )
}

#' Get Dynamic Threat Palette Based on Context
#' 
#' Dynamically selects appropriate color palette based on visualization context
#' 
#' @param context Character. One of "threat", "geographic", "temporal", "network"
#' @param n_colors Integer. Number of colors needed
#' @param accessibility Boolean. Use accessibility-enhanced palette
#' @return Character vector of hex colors
#' @export
get_threat_palette <- function(context = "threat", n_colors = 5, accessibility = FALSE) {
  palettes <- create_security_viridis_palettes()
  
  if (accessibility) {
    return(palettes$colorblind_safe[1:min(n_colors, length(palettes$colorblind_safe))])
  }
  
  palette_choice <- switch(context,
    "threat" = palettes$threat_plasma,
    "geographic" = palettes$geo_viridis, 
    "temporal" = palettes$time_cividis,
    "network" = palettes$network_turbo,
    palettes$threat_plasma  # default
  )
  
  # Interpolate if more colors needed
  if (n_colors > length(palette_choice)) {
    palette_choice <- colorRampPalette(palette_choice)(n_colors)
  }
  
  return(palette_choice[1:n_colors])
}

# =============================================================================
# BSLIB BOOTSTRAP 5 THEME INTEGRATION
# =============================================================================

#' Create Security-Themed Bootstrap 5 Theme
#' 
#' Creates a bslib theme optimized for cybersecurity visualization
#' while maintaining dark theme aesthetic.
#' 
#' @return bslib bs_theme object
#' @export
create_security_bs_theme <- function() {
  bslib::bs_theme(
    version = 5,
    
    # Primary brand colors - cyber security palette
    primary = "#00d4ff",           # Cyber blue (primary accent)
    secondary = "#1a1f3a",         # Dark secondary background
    success = "#27ae60",           # Success green
    danger = "#e74c3c",            # Danger red
    warning = "#f39c12",           # Warning orange
    info = "#3498db",              # Info blue
    light = "#e0e0e0",             # Light text
    dark = "#0a0e27",              # Dark background
    
    # Typography system
    base_font = bslib::font_google("Roboto"),
    heading_font = bslib::font_google("Roboto", wght = "700"),
    code_font = bslib::font_google("Roboto Mono"),
    
    # Layout and spacing
    "body-bg" = "#0a0e27",                    # Main background
    "body-color" = "#e0e0e0",                 # Main text color
    "border-radius" = "0.375rem",             # Rounded corners
    "box-shadow" = "0 0.125rem 0.25rem rgba(0, 0, 0, 0.3)",  # Subtle shadows
    
    # Component-specific theming
    "navbar-dark-bg" = "#1a1f3a",
    "navbar-dark-color" = "#e0e0e0",
    "card-bg" = "#1a1f3a",
    "card-border-color" = "#2a2f4a",
    
    # Enhanced accessibility
    "focus-ring-color" = "rgba(0, 212, 255, 0.25)",
    "focus-ring-width" = "0.25rem"
  )
}

# =============================================================================
# ENHANCED VISUALIZATION THEME FUNCTIONS  
# =============================================================================

#' Enhanced Plotly Theme with Viridis Integration
#' 
#' Applies enhanced security theme to plotly visualizations with
#' improved color palettes and accessibility features.
#' 
#' @param p plotly object
#' @param palette_context Character. Context for color palette selection
#' @return Enhanced plotly object
#' @export
apply_viridis_plotly_theme <- function(p, palette_context = "threat") {
  threat_colors <- get_threat_palette(palette_context, n_colors = 8)
  
  p %>%
    plotly::layout(
      paper_bgcolor = "#0a0e27",
      plot_bgcolor = "#0a0e27",
      font = list(
        color = "#e0e0e0",
        family = "Roboto, Arial, sans-serif",
        size = 12
      ),
      xaxis = list(
        gridcolor = "#2a2f4a",
        zerolinecolor = "#2a2f4a",
        tickfont = list(color = "#a0a0a0"),
        titlefont = list(color = "#e0e0e0", size = 14)
      ),
      yaxis = list(
        gridcolor = "#2a2f4a", 
        zerolinecolor = "#2a2f4a",
        tickfont = list(color = "#a0a0a0"),
        titlefont = list(color = "#e0e0e0", size = 14)
      ),
      colorway = threat_colors,
      
      # Enhanced accessibility
      hoverlabel = list(
        bgcolor = "#1a1f3a",
        bordercolor = "#00d4ff",
        font = list(color = "#e0e0e0", size = 12)
      )
    )
}

#' Enhanced Network Colors for visNetwork
#' 
#' Provides enhanced color scheme for network visualizations
#' with improved contrast and threat-level mapping.
#' 
#' @param threat_levels Character vector of threat levels
#' @param palette_type Character. Type of palette to use
#' @return List of enhanced network styling options
#' @export
enhance_network_colors <- function(threat_levels = c("low", "medium", "high"), 
                                 palette_type = "network") {
  colors <- get_threat_palette(palette_type, n_colors = length(threat_levels))
  names(colors) <- threat_levels
  
  list(
    nodes = list(
      font = list(
        color = "#e0e0e0",
        strokeWidth = 2,
        strokeColor = "#0a0e27",
        size = 14
      ),
      borderWidth = 2,
      borderWidthSelected = 4,
      chosen = list(
        node = list(
          color = "#00d4ff"
        )
      )
    ),
    edges = list(
      font = list(
        color = "#a0a0a0",
        strokeWidth = 1,
        strokeColor = "#0a0e27"
      ),
      smooth = list(
        type = "dynamic",
        forceDirection = "none"
      ),
      color = list(
        color = "#2a2f4a",
        highlight = "#00d4ff",
        hover = "#f39c12"
      )
    ),
    threat_colors = colors
  )
}

# =============================================================================
# ENHANCED GGPLOT2 THEME
# =============================================================================

#' Enhanced Security Theme for ggplot2
#' 
#' Extended version of the original theme_security with viridis integration
#' and improved accessibility features.
#' 
#' @param base_size Numeric. Base font size
#' @param palette_context Character. Context for color selection
#' @return ggplot2 theme object
#' @export
theme_security_enhanced <- function(base_size = 12, palette_context = "threat") {
  
  # Get enhanced color palette
  palettes <- create_security_viridis_palettes()
  
  # Core security colors (preserved for compatibility)
  bg_primary <- "#0a0e27"
  bg_secondary <- "#1a1f3a"
  accent <- "#00d4ff"
  text_primary <- "#e0e0e0"
  text_secondary <- "#a0a0a0"
  grid_color <- "#2a2f4a"
  
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      # Background
      plot.background = ggplot2::element_rect(fill = bg_primary, color = NA),
      panel.background = ggplot2::element_rect(fill = bg_primary, color = NA),
      
      # Enhanced grid system
      panel.grid.major = ggplot2::element_line(color = grid_color, linewidth = 0.4),
      panel.grid.minor = ggplot2::element_line(color = grid_color, linewidth = 0.2),
      
      # Enhanced typography
      text = ggplot2::element_text(color = text_primary, family = "Roboto"),
      plot.title = ggplot2::element_text(
        color = accent, 
        size = base_size * 1.5, 
        face = "bold", 
        margin = ggplot2::margin(b = 15),
        hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        color = text_secondary, 
        size = base_size * 1.1, 
        margin = ggplot2::margin(b = 15)
      ),
      axis.text = ggplot2::element_text(color = text_secondary, size = base_size * 0.9),
      axis.title = ggplot2::element_text(color = text_primary, face = "bold", size = base_size * 1.1),
      
      # Enhanced legend
      legend.background = ggplot2::element_rect(fill = bg_secondary, color = grid_color),
      legend.text = ggplot2::element_text(color = text_primary, size = base_size * 0.9),
      legend.title = ggplot2::element_text(color = accent, face = "bold", size = base_size),
      legend.key = ggplot2::element_rect(fill = bg_primary, color = NA),
      
      # Enhanced facets
      strip.background = ggplot2::element_rect(fill = bg_secondary, color = grid_color),
      strip.text = ggplot2::element_text(color = accent, face = "bold", size = base_size),
      
      # Improved panel borders
      panel.border = ggplot2::element_rect(color = grid_color, fill = NA, linewidth = 0.5)
    )
}

# =============================================================================
# ENHANCED COLOR SCALES
# =============================================================================

#' Enhanced Threat Level Fill Scale
#' 
#' @param palette_type Character. Type of viridis palette to use
#' @param ... Additional arguments passed to scale_fill_manual
#' @return ggplot2 scale object
#' @export
scale_fill_threat_enhanced <- function(palette_type = "threat", ...) {
  colors <- get_threat_palette(palette_type, n_colors = 3)
  names(colors) <- c("low", "medium", "high")
  
  ggplot2::scale_fill_manual(
    values = colors,
    name = "Threat Level",
    ...
  )
}

#' Enhanced Threat Level Color Scale
#' 
#' @param palette_type Character. Type of viridis palette to use  
#' @param ... Additional arguments passed to scale_color_manual
#' @return ggplot2 scale object
#' @export
scale_color_threat_enhanced <- function(palette_type = "threat", ...) {
  colors <- get_threat_palette(palette_type, n_colors = 3)
  names(colors) <- c("low", "medium", "high")
  
  ggplot2::scale_color_manual(
    values = colors,
    name = "Threat Level",
    ...
  )
}

# =============================================================================
# ACCESSIBILITY ENHANCEMENT FUNCTIONS
# =============================================================================

#' Check Color Contrast Ratio
#' 
#' Validates color combinations meet WCAG accessibility standards
#' 
#' @param foreground Character. Foreground color hex code
#' @param background Character. Background color hex code
#' @return List with contrast ratio and compliance status
#' @export
check_color_contrast <- function(foreground, background) {
  # Convert hex to RGB
  fg_rgb <- col2rgb(foreground)
  bg_rgb <- col2rgb(background)
  
  # Calculate relative luminance
  get_luminance <- function(rgb) {
    rgb_norm <- rgb / 255
    rgb_lin <- ifelse(rgb_norm <= 0.03928, 
                      rgb_norm / 12.92, 
                      ((rgb_norm + 0.055) / 1.055)^2.4)
    0.2126 * rgb_lin[1] + 0.7152 * rgb_lin[2] + 0.0722 * rgb_lin[3]
  }
  
  fg_lum <- get_luminance(fg_rgb)
  bg_lum <- get_luminance(bg_rgb)
  
  # Calculate contrast ratio
  contrast_ratio <- (max(fg_lum, bg_lum) + 0.05) / (min(fg_lum, bg_lum) + 0.05)
  
  list(
    ratio = round(contrast_ratio, 2),
    wcag_aa = contrast_ratio >= 4.5,
    wcag_aaa = contrast_ratio >= 7.0
  )
}

#' Get Accessible Color Palette
#' 
#' Returns accessibility-compliant color palette for visualizations
#' 
#' @param n_colors Integer. Number of colors needed
#' @param type Character. Type of accessible palette
#' @return Character vector of accessible hex colors
#' @export
get_accessible_palette <- function(n_colors = 5, type = "colorblind_safe") {
  palettes <- create_security_viridis_palettes()
  
  palette_choice <- switch(type,
    "colorblind_safe" = palettes$colorblind_safe,
    "high_contrast" = palettes$high_contrast,
    palettes$colorblind_safe  # default
  )
  
  if (n_colors > length(palette_choice)) {
    palette_choice <- colorRampPalette(palette_choice)(n_colors)
  }
  
  return(palette_choice[1:n_colors])
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Apply Enhanced Theme to Existing Components
#' 
#' Helper function to upgrade existing theme components with enhanced features
#' 
#' @param component Character. Component type to enhance
#' @param ... Additional parameters
#' @return Enhanced component configuration
#' @export
enhance_existing_component <- function(component, ...) {
  switch(component,
    "dt" = enhance_dt_theme(...),
    "plotly" = enhance_plotly_theme(...),
    "visnetwork" = enhance_visnetwork_theme(...),
    stop("Unknown component type")
  )
}

# Enhanced DT theme
enhance_dt_theme <- function() {
  list(
    dom = 'frtip',
    pageLength = 10,
    initComplete = JS("
      function(settings, json) {
        $(this.api().table().container()).css({
          'background-color': '#1a1f3a',
          'color': '#e0e0e0'
        });
        $(this.api().table().header()).css({
          'background-color': '#0a0e27',
          'color': '#00d4ff'
        });
      }
    ")
  )
}

# Enhanced plotly theme (wrapper for apply_viridis_plotly_theme)
enhance_plotly_theme <- function(palette_context = "threat") {
  function(p) apply_viridis_plotly_theme(p, palette_context)
}

# Enhanced visNetwork theme
enhance_visnetwork_theme <- function() {
  enhance_network_colors()
}

# =============================================================================
# THEME TESTING AND VALIDATION
# =============================================================================

#' Test Enhanced Theme System
#' 
#' Validates that all enhanced theme functions work correctly
#' 
#' @return List of test results
#' @export
test_enhanced_theme_system <- function() {
  results <- list()
  
  # Test palette creation
  tryCatch({
    palettes <- create_security_viridis_palettes()
    results$palettes <- "PASS"
  }, error = function(e) {
    results$palettes <- paste("FAIL:", e$message)
  })
  
  # Test bslib theme creation
  tryCatch({
    bs_theme <- create_security_bs_theme()
    results$bslib_theme <- "PASS"
  }, error = function(e) {
    results$bslib_theme <- paste("FAIL:", e$message)
  })
  
  # Test color accessibility
  tryCatch({
    contrast <- check_color_contrast("#00d4ff", "#0a0e27")
    results$accessibility <- if(contrast$wcag_aa) "PASS" else "WARN: Low contrast"
  }, error = function(e) {
    results$accessibility <- paste("FAIL:", e$message)
  })
  
  return(results)
}

# Log successful theme system load
if (interactive()) {
  message("Enhanced Security Theme System loaded successfully")
  message("Available enhancements: viridis palettes, bslib integration, accessibility features")
}