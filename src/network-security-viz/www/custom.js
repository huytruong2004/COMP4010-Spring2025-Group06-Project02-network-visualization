// Custom JavaScript for Network Security Dashboard
// Accessibility and Enhancement Features - Phase 6

$(document).ready(function() {
  
  // Add accessibility controls to the page
  addAccessibilityControls();
  
  // Initialize accessibility features
  initializeAccessibility();
  
  // Add keyboard navigation enhancements
  enhanceKeyboardNavigation();
  
  // Add ARIA labels and roles
  addAriaLabels();
  
});

// Add accessibility control buttons
function addAccessibilityControls() {
  const accessibilityHtml = `
    <div class="accessibility-controls" role="toolbar" aria-label="Accessibility Controls">
      <button class="contrast-toggle" id="contrastToggle" 
              aria-label="Toggle high contrast mode" 
              title="Toggle high contrast mode">
        <i class="fa fa-adjust"></i> Contrast
      </button>
      <button class="font-size-toggle" id="fontToggle" 
              aria-label="Toggle large fonts" 
              title="Toggle large fonts">
        <i class="fa fa-font"></i> Font Size
      </button>
    </div>
  `;
  
  $('body').append(accessibilityHtml);
  
  // Add skip link
  const skipLink = `
    <a href="#main-content" class="skip-link">
      Skip to main content
    </a>
  `;
  
  $('body').prepend(skipLink);
  
  // Add main content landmark
  $('.content-wrapper').attr('id', 'main-content').attr('role', 'main');
}

// Initialize accessibility features
function initializeAccessibility() {
  
  // High contrast toggle
  $('#contrastToggle').click(function() {
    $('body').toggleClass('high-contrast');
    const isHighContrast = $('body').hasClass('high-contrast');
    
    // Update button text and aria state
    $(this).attr('aria-pressed', isHighContrast);
    
    // Store preference
    localStorage.setItem('highContrast', isHighContrast);
    
    // Log accessibility action
    console.log('High contrast mode:', isHighContrast ? 'enabled' : 'disabled');
    
    // Show notification
    showAccessibilityNotification(
      isHighContrast ? 'High contrast mode enabled' : 'High contrast mode disabled'
    );
  });
  
  // Font size toggle
  $('#fontToggle').click(function() {
    $('body').toggleClass('large-fonts');
    const isLargeFonts = $('body').hasClass('large-fonts');
    
    // Update button text and aria state
    $(this).attr('aria-pressed', isLargeFonts);
    
    // Store preference
    localStorage.setItem('largeFonts', isLargeFonts);
    
    // Log accessibility action
    console.log('Large fonts mode:', isLargeFonts ? 'enabled' : 'disabled');
    
    // Show notification
    showAccessibilityNotification(
      isLargeFonts ? 'Large fonts enabled' : 'Large fonts disabled'
    );
  });
  
  // Restore saved preferences
  if (localStorage.getItem('highContrast') === 'true') {
    $('#contrastToggle').click();
  }
  
  if (localStorage.getItem('largeFonts') === 'true') {
    $('#fontToggle').click();
  }
}

// Enhanced keyboard navigation
function enhanceKeyboardNavigation() {
  
  // Add tab order to interactive elements
  let tabIndex = 1;
  
  // Sidebar navigation
  $('.sidebar-menu a').each(function() {
    $(this).attr('tabindex', tabIndex++);
  });
  
  // Form controls
  $('.form-control, .btn, .selectize-input').each(function() {
    if (!$(this).attr('tabindex')) {
      $(this).attr('tabindex', tabIndex++);
    }
  });
  
  // Tab navigation improvements
  $(document).on('keydown', function(e) {
    
    // Escape key to close modals/dropdowns
    if (e.keyCode === 27) {
      $('.modal').modal('hide');
      $('.dropdown-menu').removeClass('show');
    }
    
    // Enter key on focused elements
    if (e.keyCode === 13) {
      const focused = $(document.activeElement);
      if (focused.hasClass('sidebar-menu') || focused.closest('.sidebar-menu').length) {
        focused.click();
      }
    }
    
    // Arrow key navigation for tab panels
    if (e.keyCode >= 37 && e.keyCode <= 40) {
      const focused = $(document.activeElement);
      if (focused.closest('.nav-tabs').length) {
        handleArrowNavigation(e, focused);
      }
    }
  });
  
  // Focus management for dynamic content
  $(document).on('shown.bs.tab', 'a[data-toggle="tab"]', function() {
    $(this).focus();
    announceTabChange($(this).text());
  });
}

// Handle arrow key navigation in tabs
function handleArrowNavigation(e, element) {
  const tabs = element.closest('.nav-tabs').find('a[data-toggle="tab"]');
  const currentIndex = tabs.index(element);
  let newIndex;
  
  switch(e.keyCode) {
    case 37: // Left arrow
    case 38: // Up arrow
      newIndex = currentIndex > 0 ? currentIndex - 1 : tabs.length - 1;
      break;
    case 39: // Right arrow
    case 40: // Down arrow
      newIndex = currentIndex < tabs.length - 1 ? currentIndex + 1 : 0;
      break;
  }
  
  if (newIndex !== undefined) {
    e.preventDefault();
    tabs.eq(newIndex).focus().tab('show');
  }
}

// Add ARIA labels and roles
function addAriaLabels() {
  
  // Navigation roles
  $('.sidebar-menu').attr('role', 'navigation').attr('aria-label', 'Main navigation');
  
  // Tab panels
  $('.nav-tabs').attr('role', 'tablist');
  $('.nav-tabs a').attr('role', 'tab');
  $('.tab-content .tab-pane').attr('role', 'tabpanel');
  
  // Data tables
  $('.dataTable').attr('role', 'table');
  $('.dataTable thead').attr('role', 'rowgroup');
  $('.dataTable tbody').attr('role', 'rowgroup');
  $('.dataTable tr').attr('role', 'row');
  $('.dataTable th, .dataTable td').attr('role', 'cell');
  
  // Form labels
  $('label').each(function() {
    const input = $(this).next('input, select, textarea');
    if (input.length && !input.attr('aria-label')) {
      input.attr('aria-label', $(this).text());
    }
  });
  
  // Value boxes
  $('.small-box').attr('role', 'status').attr('aria-live', 'polite');
  
  // Loading indicators
  $('.load-container').attr('aria-label', 'Loading content').attr('role', 'status');
  
  // Interactive visualizations
  $('.vis-network').attr('role', 'img').attr('aria-label', 'Network visualization graph');
  $('.plotly').attr('role', 'img').attr('aria-label', 'Interactive chart');
  $('.dygraphs').attr('role', 'img').attr('aria-label', 'Time series chart');
}

// Show accessibility notifications
function showAccessibilityNotification(message) {
  // Create notification element if it doesn't exist
  let notification = $('#accessibility-notification');
  if (notification.length === 0) {
    notification = $(`
      <div id="accessibility-notification" 
           role="status" 
           aria-live="polite" 
           aria-atomic="true"
           style="position: fixed; top: 60px; right: 10px; z-index: 10000; 
                  background: var(--accent); color: var(--bg-primary); 
                  padding: 10px 15px; border-radius: 5px; 
                  box-shadow: 0 4px 8px rgba(0,0,0,0.3);
                  display: none;">
      </div>
    `);
    $('body').append(notification);
  }
  
  // Show notification
  notification.text(message).fadeIn(300);
  
  // Hide after 3 seconds
  setTimeout(function() {
    notification.fadeOut(300);
  }, 3000);
}

// Announce tab changes for screen readers
function announceTabChange(tabName) {
  const announcement = `Switched to ${tabName} tab`;
  
  // Create or update live region
  let liveRegion = $('#tab-announcement');
  if (liveRegion.length === 0) {
    liveRegion = $(`
      <div id="tab-announcement" 
           class="sr-only" 
           aria-live="polite" 
           aria-atomic="true">
      </div>
    `);
    $('body').append(liveRegion);
  }
  
  liveRegion.text(announcement);
}

// Add focus indicators for visualizations
$(document).on('focus', '.vis-network, .plotly, .dygraphs', function() {
  $(this).attr('aria-describedby', 'viz-instructions');
  
  // Add instructions if they don't exist
  if ($('#viz-instructions').length === 0) {
    const instructions = $(`
      <div id="viz-instructions" class="sr-only">
        Use arrow keys to navigate. Press Enter to interact. Press Escape to exit.
      </div>
    `);
    $('body').append(instructions);
  }
});

// Monitor for dynamic content changes
const observer = new MutationObserver(function(mutations) {
  mutations.forEach(function(mutation) {
    if (mutation.type === 'childList') {
      // Re-apply accessibility features to new content
      addAriaLabels();
      
      // Announce significant content changes
      const addedNodes = Array.from(mutation.addedNodes);
      const hasSignificantContent = addedNodes.some(node => 
        node.nodeType === 1 && 
        (node.classList.contains('box') || 
         node.classList.contains('small-box') ||
         node.tagName === 'TABLE')
      );
      
      if (hasSignificantContent) {
        showAccessibilityNotification('Content updated');
      }
    }
  });
});

// Start observing changes
observer.observe(document.body, {
  childList: true,
  subtree: true
});

// Add error handling for failed visualizations
$(document).on('error', '.plotly, .vis-network, .dygraphs', function() {
  const errorMessage = 'Visualization failed to load. Please refresh or contact support.';
  $(this).attr('aria-label', errorMessage);
  showAccessibilityNotification(errorMessage);
});

// Enhance form validation accessibility
$(document).on('invalid', '.form-control', function() {
  const field = $(this);
  const label = $('label[for="' + field.attr('id') + '"]').text() || 'This field';
  const message = field[0].validationMessage;
  
  showAccessibilityNotification(`${label}: ${message}`);
  
  // Add error styling
  field.closest('.form-group').addClass('has-error');
});

$(document).on('input', '.form-control', function() {
  // Remove error styling when user starts typing
  $(this).closest('.form-group').removeClass('has-error');
});

console.log('Network Security Dashboard: Accessibility features loaded');