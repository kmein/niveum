rec {
# Colors
 text-color = "#DADADB"; # Grey text
 text-medium-light-color = "#A0A2A8"; # Medium-light grey text
 text-medium-color = "#80828A"; # Medium grey text
 text-dark-color = "#6A6B74"; # Dark grey text
 accent-color = "#008bef"; # Blue
 accent-medium-color = "#2484C9"; # Decent blue
 background-color = "#3b4049"; # Dark grey background
 background-color-2 = "#484E59"; # Light grey background
 background-card-color = "#434952"; # Grey background
 border-color = "#383C46"; # Grey border

 # Header
 app-header-background-color = "#363941"; # Background color

 # Text
 primary-color = text-color;
 text-primary-color = text-color;

 # Left Menu
 paper-listbox-background-color = background-color; # Background
 sidebar-icon-color = text-medium-color; # icons
 sidebar-selected-icon-color = text-medium-light-color; # Selected row icon and background (15%)
 sidebar-selected-text-color = text-color; # Selected row label

 # UI
 paper-card-header-color = text-color; # Title in settings
 primary-background-color = background-color; # Background (also title background in left menu)
 mdc-theme-primary = accent-medium-color; # Action Buttons (save, restart etc.)
 card-background-color = background-card-color; # Entity Registry Background

 # Card
 paper-card-background-color = background-card-color; # Background
 dark-primary-color = text-color;
 primary-text-color = text-color;
 paper-listbox-color = text-color;
 light-primary-color = text-dark-color;
 secondary-text-color = text-medium-color;
 disabled-text-color = text-dark-color;
 paper-dialog-button-color = text-color;
 secondary-background-color = background-color-2; # Background more info title

 # Icons
 paper-item-icon-color = text-dark-color; # Off
 paper-item-icon-active-color = accent-color; # On

 # Switches
 switch-checked-button-color = text-medium-light-color; # Knob On
 switch-unchecked-button-color = text-medium-light-color; # Knob Off
 switch-checked-track-color = "#009FFF"; # Background On
 switch-unchecked-track-color = "#767682"; # Background Off

 # Slider
 paper-slider-active-color = accent-color; # Line On
 paper-slider-knob-color = text-medium-light-color; # Knob On
 paper-slider-container-color = text-dark-color; # Line Off
 paper-slider-knob-start-color = text-medium-light-color; # Knob Off

 # Badges
 label-badge-text-color = text-color;
 label-badge-background-color = "rgba(54, 57, 65, 0.6)";

 # Shadows
 ha-card-box-shadow = "inset 0px 0px 0px 1px var(--border-color)";

 # HACS
 hacs-badge-color = accent-color; # New Badge
 hacs-status-installed = text-color; # Installed Icon
 hacs-status-pending-restart = text-dark-color; # Restart Icon
 hacs-status-pending-update = accent-color;
}
