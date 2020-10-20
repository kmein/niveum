rec {
  text-color = "#636B75"; # Grey text
  text-medium-color = "#8c96a5"; # Medium grey text
  text-light-color = "#BAC0C6"; # Light grey text
  accent-color = "#00a1ff"; # Blue
  background-color = "#F7F8F9"; # Light grey background
  background-color-2 = "#F4F5F6"; # Light grey background
  background-card-color = "rgba(255,255,255,1.0)"; # White background
  border-color = "#E8E8E8"; # Light grey border

  # Header
  primary-color = text-color; # Background
  text-primary-color = "#FFF"; # Text

  # Left Menu
  paper-listbox-background-color = background-color; # Background
  # TODO = Text and Icons

  # UI
  paper-card-header-color = text-color; # Title in settings
  primary-background-color = background-color; # Background color (also title background in left menu)

  # Card
  paper-card-background-color = background-card-color; # Background
  dark-primary-color = text-color;
  primary-text-color = text-color;
  paper-listbox-color = text-color;
  light-primary-color = text-light-color;
  secondary-text-color = text-medium-color;
  disabled-text-color = text-light-color;
  paper-dialog-button-color = text-color;
  secondary-background-color = background-color-2; # Background more info title

  # Icons
  paper-item-icon-color = text-light-color; # Off
  paper-item-icon-active-color = accent-color; # On

  # Switches
  switch-checked-button-color = "#FFF"; # Knob On
  switch-unchecked-button-color = "#FFF"; # Knob Off
  switch-checked-track-color = "#0077FF"; # Background On
  switch-unchecked-track-color = disabled-text-color; # Background Off

  # Slider
  paper-slider-active-color = accent-color; # Line On
  paper-slider-container-color = "#e5e7ea"; # Line Off
  paper-slider-knob-color = text-light-color; # Knob On
  paper-slider-knob-start-color = text-light-color; # Knob Off

  # Shadows
  ha-card-box-shadow = "inset 0px 0px 0px 1px var(--border-color)";
}
