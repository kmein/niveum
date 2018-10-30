rec {
  uiFont = { name = "Roboto"; size = 10; };
  terminalFont = {
    regular = { name = "Source Code Pro for Powerline"; };
    bold = { name = "Source Code Pro Bold for Powerline"; };
    italic = { name = "Source Code Pro Italic for Powerline"; };
    boldItalic = { name = "Source Code Pro Bold Italic for Powerline"; };
    size = 10;
  };

  white = "#ffffff";
  black = "#000000";
  veryDark = "#080808";

  colorPalette = [
    gray.dark red.dark green.dark yellow.dark blue.dark magenta.dark cyan.dark gray.light
    gray.dark red.light green.light yellow.light blue.light magenta.light cyan.light gray.light
  ];

  # Antares color scheme
  gray = { light = "#bbbbbb"; dark = "#151515"; medium = "#aaaaaa"; };
  red = { light = "#de575c"; dark = "#de575c"; };
  yellow = { light = "#ebe971"; dark = "#ebe971"; };
  green = { light = "#00b853"; dark = "#00b853"; };
  blue = { light = "#90d0f0"; dark = "#7fc6f0"; };
  magenta = { light = "#cf9ffa"; dark = "#cf9ffa"; };
  cyan = { light = "#4ae5e8"; dark = "#4ae5e8"; };
}
