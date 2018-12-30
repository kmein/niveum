let
  antares = rec {
    black = { light = "#151515"; dark = "#000000"; };
    red = { light = "#de575c"; dark = "#de575c"; };
    yellow = { light = "#ebe971"; dark = "#ebe971"; };
    green = { light = "#00b853"; dark = "#00b853"; };
    blue = { light = "#90d0f0"; dark = "#7fc6f0"; };
    magenta = { light = "#cf9ffa"; dark = "#cf9ffa"; };
    cyan = { light = "#4ae5e8"; dark = "#4ae5e8"; };
    white = { light = "#ffffff"; dark = "#bbbbbb"; };
    background = black.dark;
    foreground = white.dark;
  };
  solarizedDark = rec {
    black = { dark = "#073642"; light = "#002b36"; };
    red = { dark = "#dc322f"; light = "#cb4b16"; };
    yellow = { dark = "#b58900"; light = "#657b83"; };
    green = { dark = "#859900"; light = "#586e75"; };
    blue = { dark = "#268bd2"; light = "#839496"; };
    magenta = { dark = "#d33682"; light = "#6c71c4"; };
    cyan = { dark = "#2aa198"; light = "#93a1a1"; };
    white = { dark = "#eee8d5"; light = "#fdf6e3"; };
    background = black.light;
    foreground = blue.light;
    fadeColor = black.light;
    cursorColor = cyan.light;
    pointerColorBackground = green.light;
    pointerColorForeground = cyan.light;
  };
  solarizedLight = solarizedDark // {
    background = solarizedDark.white.light;
    foreground = solarizedDark.yellow.light;
    fadeColor = solarizedDark.white.light;
    cursorColor = solarizedDark.green.light;
    pointerColorBackground = solarizedDark.cyan.light;
    pointerColorForeground = solarizedDark.green.light;
  };
in rec {
  uiFont = { name = "Roboto"; size = 9; };
  terminalFont = { name = "Source Code Pro for Powerline"; size = 9; };

  white = "#ffffff";
  black = "#000000";
  gray = "#888888";

  colorScheme = solarizedDark;
  invertedColorScheme = solarizedLight;

  colorPalette = [
    colorScheme.white.dark colorScheme.red.dark colorScheme.green.dark colorScheme.yellow.dark colorScheme.blue.dark colorScheme.magenta.dark colorScheme.cyan.dark colorScheme.black.dark
    colorScheme.white.light colorScheme.red.light colorScheme.green.light colorScheme.yellow.light colorScheme.blue.light colorScheme.magenta.light colorScheme.cyan.light colorScheme.black.light
  ];
}
