let
  flip = scheme: scheme // rec {
    background = scheme.foreground;
    foreground = scheme.background;
    fadeColor = background;
  };
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
  connormcd = rec {
    black = { light = "#454545"; dark = "#222222"; };
    red = { light = "#FA8072"; dark = "#B22222"; };
    green = { light = "#779A3E"; dark = "#556B2F"; };
    yellow = { light = "#DAA520"; dark = "#B8860B"; };
    blue = { light = "#6495ED"; dark = "#4682B4"; };
    magenta = { light = "#DA70D6"; dark = "#9932CC"; };
    cyan = { light = "#B0E0E6"; dark = "#87CEEB"; };
    white = { light = "#FFFFFF"; dark = "#C0C0C0"; };
    background = "#000000";
    foreground = "#AAAAAA";
    cursorColor = green.light;
  };
  apprentice = rec {
    black = { light = "#444444"; dark = "#1c1c1c"; };
    red = { light = "#ff8700"; dark = "#af5f5f"; };
    green = { light = "#87af87"; dark = "#5f875f"; };
    yellow = { light = "#ffffaf"; dark = "#87875f"; };
    blue = { light = "#8fafd7"; dark = "#5f87af"; };
    magenta = { light = "#8787af"; dark = "#5f5f87"; };
    cyan = { light = "#5fafaf"; dark = "#5f8787"; };
    white = { light = "#999999"; dark = "#6c6c6c"; };
    background = "#262626";
    foreground = "#bcbcbc";
    cursorColor = "#bcbcbc";
  };
  macOS = rec {
    black = { light = "#818383"; dark = "#000000"; };
    red = { light = "#fc391f"; dark = "#c23621"; };
    yellow = { light = "#eaec23"; dark = "#adad27"; };
    green = { light = "#31e722"; dark = "#25bc24"; };
    blue = { light = "#5833ff"; dark = "#492ee1"; };
    magenta = { light = "#f935f8"; dark = "#d338d3"; };
    cyan = { light = "#14f0f0"; dark = "#33bbc8"; };
    white = { light = "#e9ebeb"; dark = "#cbcccd"; };
    background = black.dark;
    foreground = white.dark;
    cursorColor = green.light;
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
    cursorColor = cyan.light;
  };
  solarizedLight = solarizedDark // {
    background = solarizedDark.white.light;
    foreground = solarizedDark.yellow.light;
    fadeColor = solarizedDark.white.light;
    cursorColor = solarizedDark.green.light;
  };
  owickstrom.default = rec {
    black = { light = "#282c34"; dark = "#282c34"; };
    red = { light = "#e06c75"; dark = "#e06c75"; };
    green = { light = "#98c379"; dark = "#98c379"; };
    yellow = { light = "#e5c07b"; dark = "#e5c07b"; };
    blue = { light = "#61afef"; dark = "#61afef"; };
    magenta = { light = "#c678dd"; dark = "#c678dd"; };
    cyan = { light = "#56b6c2"; dark = "#56b6c2"; };
    white = { light = "#dcdfe4"; dark = "#dcdfe4"; };
    background = black.dark;
    foreground = white.light;
    cursorColor = "#a3b3cc";
  };
  owickstrom.dark = rec {
    black = { light = "#4b4b4b"; dark = "#242424"; };
    red = { light = "#fc1c18"; dark = "#d71c15"; };
    green = { light = "#6bc219"; dark = "#5aa513"; };
    yellow = { light = "#fec80e"; dark = "#fdb40c"; };
    blue = { light = "#0955ff"; dark = "#063b8c"; };
    magenta = { light = "#fb0050"; dark = "#e40038"; };
    cyan = { light = "#3ea8fc"; dark = "#2595e1"; };
    white = { light = "#8c00ec"; dark = "#efefef"; };
    background = "#181818";
    foreground = white.dark;
    fadeColor = background;
    cursorColor = "#bbbbbb";
  };
in rec {
  uiFont = { name = "Sans"; size = 9; };
  terminalFont = { name = "Monospace"; size = 9; };

  white = "#ffffff";
  black = "#000000";
  gray = "#888888";

  colorScheme = owickstrom.dark // { background = "#000000"; };
  invertedColorScheme = flip owickstrom.dark;

  colorPalette = [
    colorScheme.black.dark colorScheme.red.dark colorScheme.green.dark colorScheme.yellow.dark colorScheme.blue.dark colorScheme.magenta.dark colorScheme.cyan.dark colorScheme.white.dark
    colorScheme.black.light colorScheme.red.light colorScheme.green.light colorScheme.yellow.light colorScheme.blue.light colorScheme.magenta.light colorScheme.cyan.light colorScheme.white.light
  ];
}
