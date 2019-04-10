{ config }:
with import <niveum/theme.nix>;
{
  enable = true;
  separator = "solid";
  scrollbar = false;
  terminal = config.defaultApplications.terminal;
  borderWidth = 0;
  lines = 5;
  font = "${uiFont.name} ${toString (uiFont.size + 1)}";
  colors = rec {
    window = { background = invertedColorScheme.background; border = invertedColorScheme.background; separator = invertedColorScheme.black.light; };
    rows = {
      normal = {
        background = window.background;
        backgroundAlt = window.background;
        foreground = invertedColorScheme.foreground;
        highlight = { foreground = invertedColorScheme.cyan.dark; inherit (window) background; };
      };
      active = {
        background = window.background;
        backgroundAlt = window.background;
        foreground = invertedColorScheme.yellow.dark;
        highlight = { foreground = invertedColorScheme.green.dark; inherit (window) background; };
      };
      urgent = {
        background = window.background;
        backgroundAlt = window.background;
        foreground = invertedColorScheme.red.dark;
        highlight = { foreground = invertedColorScheme.magenta.dark; inherit (window) background; };
      };
    };
  };
}
