{ config, ... }:
let theme = import <dot/theme.nix>;
in {
  home-manager.users.me.programs.rofi = {
    enable = true;
    separator = "solid";
    scrollbar = false;
    terminal = config.niveum.applications.terminal;
    borderWidth = 0;
    lines = 5;
    font = "${theme.terminalFont.name} ${toString (theme.terminalFont.size + 1)}";
    colors = rec {
      window = {
        background = theme.invertedColorScheme.background;
        border = theme.invertedColorScheme.background;
        separator = theme.invertedColorScheme.black.light;
      };
      rows = {
        normal = {
          background = window.background;
          backgroundAlt = window.background;
          foreground = theme.invertedColorScheme.foreground;
          highlight = { foreground = theme.invertedColorScheme.cyan.dark; inherit (window) background; };
        };
        active = {
          background = window.background;
          backgroundAlt = window.background;
          foreground = theme.invertedColorScheme.yellow.dark;
          highlight = { foreground = theme.invertedColorScheme.green.dark; inherit (window) background; };
        };
        urgent = {
          background = window.background;
          backgroundAlt = window.background;
          foreground = theme.invertedColorScheme.red.dark;
          highlight = { foreground = theme.invertedColorScheme.magenta.dark; inherit (window) background; };
        };
      };
    };
  };
}
