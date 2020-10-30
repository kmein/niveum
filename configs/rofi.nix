{ config, ... }:
let
  inherit (import <niveum/lib>) colours;
in{
  home-manager.users.me.programs.rofi = {
    enable = true;
    separator = "solid";
    scrollbar = false;
    borderWidth = 0;
    lines = 5;
    font = "Monospace 10";
    colors = rec {
      window = rec {
        background = colours.foreground;
        border = background;
        separator = colours.black.bright;
      };
      rows = {
        normal = {
          background = window.background;
          backgroundAlt = window.background;
          foreground = colours.background;
          highlight = {
            foreground = colours.cyan.dark;
            inherit (window) background;
          };
        };
        active = {
          background = window.background;
          backgroundAlt = window.background;
          foreground = colours.yellow.dark;
          highlight = {
            foreground = colours.green.dark;
            inherit (window) background;
          };
        };
        urgent = {
          background = window.background;
          backgroundAlt = window.background;
          foreground = colours.red.dark;
          highlight = {
            foreground = colours.magenta.dark;
            inherit (window) background;
          };
        };
      };
    };
  };
}
