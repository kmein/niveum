{ pkgs, lib, config, ... }:
let
  inherit (import <niveum/lib>) colours;
  colourNames =
    [ "black" "red" "green" "yellow" "blue" "magenta" "cyan" "white" ];
  colourPairs = lib.getAttrs colourNames colours;
  alacrittyConfig = {
    background_opacity = 0.9;
    colors = {
      primary = { inherit (colours) background foreground; };
      normal = lib.mapAttrs (_: colour: colour.dark) colourPairs;
      bright = lib.mapAttrs (_: colour: colour.bright) colourPairs;
    };
    font = {
      normal.family = "Monospace";
      size = 8;
    };
    key_bindings = [
      {
        key = "Add";
        mods = "Control";
        action = "IncreaseFontSize";
      }
      {
        key = "Minus";
        mods = "Control";
        action = "DecreaseFontSize";
      }
      {
        key = "Key0";
        mods = "Control";
        action = "ResetFontSize";
      }
    ];
  };
in {
  environment.variables.TERMINAL = "alacritty";

  environment.systemPackages = [
    pkgs.alacritty
  ];

  home-manager.users.me.xdg.configFile = {
    "alacritty/alacritty.yml".text = builtins.toJSON alacrittyConfig;
  };
}
