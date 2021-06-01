{ pkgs, lib, config, ... }:
{
  environment.variables.TERMINAL = "alacritty";

  environment.systemPackages = [
    pkgs.alacritty
  ];

  home-manager.users.me.xdg.configFile =
  let
    inherit (import <niveum/lib>) colours;
    colourNames = [ "black" "red" "green" "yellow" "blue" "magenta" "cyan" "white" ];
    colourPairs = lib.getAttrs colourNames colours;
  in {
    "alacritty/alacritty.yml".source = (pkgs.formats.yaml {}).generate "alacritty.yml" {
      background_opacity = 0.9;
      colors = {
        primary = { inherit (colours) background foreground; };
        normal = lib.mapAttrs (_: colour: colour.dark) colourPairs;
        bright = lib.mapAttrs (_: colour: colour.bright) colourPairs;
      };
      font = {
        normal.family = "Monospace";
        size = 6;
      };
      key_bindings = [
        {
          key = "Plus";
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
  };
}
