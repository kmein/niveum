{
  pkgs,
  lib,
  config,
  ...
}: let
  alacritty-cfg = theme:
    (pkgs.formats.yaml {}).generate "alacritty.yml" {
      window.opacity = 0.99;
      bell = {
        animation = "EaseOut";
        duration = 100;
        color = "#ffffff";
      };
      font = {
        normal.family = "Monospace";
        size = 6;
      };
      live_config_reload = true;
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
      colors = let
        colourNames = ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"];
        colourPairs = lib.getAttrs colourNames theme;
      in {
        primary = {inherit (theme) background foreground;};
        cursor = {inherit (theme) cursor;};
        normal = lib.mapAttrs (_: colour: colour.dark) colourPairs;
        bright = lib.mapAttrs (_: colour: colour.bright) colourPairs;
      };
    };
  alacritty-pkg = pkgs.symlinkJoin {
    name = "alacritty";
    paths = [
      (pkgs.writers.writeDashBin "alacritty" ''
        ${pkgs.alacritty}/bin/alacritty --config-file /var/theme/config/alacritty.yml msg create-window "$@" ||
        ${pkgs.alacritty}/bin/alacritty --config-file /var/theme/config/alacritty.yml "$@"
      '')
      pkgs.alacritty
    ];
  };
in {
  environment.variables.TERMINAL = "alacritty";

  environment.systemPackages = [
    alacritty-pkg
  ];

  environment.etc = {
    "themes/dark/alacritty.yml".source = alacritty-cfg (import ../lib/colours/owickstrom-dark.nix);
    "themes/light/alacritty.yml".source = alacritty-cfg (import ../lib/colours/owickstrom-light.nix);
  };
}
