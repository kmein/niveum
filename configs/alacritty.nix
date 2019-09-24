{ pkgs, lib, config, ... }:
{
  niveum.applications.terminal = "alacritty";

  environment.systemPackages = [
    pkgs.alacritty
    pkgs.alacritty.terminfo
  ];

  home-manager.users.me.xdg.configFile."alacritty/alacritty.yml".text =
  let
    colourNames = [ "black" "red" "green" "yellow" "blue" "magenta" "cyan" "white" ];
    colours = lib.getAttrs colourNames config.niveum.colours;
  in builtins.toJSON {
    colors = {
      primary = {
        inherit (config.niveum.colours) background foreground;
        dim_foreground = config.niveum.colours.background;
      };
      normal = lib.mapAttrs (_: colour: colour.dark) colours;
      bright = lib.mapAttrs (_: colour: colour.bright) colours;
    };
    font = {
      normal.family = config.niveum.fonts.terminal.name;
      size = config.niveum.fonts.terminal.size - 2;
    };
    key_bindings = [
      { key = "Add"; mods = "Control"; action = "IncreaseFontSize"; }
      { key = "Minus"; mods = "Control"; action = "DecreaseFontSize"; }
    ];
  };
}
