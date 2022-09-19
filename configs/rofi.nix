{
  config,
  pkgs,
  ...
}: let
  inherit (import <niveum/lib>) colours;
in {
  home-manager.users.me.programs.rofi = {
    enable = true;
    font = "Monospace 10";
    theme = "${pkgs.rofi}/share/rofi/themes/Arc.rasi";
  };
}
