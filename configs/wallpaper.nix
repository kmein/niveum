{
  pkgs,
  lib,
  ...
}:
let
  # url = "http://wallpaper.r/realwallpaper-krebs-stars-berlin.png";
  url = "http://wallpaper.r/realwallpaper-krebs.png";
  stateDir = "~/.cache/wallpaper";
in
{
  systemd.user.services.wallpaper = {
    wantedBy = [ "graphical-session.target" ];
    after = [ "network.target" ];
    path = [
      pkgs.curl
      pkgs.hyprland
    ];
    script = ''
      set -euf

      mkdir -p ${stateDir}
      chmod o+rx ${stateDir}
      cd ${stateDir}
      (${pkgs.curl}/bin/curl -s -o wallpaper.tmp -z wallpaper.tmp ${lib.escapeShellArg url} && cp wallpaper.tmp wallpaper.png) || :
      hyprctl hyprpaper preload ${stateDir}/wallpaper.png
      hyprctl hyprpaper wallpaper ",${stateDir}/wallpaper.png"
      sleep 5
      hyprctl hyprpaper unload unused
      true
    '';
    startAt = "*:00,10,20,30,40,50";
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "15s";
      StartLimitBurst = 0;
    };
  };
}
