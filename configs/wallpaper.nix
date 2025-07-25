{
  pkgs,
  lib,
  ...
}: let
  # url = "http://wallpaper.r/realwallpaper-krebs-stars-berlin.png";
  url = "http://wallpaper.r/realwallpaper-krebs.png";
  stateDir = "~/.cache/wallpaper";
in {
  systemd.user.services.wallpaper = {
    wantedBy = ["graphical-session.target"];
    after = ["network.target"];
    script = ''
      set -euf

      mkdir -p ${stateDir}
      chmod o+rx ${stateDir}
      cd ${stateDir}
      (${pkgs.curl}/bin/curl -s -o wallpaper.tmp -z wallpaper.tmp ${lib.escapeShellArg url} && cp wallpaper.tmp wallpaper) || :
      if [ -z $SWAYSOCK ]; then
        ${pkgs.feh}/bin/feh --no-fehbg --bg-scale wallpaper
      else
        ${pkgs.sway}/bin/swaymsg -s $SWAYSOCK 'output * bg ${stateDir}/wallpaper fill'
      fi
    '';
    startAt = "*:00,10,20,30,40,50";
    serviceConfig = {
      Restart = "always";
      RestartSec = "15s";
      StartLimitBurst = 0;
    };
  };
}
