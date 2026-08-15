{
  lib,
  pkgs,
  config,
  ...
}:
{
  # waybar has no notification module (ashell, which it replaced, did), so the
  # notification daemon is mako again; stylix themes it to match the bar
  services.mako = {
    enable = true;
    settings.default-timeout = 10 * 1000;
  };

  # transit indicator in the bar's tray (ephemeris-tray, from the
  # ephemeris-service flake): forks the service for a wide sweep,
  # caches, pops a native dbusmenu
  systemd.user.services.astro-tray = {
    Unit = {
      Description = "astrological transit tray indicator";
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = lib.getExe pkgs.ephemeris-tray;
      Restart = "on-failure";
      Environment = [
        # natal datetime for transits (UTC)
        "ASTRO_NATAL=1999-10-22T04:32:00Z"
        "ASTRO_SERVICE_BIN=${lib.getExe pkgs.ephemeris-service}"
      ];
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };

  services.hypridle = {
    enable = true;
    settings = {
      general = {
        after_sleep_cmd = "hyprctl dispatch dpms on";
        ignore_dbus_inhibit = false;
        lock_cmd = "hyprlock";
      };
      listener = [
        {
          timeout = 900;
          on-timeout = "hyprlock";
        }
        {
          timeout = 1200;
          on-timeout = "hyprctl dispatch dpms off";
          on-resume = "hyprctl dispatch dpms on";
        }
      ];
    };
  };

  programs.hyprlock = {
    enable = true;
    settings = {
      animations.enabled = false;
      general = {
        hide_cursor = true;
        ignore_empty_input = true;
      };
      # On hosts with a fingerprint reader (fatteh), unlock via fprintd's D-Bus
      # API. hyprlock runs this backend in parallel with the password prompt, so
      # typing your password unlocks instantly instead of waiting on a swipe.
      auth.fingerprint.enabled = config.services.fprintd.enable;
    };
  };

  gtk = {
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
    };
    # gtk4.theme = config.home-manager.users.me.gtk.theme;
  };
}
