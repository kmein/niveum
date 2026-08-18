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
