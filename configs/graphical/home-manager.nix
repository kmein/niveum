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

  programs.swaylock = {
    enable = true;
    settings = {
      daemonize = true;
      ignore-empty-password = true;
    };
  };
  # stylix skips swaylock on stateVersion < 23.05
  stylix.targets.swaylock.enable = true;

  gtk = {
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
    };
    # gtk4.theme = config.home-manager.users.me.gtk.theme;
  };
}
