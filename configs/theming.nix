{
  lib,
  config,
  pkgs,
  ...
}: let
  theme = (import <niveum/lib>).theme pkgs;
in {
  environment.systemPackages = [theme.gtk.package theme.icon.package theme.cursor.package];

  services.xserver.displayManager.lightdm.greeters.gtk = {
    theme = {inherit (theme.gtk) name package;};
    iconTheme = {inherit (theme.icon) name package;};
  };

  home-manager.users.me = {
    gtk = {
      enable = true;
      iconTheme = theme.icon;
      theme = theme.gtk;
    };
    qt = {
      enable = true;
      platformTheme = "gtk";
    };
    home.pointerCursor =
      theme.cursor
      // {
        size = 16;
        x11.enable = true;
      };
  };
}
