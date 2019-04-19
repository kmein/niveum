{ lib, config, ... }:
{
  i18n.consoleColors = with import <dot/theme.nix>; map (c: lib.strings.removePrefix "#" c) colorPalette;

  environment.systemPackages = [
    config.niveum.theme.gtk.package
    config.niveum.theme.icon.package
    config.niveum.theme.cursor.package
  ];


  services.xserver.displayManager.lightdm.greeters.gtk = {
    theme = {
      name = config.niveum.theme.gtk.name;
      package = config.niveum.theme.gtk.package;
    };
    iconTheme = {
      name = config.niveum.theme.icon.name;
      package = config.niveum.theme.icon.package;
    };
  };

  home-manager.users.me = {
    gtk = {
      enable = true;
      iconTheme = config.niveum.theme.icon;
      theme = config.niveum.theme.gtk;
    };
    qt = {
      enable = true;
      useGtkTheme = true;
    };
    xsession.pointerCursor = config.niveum.theme.cursor // { size = 16; };
  };
}
