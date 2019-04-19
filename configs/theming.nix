{ lib, config, ... }:
with config.niveum; {
  i18n.consoleColors = map (c: lib.strings.removePrefix "#" c) colourPalette;

  environment.systemPackages = [
    theme.gtk.package
    theme.icon.package
    theme.cursor.package
  ];

  services.xserver.displayManager.lightdm.greeters.gtk = {
    theme = {
      name = theme.gtk.name;
      package = theme.gtk.package;
    };
    iconTheme = {
      name = theme.icon.name;
      package = theme.icon.package;
    };
  };

  home-manager.users.me = {
    gtk = {
      enable = true;
      iconTheme = theme.icon;
      theme = theme.gtk;
    };
    qt = {
      enable = true;
      useGtkTheme = true;
    };
    xsession.pointerCursor = theme.cursor // { size = 16; };
  };
}
