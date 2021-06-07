{ config, lib, pkgs, ... }:
{
  home-manager.users.me = {
    services.flameshot.enable = true;

    xdg.configFile."flameshot/flameshot.ini".source = (pkgs.formats.ini {}).generate "flameshot.ini" {
      General = {
        disabledTrayIcon = true;
        checkForUpdates = false;
        contrastOpacity = 188;
        savePath = "/tmp";
        savePathFixed = true;
        drawThickness = 0;
        showStartupLaunchMessage = false;
        filenamePattern = "shot_%F_%T";
      };
    };

    systemd.user.services.flameshot.Unit.Requires = lib.mkForce [];
    systemd.user.services.flameshot.Environment = {
      # IMGUR_CREATE_URL = "https://p.krebsco.de/image";
      # IMGUR_DELETE_URL = "https://p.krebsco.de/image/delete/%1";
      PATH = "${config.home-manager.users.me.home.profileDirectory}/bin";
    };
  };
}
