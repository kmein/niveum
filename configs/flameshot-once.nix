{ config, lib, pkgs, ... }:
{
  home-manager.users.me = {
    services.flameshot.enable = true;

    home.file.".config/Dharkael/flameshot.ini".source = (pkgs.formats.ini {}).generate "flameshot.ini" {
      General = {
        disabledTrayIcon = true;
        drawColor = ''@Variant(\0\0\0\x43\x1\xff\xff\0\0\0\0\xff\xff\0\0)'';
        drawThickness = 0;
        filenamePattern = "shot_%F_%T";
      };
    };

    systemd.user.services.flameshot.Unit.Requires = lib.mkForce [];
    systemd.user.services.flameshot.Environment = {
      IMGUR_CREATE_URL = "https://p.krebsco.de/image";
      IMGUR_DELETE_URL = "https://p.krebsco.de/image/delete/%1";
      PATH = "${config.home-manager.users.me.home.profileDirectory}/bin";
    };
  };
}
