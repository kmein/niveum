{
  lib,
  pkgs,
  ...
}:
{
  home-manager.users.me = {
    services.flameshot = {
      enable = true;
      settings.General = {
        autoCloseIdleDaemon = true;
        drawColor = "#ff0000";
        drawThickness = 2;
        showDesktopNotification = true;
        disabledTrayIcon = true;
        showHelp = false;
        squareMagnifier = true;
        uploadWithoutConfirmation = true;
        # buttons = ''@Variant(\0\0\0\x7f\0\0\0\vQList<int>\0\0\0\0\x10\0\0\0\x2\0\0\0\x5\0\0\0\x13\0\0\0\xa\0\0\0\x1\0\0\0\xc\0\0\0\xd\0\0\0\x6\0\0\0\x8\0\0\0\0\0\0\0\xf\0\0\0\x4\0\0\0\xb\0\0\0\x3\0\0\0\x12\0\0\0\x9)'';
      };
    };
  };
}
