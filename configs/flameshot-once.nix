{
  lib,
  pkgs,
  ...
}: let
  inherit (import <niveum/lib>) defaultApplications;
  flameshot-once = pkgs.callPackage <stockholm/krebs/5pkgs/simple/flameshot-once> {};
in {
  environment.systemPackages = [
    (flameshot-once.override {
      name = "flameshot-once-kmein";
      config = {
        imgur = {
          enable = true;
          createUrl = "http://p.r/image";
          deleteUrl = "http://p.r/image/delete/%1";
          xdg-open.browser = (defaultApplications pkgs).browser;
        };
        settings.General = {
          autoCloseIdleDaemon = true;
          drawColor = "#ff0000";
          drawThickness = 2;
          checkForUpdates = false;
          showDesktopNotification = true;
          disabledTrayIcon = true;
          showHelp = false;
          squareMagnifier = true;
          uploadWithoutConfirmation = true;
          buttons = [
            "TYPE_ARROW"
            "TYPE_CIRCLE"
            "TYPE_CIRCLECOUNT"
            "TYPE_COPY"
            "TYPE_DRAWER"
            "TYPE_EXIT"
            "TYPE_IMAGEUPLOADER"
            "TYPE_MARKER"
            "TYPE_MOVESELECTION"
            "TYPE_PENCIL"
            "TYPE_PIXELATE"
            "TYPE_RECTANGLE"
            "TYPE_SAVE"
            "TYPE_SELECTION"
            # "TYPE_SELECTIONINDICATOR"
            "TYPE_TEXT"
            "TYPE_UNDO"
          ];
        };
      };
    })
  ];
}
