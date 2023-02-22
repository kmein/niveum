{
  lib,
  pkgs,
  ...
}: let
  inherit (import <niveum/lib>) defaultApplications;
  flameshot-once = pkgs.callPackage <stockholm/krebs/5pkgs/simple/flameshot-once> {};
in {
  home-manager.users.me = {
    services.flameshot = {
      enable = true;
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
        buttons = lib.concatStringsSep " " [
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
  };
}
