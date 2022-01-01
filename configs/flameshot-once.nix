{ lib, pkgs, ... }:
let
  inherit (import <niveum/lib>) defaultApplications;
  flameshot-once = pkgs.callPackage <stockholm/krebs/5pkgs/simple/flameshot-once> {};
in {
  nixpkgs.overlays = [
    (self: super: {
      write =
        super.callPackage <stockholm/krebs/5pkgs/simple/xwaitforwindow.nix> { };
    })
  ];

  environment.systemPackages = [
    (flameshot-once.override {
      config = {
        imgur = {
          enable = true;
          createUrl = "http://p.r/image";
          deleteUrl = "http://p.r/image/delete/%1";
          xdg-open.browser = (defaultApplications pkgs).browser;
        };
        timeout = 1000;
        drawColor = "#ff0000";
        drawThickness = 2;
        showDesktopNotification = true;
        buttons = [
          "ARROW"
          "BLUR"
          "CIRCLE"
          "CIRCLECOUNT"
          "COPY"
          "DRAWER"
          "EXIT"
          "MARKER"
          "MOVESELECTION"
          "PENCIL"
          "RECTANGLE"
          "SAVE"
          "SELECTION"
          "SELECTIONINDICATOR"
          "TEXT"
          "UNDO"
        ];
      };
    })
  ];
}
