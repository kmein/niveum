{ lib, pkgs, ... }:
let
  flameshot-once =
    pkgs.callPackage <stockholm/krebs/5pkgs/simple/flameshot-once> {};
in {
  nixpkgs.overlays = [
    (self: super: {
      xwaitforwindow =
        super.callPackage <stockholm/krebs/5pkgs/simple/xwaitforwindow.nix> { };
    })
  ];

  environment.systemPackages = [
    (flameshot-once.override {
      config.imgur = {
        enable = true;
        createUrl = "http://p.r/image";
        deleteUrl = "http://p.r/image/delete/%1";
        xdg-open.browser = "x-www-browser";
      };
      config.timeout = 200;
    })
  ];
}
