{ lib, pkgs, ... }:
let
  overrides = self: super: {
      flameshot-once = self.callPackage <stockholm/krebs/5pkgs/haskell/flameshot-once.nix> {};
      blessings = self.callPackage <stockholm/krebs/5pkgs/haskell/blessings.nix> {};
  };
  flameshot-once = pkgs.callPackage <stockholm/krebs/5pkgs/simple/flameshot-once> {};
in
{
  nixpkgs.overlays = [
    (import <stockholm/submodules/nix-writers/pkgs>)
    (import <stockholm/krebs/5pkgs/override>)
    (self: super: {
      haskell = super.haskell // {
        packages = lib.mapAttrs (name: value:
          if lib.hasAttr "override" value
            then value.override { inherit overrides; }
            else value
        ) super.haskell.packages;
      };
      haskellPackages = super.haskellPackages.override {
        inherit overrides;
      };
      xwaitforwindow = super.callPackage <stockholm/krebs/5pkgs/simple/xwaitforwindow.nix> {};
    })
  ];

  environment.systemPackages = [
    (flameshot-once.override {
      config.imgur.enable = true;
      config.imgur.createUrl = "http://p.r/image";
      config.imgur.deleteUrl = "http://p.r/image/delete/%1";
      config.imgur.xdg-open.browser = "x-www-browser";
      config.timeout = 200;
    })
  ];
}
