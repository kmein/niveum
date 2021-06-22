{ lib, pkgs, ... }:
let
  krebs-packages = pkgs: {
    dic = pkgs.callPackage <stockholm/krebs/5pkgs/simple/dic> { };
    yt-next = pkgs.callPackage <stockholm/lass/5pkgs/yt-next> { };
    acronym = pkgs.callPackage <stockholm/lass/5pkgs/acronym> { };
    urban = pkgs.callPackage <stockholm/lass/5pkgs/urban> { };
    mpv-poll = pkgs.callPackage <stockholm/lass/5pkgs/mpv-poll> { };
    untilport = pkgs.callPackage <stockholm/krebs/5pkgs/simple/untilport> { };
    kpaste = pkgs.callPackage <stockholm/krebs/5pkgs/simple/kpaste> { };
    irc-announce =
      pkgs.callPackage <stockholm/krebs/5pkgs/simple/irc-announce> { };
    git-preview =
      pkgs.callPackage <stockholm/krebs/5pkgs/simple/git-preview> { };
  };
in {
  nixpkgs.config.packageOverrides = krebs-packages;

  environment.systemPackages =
    map (name: pkgs.${name}) (lib.attrNames (krebs-packages pkgs));
}
