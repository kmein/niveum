{ pkgs, ... }:
{
  bvg = pkgs.callPackage ./bvg.nix {};
  daybook = pkgs.callPackage ./daybook.nix {};
  depp = pkgs.callPackage ./depp.nix {};
  dirmir = pkgs.callPackage ./dirmir.nix {};
  favicon = pkgs.callPackage ./favicon.nix {};
  genius = pkgs.callPackage ./genius.nix {};
  instaget = pkgs.callPackage ./instaget.nix {};
  literature-quote = pkgs.callPackage ./literature-quote.nix {};
  man-pdf = pkgs.callPackage ./man-pdf.nix {};
  n = pkgs.callPackage ./n.nix {};
  nav = pkgs.callPackage ./nav.nix {};
  odyssey = pkgs.callPackage ./odyssey.nix {};
  tolino-screensaver = pkgs.callPackage ./tolino-screensaver.nix {};
  wttr = pkgs.callPackage ./wttr.nix {};
  slide =
    let slide-package = pkgs.fetchFromGitHub {
      owner = "kmein";
      repo = "slide";
      rev = "0470583d22212745eab4f46076267addf4d2346c";
      sha256 = "0skcp3va9v4hmxy5ramghpz53gnyxv10wsacgmc2jr0v1wrqlzbh";
    };
    in pkgs.callPackage slide-package {};
  mnemosyne =
    let mnemosyne-package = pkgs.fetchFromGitHub {
      repo = "mnemosyne";
      owner = "kmein";
      rev = "6bfa13c88db176af80be90840ff03573d67d679f";
      sha256 = "1rimv5c5q9602y501hbkgkfbimqnmdkcr5hp1434q06gcazhjhca";
    };
    in pkgs.haskellPackages.callPackage mnemosyne-package {};
}
