{ config, lib, pkgs, ... }:
let
  executables = pkgs.haskell.lib.justStaticExecutables;
  unstable = import <nixos-unstable> {};
  todoist = pkgs.callPackage ../packages/todoist {};
  daybook = pkgs.callPackage ../packages/daybook.nix {};
  iolanguage = pkgs.callPackage ../packages/iolanguage.nix {};
in {
  imports = [
    ../configs/base
    ../configs/docker.nix
    ../configs/google-drive.nix
    ../configs/graphical.nix
    ../configs/haskell.nix
    ../configs/hu-berlin.nix
    ../configs/retiolum.nix
    ../configs/wifi.nix
  ];

  users.users.kfm.packages = with pkgs; [
  ] ++ [ # typesetting
    (texlive.combine {
      inherit (pkgs.texlive) scheme-full texdoc latex2e-help-texinfo;
      pkgFilter = pkg: pkg.tlType == "run" || pkg.tlType == "bin" || pkg.pname == "latex2e-help-texinfo";
    })
    pandoc
    (executables haskellPackages.pandoc-citeproc)
    (executables haskellPackages.hakyll)
    asciidoctor
  ] ++ [ # programming
    tokei
    gnumake
    cabal2nix
    chicken
    clojure
    gcc
    htmlTidy
    iolanguage
    lua
    mypy
    nix-prefetch-git
    nodejs
    nodePackages.eslint
    nodePackages.csslint
    nodePackages.prettier
    ocaml
    python3
    python36Packages.black
    python36Packages.flake8
    ruby
    rustup
    scala
    shellcheck
  ] ++ [ # media
    audacity
    calibre
    inkscape
    poppler_utils
    spotify
    youtubeDL
  ] ++ [ # cloud
    dropbox-cli
    grive2
    seafile-client
  ] ++ [ # math
    bc
    graphviz
    maxima
  ] ++ [ # shell
    # todoist
    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.la
    daybook
    memo
    qrencode
    unstable.hledger
    wordnet
    xsv
  ];
}
