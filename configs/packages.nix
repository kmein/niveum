{ config, pkgs, ... }:
with pkgs;
{
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides =
      let nix-writers = builtins.fetchGit {
        url = https://cgit.krebsco.de/nix-writers/;
        rev = "0660cc1a1169e799bda356c6fadb245a96345816";
      }; in import "${nix-writers}/pkgs" pkgs;
  };

  fonts.enableDefaultFonts = true;
  fonts.fonts = [
    corefonts
    eb-garamond
    fira
    font-awesome-ttf
    lmodern
    powerline-fonts
    roboto
  ];

  environment.systemPackages = [
    arandr
    blueman
    chromium
    config.constants.theme.gtk.package
    config.constants.theme.icon.package
    ffmpeg
    file
    firefox
    git
    gnumake
    gthumb
    htop
    imagemagick
    libnotify
    libreoffice-fresh
    lsof
    lxappearance
    mpv
    pamixer
    pavucontrol
    pmount
    ranger
    ripgrep tree
    rlwrap
    tor-browser-bundle-bin
    unzip
    w3m
    wget
    whois
    xclip
    xfce.ristretto
    xfce.terminal
    xfce.thunar
    xfce.tumbler
    xorg.xbacklight
    xorg.xcursorthemes
    xorg.xkill
    youtubeDL
    zathura
  ];

  programs.command-not-found.enable = true;
  programs.java.enable = true;

  users.users.kfm.packages = [
    (texlive.combine { inherit (pkgs.texlive) scheme-full latexmk; })
    audacity
    cabal-install
    cabal2nix
    calibre
    cloc
    clojure
    ctags
    dropbox-cli
    fsharp
    gcc
    ghc
    gnuplot
    grive2
    haskellPackages.ghcid
    haskellPackages.hakyll
    haskellPackages.hasktags
    haskellPackages.hindent
    haskellPackages.hoogle
    haskellPackages.pandoc
    haskellPackages.pandoc-citeproc
    idris
    inkscape
    jo
    lua
    maxima
    mypy
    nasm
    nix-prefetch-git
    nodejs
    ocaml
    par
    perl
    python3
    python36Packages.flake8
    racket-minimal
    ruby
    rustup
    scala
    seafile-client
    shellcheck
    spotify
    stack
    swiProlog
    tinycc
    zeroad
  ];
}
