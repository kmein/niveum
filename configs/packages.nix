{ config, pkgs, ... }:
with pkgs;
{
  fonts.fonts = [
    eb-garamond
    fira-code
    font-awesome-ttf
    hasklig
    lmodern
    powerline-fonts
    roboto
  ];

  environment.systemPackages = [
    chromium
    config.constants.theme.gtk.package
    config.constants.theme.icon.package
    ffmpeg
    firefox
    git
    gnumake
    htop
    imagemagick
    libnotify
    libreoffice-fresh
    lxappearance
    mpv
    pamixer
    pmount
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
    zip
  ];

  programs.command-not-found.enable = true;
  programs.java.enable = true;

  users.users.kfm.packages = [
    (texlive.combine { inherit (pkgs.texlive) scheme-full latexmk; })
    audacity
    biber
    cabal-install
    cabal2nix
    calibre
    clojure
    ctags
    dropbox-cli
    franz
    fsharp
    gcc
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
    nodejs
    ocaml
    par
    perl
    python3
    racket-minimal
    ruby
    rustup
    scala
    spotify
    stack
    swiProlog
    tinycc
  ];

}
