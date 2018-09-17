{ pkgs, ... }:
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
    ffmpeg
    firefox firefoxPackages.tor-browser
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
    w3m
    wget
    whois
    xclip
    xfce.ristretto
    xfce.terminal
    xfce.thunar
    youtubeDL
    zathura
    zip unzip
  ];

  programs.command-not-found.enable = true;
  programs.java.enable = true;
  programs.light.enable = true;

  users.users.kfm.packages = [
    audacity
    biber
    calibre
    clojure
    ctags
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
    haskellPackages.idris
    haskellPackages.pandoc haskellPackages.pandoc-citeproc
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
    texlive.combined.scheme-tetex
    tinycc
  ];

}
