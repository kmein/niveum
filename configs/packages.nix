{ config, pkgs, ... }:
with pkgs;
{
  nixpkgs.config.allowUnfree = true;

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
    # libreoffice
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
    sxiv
    xorg.xbacklight
    xorg.xcursorthemes
    xorg.xkill
    wpa_supplicant_gui
    youtubeDL
    zathura
  ];

  programs.command-not-found.enable = true;
  programs.java = {
    enable = true;
    package = pkgs.openjdk;
  };

  users.users.kfm.packages = [
    (texlive.combine { inherit (pkgs.texlive) scheme-full latexmk; })
    audacity
    cabal-install
    cabal2nix
    calibre
    cloc
    clojure
    ctags
    dot2tex
    dropbox-cli
    fsharp
    gcc
    ghc
    gnuplot
    graphviz
    grive2
    haskellPackages.ghcid
    haskellPackages.hakyll
    haskellPackages.hasktags
    haskellPackages.hindent
    haskellPackages.hoogle
    haskellPackages.pandoc
    haskellPackages.pandoc-citeproc
    hlint
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
    python36Packages.black
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
