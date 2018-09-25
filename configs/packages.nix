{ config, pkgs, ... }:
with pkgs;
{
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides =
      let nix-writers = builtins.fetchGit {
        url = https://cgit.krebsco.de/nix-writers/;
        ref = "tags/v3.0.0";
        # sha256 = "066y18q19d35x5jjr3kdn1dwi7s1l12icr90s2vxwzif6ahnzmb3";
      }; in import "${nix-writers}/pkgs" pkgs;
  };

  fonts.enableDefaultFonts = true;
  fonts.fonts = [
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
    firefox
    git
    gnumake
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
    zeroad
  ];

}
