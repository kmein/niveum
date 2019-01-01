{ config, pkgs, lib, ... }:
let scripts = import ../dot/scripts.nix { inherit pkgs lib; };
in with pkgs;
{
  nixpkgs.config.allowUnfree = true;

  fonts.enableDefaultFonts = true;
  fonts.fonts = [
    corefonts
    xlibs.fontschumachermisc
    eb-garamond
    fira
    font-awesome-ttf
    libertine
    lmodern
    powerline-fonts
    roboto
  ];

  environment.systemPackages = [
    abiword
    arandr
    bat
    blueman
    chromium
    config.constants.theme.gtk.package
    config.constants.theme.icon.package
    config.constants.theme.cursor.package
    dos2unix
    ffmpeg
    file
    firefox
    git
    gnumake
    gnumeric
    gthumb
    htop
    imagemagick
    libnotify
    lsof
    lxappearance
    mpv
    pamixer
    pavucontrol
    pmount
    ranger
    ripgrep
    tree
    rlwrap
    tor-browser-bundle-bin
    unzip
    w3m
    wget
    whois
    xclip
    sxiv
    xorg.xkill
    wpa_supplicant_gui
    zathura
  ];

  programs.command-not-found.enable = true;
  programs.java = {
    enable = true;
    package = pkgs.openjdk;
  };
  virtualisation.docker.enable = true;
  services.urxvtd.enable = true;
  services.dbus.packages = [ pkgs.gnome3.dconf ];

  users.users.kfm.packages = scripts ++ [
    (texlive.combine { inherit (pkgs.texlive)
      scheme-tetex
      latexmk
      biblatex
      comment
      csquotes
      enumitem
      fontaxes
      ifnextok
      imakeidx
      hardwrap
      titlesec
      libertine
      logreq
      marginnote
      mweights
      realscripts
      pbox
      stdclsdv
      xstring;
    })
    (callPackage ../packages/daybook {})
    audacity
    cabal-install
    cabal2nix
    calibre
    cloc
    clojure
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
    pandoc
    haskellPackages.pandoc-citeproc
    hlint
    inkscape
    jo jq
    lua
    maxima
    memo
    mypy
    nix-prefetch-git
    nodejs
    ocaml
    par
    python3
    python36Packages.black
    python36Packages.flake8
    racket-minimal
    rustup
    scala
    seafile-client
    shellcheck
    youtubeDL
    spotify
    stack
    # zeroad
  ];
}
