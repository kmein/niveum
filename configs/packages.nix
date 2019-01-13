{ config, pkgs, lib, ... }:
let
  scripts = import ../dot/scripts.nix { inherit pkgs lib; };
  daybook = pkgs.callPackage ../packages/daybook.nix {};
  todoist = pkgs.callPackage ../packages/todoist {};
  unstable = import <nixos-unstable> {};
in with pkgs;
{
  nixpkgs.config.allowUnfree = true;

  fonts.enableDefaultFonts = true;
  fonts.fonts = [
    cantarell-fonts
    corefonts
    eb-garamond
    fira
    libertine
    lmodern
    noto-fonts
    powerline-fonts
    roboto
    xlibs.fontschumachermisc
    ubuntu_font_family
  ];

  environment.systemPackages = [
  ] ++ [ # office
    abiword
    gnumeric
    typora
  ] ++ [ # theme
    config.constants.theme.gtk.package
    config.constants.theme.icon.package
    config.constants.theme.cursor.package
  ] ++ [ # internet
    aria2
    chromium
    firefox
    tor-browser-bundle-bin
    w3m
    wget
    whois
  ] ++ [ # media
    ffmpeg
    mpv
    pamixer
    pavucontrol
    gthumb
    imagemagick
    sxiv
    blueman
    zathura
  ] ++ [ # archive
    unzip
    unrar
    p7zip
  ] ++ [ # monitor
    htop
    iotop
    iftop
    lsof
    psmisc
  ] ++ [ # shell
    bat
    dos2unix
    file
    git
    manpages
    posix_man_pages
    most
    ranger
    ripgrep
    rlwrap
    tree
  ] ++ [ # hardware
    pmount
    usbutils
    pciutils
  ] ++ [ # graphical
    arandr
    libnotify
    xclip
    xorg.xkill
    wpa_supplicant_gui
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
  ] ++ [ # typesetting
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
      fdsymbol
      moderncv
      xstring;
    })
    pandoc
    haskellPackages.pandoc-citeproc
    asciidoctor
  ] ++ [ # programming
    cloc
    gnumake
    cabal2nix
    clojure
    gcc
    ghc
    haskellPackages.ghcid
    haskellPackages.hakyll
    haskellPackages.hasktags
    haskellPackages.hindent
    haskellPackages.hoogle
    hlint
    lua
    mypy
    nix-prefetch-git
    nodejs
    ocaml
    python3
    python36Packages.black
    python36Packages.flake8
    racket-minimal
    rustup
    scala
    shellcheck
    stack
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
    daybook
    # todoist
    unstable.hledger
    jo
    jq
    memo
    par
  ];
}
