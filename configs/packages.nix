{ config, pkgs, lib, ... }:
let
  daybook = pkgs.callPackage ../packages/daybook.nix {};
  iolanguage = pkgs.callPackage ../packages/iolanguage.nix {};
  todoist = pkgs.callPackage ../packages/todoist {};
  haskells = import ../dot/haskells.nix;
  unstable = import <nixos-unstable> {};
  executables = pkgs.haskell.lib.justStaticExecutables;
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
    # typora
  ] ++ [ # theme
    config.constants.theme.gtk.package
    config.constants.theme.icon.package
    config.constants.theme.cursor.package
  ] ++ [ # internet
    aria2
    chromium
    firefox
    tor-browser-bundle-bin
    thunderbird
    w3m
    wget
    httpie
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
    zip
  ] ++ [ # monitor
    htop
    iotop
    iftop
    lsof
    psmisc
  ] ++ [ # shell
    bat
    dos2unix
    fd
    file
    git
    gitAndTools.hub
    gitstats
    manpages
    patch
    patchutils
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

  users.users.kfm.packages = [
  ] ++ [ # typesetting
    (texlive.combine {
      inherit (pkgs.texlive) scheme-full texdoc latex2e-help-texinfo;
      pkgFilter = pkg: pkg.tlType == "run" || pkg.tlType == "bin" || pkg.pname == "latex2e-help-texinfo";
    })
    pandoc
    (executables haskellPackages.pandoc-citeproc)
    asciidoctor
  ] ++ [ # programming
    tokei
    gnumake
    cabal2nix
    chicken
    clojure
    gcc
    (haskellPackages.ghcWithHoogle haskells)
    (executables haskellPackages.cabal-install)
    (executables haskellPackages.ghcid)
    (executables haskellPackages.hakyll)
    (executables haskellPackages.hasktags)
    (executables haskellPackages.hindent)
    (executables haskellPackages.pointfree)
    (executables haskellPackages.pointful)
    (executables haskellPackages.hlint)
    (executables haskellPackages.hpack)
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
    jo
    jq
    memo
    par
    qrencode
    unstable.hledger
    wordnet
    xsv
  ];
}
