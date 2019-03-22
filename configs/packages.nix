{ config, pkgs, lib, ... }:
let
  bvg = pkgs.callPackage ../packages/bvg.nix {};
  daybook = pkgs.callPackage ../packages/daybook.nix {};
  iolanguage = pkgs.callPackage ../packages/iolanguage.nix {};
  sncli = pkgs.python3Packages.callPackage ../packages/sncli.nix {};
  todoist = pkgs.callPackage ../packages/todoist {};
  dic-pkg = pkgs.fetchurl {
    url = "https://cgit.krebsco.de/stockholm/plain/krebs/5pkgs/simple/dic/default.nix?id=8371e21c10bdb5d5353cc581efba7e09e4ce7a91";
    sha256 = "1vd8mg1ac7wzrcs5bl20srkxcs65zr7rd7y3wxzrxspij5wrb23i";
  };
  dic = pkgs.callPackage dic-pkg {};
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
    du-dust
    exa
    fd
    file
    git
    gitAndTools.hub
    gitAndTools.git-extras
    gitstats
    jq
    manpages
    moreutils
    patch
    patchutils
    posix_man_pages
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

  security.wrappers = {
    pmount.source = "${pkgs.pmount}/bin/pmount";
    pumount.source = "${pkgs.pmount}/bin/pumount";
  };

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
    (executables haskellPackages.patat)
    asciidoctor
    proselint
  ] ++ [ # programming
    vscode
    tokei
    gnumake
    cabal2nix
    chicken
    clojure
    gcc
    binutils-unwrapped
    (haskellPackages.ghcWithHoogle haskells)
    (executables haskellPackages.cabal-install)
    (executables haskellPackages.ghcid)
    (executables haskellPackages.hakyll)
    (executables haskellPackages.brittany)
    (executables haskellPackages.hfmt)
    (executables haskellPackages.hasktags)
    # (executables haskellPackages.hindent)
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
    python3Packages.black
    # python3Packages.yapf
    python3Packages.flake8
    python3Packages.jedi
    ruby
    rustup
    # rustracer
    scala
    shellcheck
  ] ++ [ # media
    audacity
    calibre
    inkscape
    xpdf
    pdfgrep
    pdftk
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
    bvg
    daybook
    gnupg
    jo
    memo
    par
    fzf
    pass
    qrencode
    sncli
    dic
    tmuxp
    unstable.hledger
    wordnet
    xsv
  ] ++ (if config.networking.hostName == "homeros" then [ unstable.zeroad ] else []);
}
