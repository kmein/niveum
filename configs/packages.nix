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
  yt-next-pkg = pkgs.fetchurl {
    url = http://cgit.lassul.us/stockholm/plain/lass/5pkgs/yt-next/default.nix;
    sha256 = "0j9r9xy34sl9ci5lz38060b3nakf0vd7gw46pykdiriwz6znbxn3";
  };
  urlencode-pkg = pkgs.fetchurl {
    url = http://cgit.lassul.us/stockholm/plain/krebs/5pkgs/simple/urlencode/default.nix;
    sha256 = "0cxf0fcaq02krkn33qipv878drqnjr035a564m66wp9x8n2zjgim";
  };
  acronym-pkg = pkgs.fetchurl {
    url = http://cgit.lassul.us/stockholm/plain/lass/5pkgs/acronym/default.nix;
    sha256 = "1rpr1rniz74vmkl4r3hgrg8q7ncxrvbf7zp0lq9b7lva85i12zx9";
  };
  urban-pkg = pkgs.fetchurl {
    url = http://cgit.lassul.us/stockholm/plain/lass/5pkgs/urban/default.nix;
    sha256 = "128v0znnapcqbyvc0nf112ddfyipr8sc1z4kcnggnbjf99i763ji";
  };
  mpv-poll-pkg = pkgs.fetchurl {
    url = http://cgit.lassul.us/stockholm/plain/lass/5pkgs/mpv-poll/default.nix;
    sha256 = "0ccmm7spxll98j8gy58fc3p8331arznshsj5wn4kkcypcs16n6ci";
  };
  dic = pkgs.callPackage dic-pkg {};
  yt-next = pkgs.callPackage yt-next-pkg {};
  acronym = pkgs.callPackage acronym-pkg {};
  urban = pkgs.callPackage urban-pkg {};
  mpv-poll = pkgs.callPackage mpv-poll-pkg {};
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
    (haskellPackages.brittany)
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
    nodePackages.csslint
    nodePackages.prettier
    nodePackages.jsonlint
    ocaml
    python3
    python3Packages.black
    python3Packages.python-language-server
    python3Packages.pyls-mypy
    python3Packages.flake8
    # python3Packages.jedi
    ruby
    rubocop
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
    (pass.withExtensions (ext: [ext.pass-otp]))
    qrencode
    sncli
    tmuxp
    unstable.hledger
    wordnet
    xsv
    dic
    yt-next
    mpv-poll
    acronym
    urban
  ] ++ (if config.networking.hostName == "homeros" then [ unstable.zeroad ] else []);
}
