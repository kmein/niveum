{ pkgs, lib, ... }:
let
  hc = pkgs.callPackage <stockholm/tv/5pkgs/simple/hc.nix> {};
  worldradio = pkgs.callPackage <niveum/packages/worldradio.nix> {};
  menstruation = pkgs.callPackage <menstruation-backend> {};
  pandoc-doc = pkgs.callPackage <niveum/packages/man/pandoc.nix> {};

  nixpkgs-unstable = import <nixpkgs-unstable> { config.allowUnfree = true; };

  zoteroStyle = { name, sha256 }: {
    name = "${name}.csl";
    path = pkgs.fetchurl {
      url = "https://www.zotero.org/styles/${name}";
      inherit sha256;
    };
  };
  cslDirectory = pkgs.linkFarm "citation-styles" [
    (zoteroStyle {
      name = "chicago-author-date-de";
      sha256 = "0fz0xn46rkciblr34a7x2v60j0lbq9l3fmzi43iphph27m0czn6s";
    })
    (zoteroStyle {
      name = "din-1505-2";
      sha256 = "1pvy1b7qm13mnph7z365rrz1j082bl2y8ih73rhzd0zd6dz1jyjq";
    })
    (zoteroStyle {
      name = "apa";
      sha256 = "1878vxp0y0h05yzaghnd51n981623mxskw3lsdyzmffqhihvv111";
    })
  ];


  astrolog = nixpkgs-unstable.astrolog.overrideAttrs (old: old // {
    installPhase = ''
      ${old.installPhase}
      # set sensible defaults
      sed -i '
        /^-z /s/8:00W/1:00E/ # timezone
        /^-zl /s/122W19:59 47N36:35/13E22:42 52N27:42/ # default location
        /^-zj /s/"Current moment now"/Now/ # default name
        /^-zj /s/"Seattle, WA, USA"/Berlin/ # default location
        /^_k/s/_k/=k/ # use color
        /^_Yd/s/_Yd/=Yd/ # sensible date format
        /^_Yt/s/_Yt/=Yt/ # sensible time format
        /^_Yv/s/_Yv/=Yv/ # sensible length format
        /^:Xbw/s/:Xbw/:Xbn/ # set X11 bitmap format
        /^:I /s/80/120/ # wider text output
      ' $out/astrolog/astrolog.as
    '';
  });

  recht = pkgs.callPackage <recht> {};

in {
  home-manager.users.me.home.file = {
    ".csl".source = cslDirectory;
    ".local/share/pandoc/csl".source = cslDirectory; # as of pandoc 2.11, it includes citeproc
  };

  environment.systemPackages = with pkgs; [
    # INTERNET
    aria2
    firefox
    tdesktop
    w3m
    wget
    whois
    dnsutils
    # FILE MANAGERS
    ranger
    pcmanfm
    # MEDIA
    ffmpeg
    imagemagick
    exiftool
    # ARCHIVE TOOLS
    unzip
    unrar
    p7zip
    zip
    # MONITORS
    htop
    iotop # I/O load monitor
    iftop # interface bandwidth monitor
    lsof # list open files
    psmisc # for killall, pstree
    # SHELL
    bat # better cat
    fd # better find
    file # determine file type
    dos2unix
    ncdu # ncurses disk usage
    python3Packages.jsonschema # json validation
    jq # json toolkit
    pup # html toolkit
    nixpkgs-unstable.htmlq
    xsv # csv toolkit
    xmlstarlet # xml toolkit
    manpages
    posix_man_pages
    tree
    fuse_exfat # to mount windows drives
    parallel # for parallel, since moreutils shadows task spooler
    ripgrep # better grep
    rlwrap
    progress # display progress bars for pipes
    # HARDWARE TOOLS
    usbutils # for lsusb
    pciutils # for lspci
    lshw # for lshw
    arandr # xrandr for noobs
    libnotify # for notify-send
    xclip # clipboard CLI
    dragon-drop # drag and drop
    xorg.xkill # kill by clicking
    audacity
    calibre
    inkscape
    astrolog
    anki # flashcards
    nixpkgs-unstable.zoom-us # video conferencing
    pdfgrep # search in pdf
    pdftk # pdf toolkit
    mupdf
    poppler_utils # pdf toolkit
    foxitreader # for viewing pdf annotations
    xournalpp # for annotating pdfs
    pdfpc # presenter console for pdf slides
    hc # print files as qr codes
    youtubeDL
    espeak
    bc # calculator
    pari # gp -- better calculator
    scripts.auc
    scripts.infschmv
    scripts.qrpaste
    scripts.ttspaste
    scripts.new-mac # get a new mac address
    scripts.scanned
    scripts.default-gateway
    scripts.showkeys-toggle
    scripts.kirciuoklis
    scripts.favicon
    scripts.ipa # XSAMPA to IPA converter
    scripts.playlist
    scripts.mpv-tv
    scripts.devanagari
    scripts.betacode # ancient greek betacode to unicode converter
    scripts.meteo
    scripts.mahlzeit
    recht
    scripts.vimv
    scripts.swallow # window swallowing
    scripts.literature-quote
    scripts.nav # json navigation
    scripts.notetags
    scripts.booksplit
    scripts.dmenurandr
    scripts.interdimensional-cable
    scripts.dmenubluetooth
    scripts.manual-sort
    scripts.much-scripts
    scripts.dns-sledgehammer
    ts
    scripts.vg
    scripts.fkill
    scripts.wttr
    scripts.sanskrit-dictionary
    scripts.unicodmenu
    scripts.horoscope
    scripts.closest
    scripts.trans
    scripts.liddel-scott-jones
    scripts.mpv-radio
    # kmein.slide
    scripts.tolino-screensaver
    scripts.rfc
    scripts.tag
    python3Packages.eyeD3
    scripts.menu-calc
    nix-prefetch-git
    scripts.nix-git
    nixfmt
    par
    qrencode

    menstruation

    (pkgs.writers.writeDashBin "worldradio" ''
      shuf ${worldradio} | ${pkgs.findutils}/bin/xargs ${pkgs.mpv}/bin/mpv --no-video
    '')

    (pkgs.writers.writeDashBin "chats" ''
      ${pkgs.openssh}/bin/ssh makanek "cd /var/lib/weechat/logs && grep --ignore-case --color=always --recursive $@" | ${pkgs.less}/bin/less --raw-control-chars
    '')

    (pkgs.writers.writeDashBin "ncmpcpp-zaatar" ''MPD_HOST=${(import <niveum/lib/local-network.nix>).zaatar} exec ${pkgs.ncmpcpp}/bin/ncmpcpp "$@"'')
    (pkgs.writers.writeDashBin "mpc-zaatar" ''MPD_HOST=${(import <niveum/lib/local-network.nix>).zaatar} exec ${pkgs.mpc_cli}/bin/mpc "$@"'')

    nixpkgs-unstable.spotify
    ncspot
    playerctl

    nix-index
    scripts.nix-index-update

    #krebs
    dic
    cyberlocker-tools
    untilport
    kpaste
    irc-announce
    git-preview
    ircaids

    (python3.withPackages (py: [
      py.black
      # py.python-language-server
      # py.pyls-mypy
      # py.pyls-black
      # py.pyls-isort
      py.flake8
      py.pygments
      py.schema
    ]))
    python3Packages.poetry

    htmlTidy
    nodePackages.csslint
    nodePackages.jsonlint
    nodePackages.prettier
    nodePackages.typescript
    nodePackages.yarn
    nodejs
    nodePackages.javascript-typescript-langserver
    texlive.combined.scheme-full
    latexrun
    (aspellWithDicts (dict: [ dict.de dict.en dict.en-computers ]))
    # haskellPackages.pandoc-citeproc
    scripts.text2pdf
    lowdown
    glow # markdown to term
    libreoffice
    # gnumeric
    dia
    pandoc
    pandoc-doc
    # proselint
    asciidoctor
    wordnet
    tokei # count lines of code
    gnumake
    binutils # for strip, ld, ...
    # nightly.rust
    shellcheck
  ];


  home-manager.users.me.xdg.configFile."pycodestyle".text = ''
    [pycodestyle]
    max-line-length = 110
  '';

}
