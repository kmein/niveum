{ pkgs, lib, ... }:
let
  hc = pkgs.callPackage <stockholm/tv/5pkgs/simple/hc.nix> {};
  worldradio = pkgs.callPackage <niveum/packages/worldradio.nix> {};
  menstruation = pkgs.callPackage <niveum/submodules/menstruation-backend> {};

  nixpkgs-unstable = import <nixpkgs-unstable> { config.allowUnfree = true; };

  recht = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "kmein";
    repo = "recht";
    rev = "0.6.2";
    sha256 = "08gnrnz3lwh8h6fyga56yfy9qryzm89xbshm7wpxfyxf2pmp1qfx";
  }) {};

in {
  imports = [
    ./writing.nix
    ./python.nix
    ./haskell
    {
      environment.systemPackages = let
        # nightly = pkgs.rustChannelOf {
        #   date = "2019-12-27";
        #   channel = "nightly";
        # };
      in with pkgs; [
        htmlTidy
        nodePackages.csslint
        nodePackages.jsonlint
        nodePackages.prettier
        nodePackages.typescript
        nodePackages.yarn
        nodejs
        nodePackages.javascript-typescript-langserver

        tokei # count lines of code
        gnumake
        binutils # for strip, ld, ...
        # nightly.rust
        shellcheck
      ];
    }
  ];

  environment.systemPackages = with pkgs; [
    # INTERNET
    aria2
    firefox
    tdesktop
    w3m
    wget
    whois
    ix
    dnsutils
    # mtr # my traceroute
    # FILE MANAGERS
    ranger
    pcmanfm
    # MEDIA
    ffmpeg
    imagemagick
    exiftool
    scrot
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
    # moreutils # for parallel, sponge, combine
    tree
    fuse_exfat # to mount windows drives
    parallel # for parallel, since moreutils shadows task spooler
    ripgrep # better grep
    rlwrap
    progress # display progress bars for pipes
    up # universal plumber (piping tool)
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
    nixpkgs-unstable.zoom-us # video conferencing
    pdfgrep # search in pdf
    pdftk # pdf toolkit
    poppler_utils # pdf toolkit
    foxitreader # for viewing pdf annotations
    xournalpp # for annotating pdfs
    pdfpc # presenter console for pdf slides
    hc # print files as qr codes
    youtubeDL
    bc # calculator
    pari # gp -- better calculator
    scripts.auc
    scripts.infschmv
    scripts.qrpaste
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
    scripts.genius
    scripts.instaget
    scripts.literature-quote
    scripts.nav # json navigation
    scripts.n
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
    scripts.menu-calc
    nix-prefetch-git
    scripts.nix-git
    nixfmt
    par
    qrencode
    wtf

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
  ];

}
