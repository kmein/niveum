{ pkgs, lib, ... }: 
let hc = pkgs.callPackage <stockholm/tv/5pkgs/simple/hc.nix> {};
in {
  imports = [
    ./krebs.nix
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
    httpie
    whois
    ddgr
    ix
    nur.repos.kmein.python3Packages.instaloader
    dnsutils
    # mtr # my traceroute
    # FILE MANAGERS
    ranger
    gnome3.nautilus
    # MEDIA
    ffmpeg
    imagemagick
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
    du-dust # better du
    fd # better find
    file # determine file type
    dos2unix
    trash-cli
    ncdu # ncurses disk usage
    python3Packages.jsonschema # json validation
    jq # json toolkit
    jo # json creation
    xsv # csv toolkit
    xmlstarlet # xml toolkit
    manpages
    posix_man_pages
    # moreutils # for parallel, sponge, combine
    tree
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
    zoom-us # video conferencing
    pdfgrep # search in pdf
    pdftk # pdf toolkit
    evince # for viewing pdf annotations
    xournal # for annotating pdfs
    hc # print files as qr codes
    youtubeDL
    bc # calculator
    scripts.default-gateway
    scripts.showkeys-toggle
    scripts.favicon
    scripts.ipa # XSAMPA to IPA converter
    scripts.betacode # ancient greek betacode to unicode converter
    nur.repos.kmein.mahlzeit
    # nur.repos.kmein.slide
    nur.repos.kmein.vimv
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
    # kmein.slide
    scripts.tolino-screensaver
    nix-prefetch-git
    scripts.nix-git
    nixfmt
    par
    qrencode
    wtf

    spotify
    spotify-tui
    playerctl
  ];

}
