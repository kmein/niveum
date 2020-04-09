{ pkgs, ... }:
{
  imports = [
    ./krebs.nix
    ./writing.nix
    ./python.nix
    ./haskell
    {
      environment.systemPackages =
      let
        nightly = pkgs.rustChannelOf {
          date = "2019-12-27";
          channel = "nightly";
        };
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
        nightly.rust
        shellcheck
      ];
    }
    {
      /*
      environment.systemPackages =
      let
        package = pkgs.fetchFromGitHub {
          owner = "kmein";
          repo = "mahlzeit";
          rev = "954c0fb3f45815999bc65d003794af6a850b069c";
          sha256 = "046yrr40hjmxkjmwzcvmwb39fxx2v2i6hgdxrjfiwilzvhikarrg";
        };
        mahlzeit = pkgs.haskellPackages.callPackage package {};
      in [ mahlzeit ];
      */
    }
  ];

  environment.systemPackages = with pkgs; [
    # INTERNET
    aria2
    firefox
    tor-browser-bundle-bin
    qutebrowser
    tdesktop
    w3m
    wget
    httpie
    whois
    ddgr
    ix
    # thunderbird
    # python3Packages.instaloader
    # mtr # my traceroute
    # FILE MANAGERS
    ranger
    gnome3.nautilus
    # MEDIA
    ffmpeg
    imagemagick
    sxiv
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
    ncdu # ncurses disk usage
    python3Packages.jsonschema # json validation
    jq # json toolkit
    jo # json creation
    xsv # csv toolkit
    xmlstarlet # xml toolkit
    manpages
    posix_man_pages
    moreutils # for parallel, sponge, combine
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
    xorg.xkill # kill by clicking
    audacity
    calibre
    inkscape
    pdfgrep # search in pdf
    pdftk # pdf toolkit
    spotify
    python3Packages.spotify-cli-linux
    youtubeDL
    bc # calculator
    fzf
    pass
    gnupg
    kmein.favicon
    # kmein.bvg
    kmein.daybook
    kmein.depp
    # kmein.dirmir
    kmein.genius
    kmein.instaget
    kmein.literature-quote
    kmein.nav # json navigation
    kmein.n
    kmein.vf
    kmein.vg
    kmein.fkill
    kmein.odyssey
    kmein.wttr
    # kmein.slide
    kmein.tolino-screensaver
    memo
    nix-prefetch-git
    nix-git
    par
    qrencode
    wtf
  ];

}
