{
  config,
  pkgs,
  lib,
  inputs,
  niveumPackages,
  unstablePackages,
  ...
}: let
  worldradio = pkgs.callPackage ../packages/worldradio.nix {};

  externalNetwork = import ../lib/external-network.nix;

  zoteroStyle = {
    name,
    sha256,
  }: {
    name = "${name}.csl";
    path = pkgs.fetchurl {
      url = "https://www.zotero.org/styles/${name}";
      inherit sha256;
    };
  };
  cslDirectory = pkgs.linkFarm "citation-styles" [
    (zoteroStyle {
      name = "chicago-author-date-de";
      sha256 = "sha256-ddMYk4A9DJQhx9ldkmF7PhwKuc7wUSr26uHHGAze9Ps=";
    })
    (zoteroStyle {
      name = "din-1505-2";
      sha256 = "sha256-bXZbB850fek8J6wMVFL32ndI7F4wiKKr1qUC71ezreE=";
    })
    (zoteroStyle {
      name = "apa";
      sha256 = "sha256-sUf0Ov5c9aTUoLsYSRbQl3Qs9ELkb5/Tky35kH7pKuE=";
    })
  ];

  astrolog = pkgs.astrolog.overrideAttrs (old:
    old
    // {
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
in {
  home-manager.users.me.home.file = {
    ".csl".source = cslDirectory;
    ".local/share/pandoc/csl".source = cslDirectory; # as of pandoc 2.11, it includes citeproc
  };

  environment.systemPackages = with pkgs; [
    # INTERNET
    aria2
    tdesktop
    whois
    dnsutils
    # FILE MANAGERS
    lf
    pcmanfm
    # MEDIA
    ffmpeg
    imagemagick
    exiftool
    nsxiv
    graphviz
    # SHELL
    bat # better cat
    dos2unix
    genpass # generate passwords
    (pkgs.writers.writeDashBin "genpassphrase" ''${pkgs.genpass}/bin/genpass --passphrase | ${pkgs.gnused}/bin/sed 's/ /-/g;s/\(^\|-\)\([a-z]\)/\1\U\2/g;s/$/-'$(${pkgs.coreutils}/bin/date +%Y)'/' '')
    gcc
    python3Packages.jsonschema # json validation
    pup # html toolkit
    xan # csv toolkit
    magic-wormhole-rs # file transfer
    man-pages
    man-pages-posix
    exfat # to mount windows drives
    # HARDWARE TOOLS
    gnome-disk-utility
    arandr # xrandr for noobs
    libnotify # for notify-send
    xclip # clipboard CLI
    xdragon # drag and drop
    xorg.xkill # kill by clicking
    audacity
    calibre
    electrum
    inkscape
    niveumPackages.gimp
    gthumb
    astrolog
    obsidian
    lemmeknow # identify strings
    aichat # chat with llms
    anki-bin # flashcards
    jbofihe # lojbanic software
    zoom-us # video conferencing
    (pkgs.writers.writeDashBin "im" ''
      weechat_password=$(${pkgs.pass}/bin/pass weechat)
      exec ${unstablePackages.weechat}/bin/weechat -t -r '/mouse enable; /remote add makanek http://${externalNetwork.makanek}:8002 -password='"$weechat_password"'; /remote connect makanek'
    '')
    alejandra # nix formatter
    pdfgrep # search in pdf
    pdftk # pdf toolkit
    mupdf
    poppler_utils # pdf toolkit
    kdePackages.okular # the word is nucular
    xournalpp # for annotating pdfs
    pdfpc # presenter console for pdf slides
    niveumPackages.hc # print files as qr codes
    yt-dlp
    espeak
    rink # unit converter
    niveumPackages.auc
    niveumPackages.noise-waves
    niveumPackages.cheat-sh
    niveumPackages.polyglot
    niveumPackages.qrpaste
    niveumPackages.ttspaste
    niveumPackages.new-mac # get a new mac address
    niveumPackages.scanned
    niveumPackages.default-gateway
    niveumPackages.kirciuoklis
    niveumPackages.image-convert-favicon
    niveumPackages.heuretes
    niveumPackages.ipa # XSAMPA to IPA converter
    niveumPackages.pls
    niveumPackages.mpv-tv
    niveumPackages.mpv-iptv
    jellyfin-media-player
    niveumPackages.devanagari
    niveumPackages.betacode # ancient greek betacode to unicode converter
    niveumPackages.meteo
    niveumPackages.jq-lsp
    niveumPackages.swallow # window swallowing
    niveumPackages.literature-quote
    niveumPackages.booksplit
    niveumPackages.dmenu-randr
    niveumPackages.dmenu-bluetooth
    niveumPackages.manual-sort
    niveumPackages.dns-sledgehammer
    niveumPackages.wttr
    niveumPackages.unicodmenu
    niveumPackages.emailmenu
    niveumPackages.closest
    niveumPackages.trans
    (niveumPackages.mpv-radio.override {
      di-fm-key-file = config.age.secrets.di-fm-key.path;
    })
    (niveumPackages.mpv-radio.override {
      di-fm-key-file = config.age.secrets.di-fm-key.path;
      executableName = "cro-radio";
      mpvCommand = "${niveumPackages.cro}/bin/cro";
    })
    (niveumPackages.mpv-tuner.override {
      di-fm-key-file = config.age.secrets.di-fm-key.path;
    })
    # kmein.slide
    termdown
    niveumPackages.image-convert-tolino
    niveumPackages.rfc
    niveumPackages.tag
    niveumPackages.timer
    niveumPackages.menu-calc
    nix-prefetch-git
    niveumPackages.nix-git
    nixfmt-rfc-style
    par
    qrencode

    # inputs.menstruation-backend.defaultPackage.x86_64-linux
    inputs.agenix.packages.x86_64-linux.default
    inputs.recht.defaultPackage.x86_64-linux

    (pkgs.writers.writeDashBin "worldradio" ''
      shuf ${worldradio} | ${pkgs.findutils}/bin/xargs ${pkgs.mpv}/bin/mpv --no-video
    '')

    (pkgs.writers.writeDashBin "chats" ''
      ${pkgs.openssh}/bin/ssh makanek "cd /var/lib/weechat/logs && grep --ignore-case --color=always --recursive $@" | ${pkgs.less}/bin/less --raw-control-chars
    '')

    (pkgs.writers.writeDashBin "ncmpcpp-zaatar" ''MPD_HOST=${(import ../lib/local-network.nix).zaatar} exec ${pkgs.ncmpcpp}/bin/ncmpcpp "$@"'')
    (pkgs.writers.writeDashBin "mpc-zaatar" ''MPD_HOST=${(import ../lib/local-network.nix).zaatar} exec ${pkgs.mpc_cli}/bin/mpc "$@"'')

    inputs.scripts.packages.x86_64-linux.alarm

    spotify
    ncspot
    playerctl

    nix-index
    niveumPackages.nix-index-update

    #krebs
    niveumPackages.dic
    pkgs.nur.repos.mic92.ircsink

    (haskellPackages.ghcWithHoogle (hs: [
      hs.text
      hs.lens
      hs.bytestring
    ]))

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
    # python3Packages.poetry

    dhall-nix
    dhall-bash
    dhall-json
    dhall

    html-tidy
    nodePackages.csslint
    nodePackages.jsonlint
    deno # better node.js
    texlive.combined.scheme-full
    latexrun
    (aspellWithDicts (dict: [dict.de dict.en dict.en-computers]))
    # haskellPackages.pandoc-citeproc
    niveumPackages.text2pdf
    lowdown
    glow # markdown to term
    libreoffice
    # gnumeric
    dia
    pandoc
    librsvg # pandoc depends on this to include SVG in documents
    # niveumPackages.man-pandoc
    typst
    # proselint
    asciidoctor
    wordnet
    tokei # count lines of code
    gnumake
    binutils # for strip, ld, ...
    # nightly.rust
    shellcheck

    # photography
    gphoto2
    darktable

    (pkgs.writers.writeDashBin "hass-cli" ''
      HASS_SERVER=http://zaatar.r:8123 HASS_TOKEN="$(cat ${config.age.secrets.home-assistant-token.path})"  exec ${pkgs.home-assistant-cli}/bin/hass-cli "$@"
    '')

    # xml
    saxonb_9_1
    libxml2
    zotero
  ];

  age.secrets.home-assistant-token = {
    file = ../secrets/home-assistant-token.age;
    owner = config.users.users.me.name;
    group = config.users.users.me.group;
    mode = "400";
  };

  home-manager.users.me.xdg.configFile."pycodestyle".text = ''
    [pycodestyle]
    max-line-length = 110
  '';
}
